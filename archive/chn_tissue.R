# README ------------------------------------------------------------------

# fall 2023

# CHN data upload was moved to the Shiny application:
# https://github.com/CAPLTER/databases-change-log/commit/f381eee078121f671f77b8759fd5222b947b283d

# 2019-07-01

# Seems that CHN output is not entirely consistent. For example, in previous
# runs, Comments was Comment, there was a Messages column, and Weight (mg) was
# just Weight. These differences all require(d) minor adjustments to the insert
# statement of the insert_raw_chn function. Adjust as needed, and be sure sure
# to do trial runs on dev before attempting to upload to prod.


# 2018-06-05

# The data publishing component of this workflow has been moved to
# knb-lter-cap.632.Rmd. This workflow now is focused only on new data
# processing.

# Here we are creating a set of clean, easily-interpretable CHN tissue data to
# publish. I had started out attacking this in a manner similar to the ICP data
# where the raw data in Excel format would be saved and we would keep appending
# to a cleaned version of those and saving the files for reference. However, a
# difference here is that the CHN data are amenable to loading into a database
# for reference. That is the approach taken. Also, because the data format is
# relatively consistent (unlike, e.g., the ICP data), data import, munging, and
# db interaction were functionalized.

# Similar to the ICP data, the record of CHN data processing will be stored (see
# the data processing (cumulative record) section) hence this workflow will be
# outside of the main knb-lter-cap.632 workflow.


# project id --------------------------------------------------------------
projectid <- '632'


# libraries ---------------------------------------------------------------
library(EML)
library(RPostgreSQL)
library(tidyverse)
library(readxl)
library(lubridate)
library(aws.s3)

# database connections ----------------------------------------------------
source('~/Documents/localSettings/pg_prod.R')
source('~/Documents/localSettings/pg_local.R')
  
pg <- pg_prod
pg <- pg_local

# chn processing functions -------------------------------------------------

# chn_to_raw: associate sample details (site_code, sample_date, etc.)
chn_to_raw <- function(dataFile, tissueType, surveyYear, season) {
  
  # check for required environmental parameters and arguments

    # do not proceed if the datafile has not been identified
    if (!exists('dataFile')) { stop("missing data file") }

    # do not proceed if the tissue type has not been identified
    if (!exists('tissueType')) { stop("specify tissue type") }

    # do not proceed if the survey year has not been identified
    if (!exists('surveyYear')) { stop("specify survey year") }
  
  if(grepl("lar", tissueType, ignore.case = T)) {
    
    if(grepl("spr", season, ignore.case = T)) {
      
      stemsDatesBaseQuery <- "
        SELECT
         sh.plot_id,
         s.code AS site_code,
         t.code AS treatment_code,
         st.post_date AS date
       FROM urbancndep.stems st
       JOIN urbancndep.shrubs sh ON st.shrub_id = sh.id
       JOIN urbancndep.plots pl ON sh.plot_id = pl.id
       JOIN urbancndep.treatments t ON pl.treatment_id = t.id
       JOIN urbancndep.sites s ON pl.site_id = s.id
       WHERE
         EXTRACT (YEAR FROM st.post_date) = ?survey_year AND
         EXTRACT (MONTH FROM st.post_date) IN (4, 5, 6) AND
         st.post_date IS NOT NULL
       GROUP BY plot_id, t.code, s.code, post_date
       ORDER BY post_date, plot_id;"
      
      stemsDatesQuery <- sqlInterpolate(ANSI(),
                                        stemsDatesBaseQuery,
                                        survey_year = surveyYear)
      
      sampling_event <- dbGetQuery(pg, stemsDatesQuery) %>% 
        mutate(tissue_type = 'Larrea tridentata')
      
    } else {
      
      stemsDatesBaseQuery <- "
        SELECT
         sh.plot_id,
         s.code AS site_code,
         t.code AS treatment_code,
         st.post_date AS date
       FROM urbancndep.stems st
       JOIN urbancndep.shrubs sh ON st.shrub_id = sh.id
       JOIN urbancndep.plots pl ON sh.plot_id = pl.id
       JOIN urbancndep.treatments t ON pl.treatment_id = t.id
       JOIN urbancndep.sites s ON pl.site_id = s.id
       WHERE
         EXTRACT (YEAR FROM st.post_date) = ?survey_year AND
         EXTRACT (MONTH FROM st.post_date) IN (9, 10, 11) AND
         st.post_date IS NOT NULL
       GROUP BY plot_id, t.code, s.code, post_date
       ORDER BY post_date, plot_id;"
      
      stemsDatesQuery <- sqlInterpolate(ANSI(),
                                        stemsDatesBaseQuery,
                                        survey_year = surveyYear)
      
      sampling_event <- dbGetQuery(pg, stemsDatesQuery) %>% 
        mutate(tissue_type = 'Larrea tridentata')
      
    }
    
  } # close if Larrea
  
  
  if(grepl("pec", tissueType, ignore.case = T)) {
    
    annualsDatesBaseQuery <- "
      SELECT
        ab.plot_id,
        s.code AS site_code,
        t.code AS treatment_code,
        ab.date
      FROM urbancndep.annuals_biomass ab
      JOIN urbancndep.plots pl ON ab.plot_id = pl.id
      JOIN urbancndep.treatments t ON pl.treatment_id = t.id
      JOIN urbancndep.sites s ON pl.site_id = s.id
      WHERE 
        year = ?survey_year 
      GROUP BY plot_id, site_code, treatment_code, date
      ORDER BY date, plot_id;"
    
    annualsDatesQuery <- sqlInterpolate(ANSI(),
                                        annualsDatesBaseQuery,
                                        survey_year = surveyYear)
    
    sampling_event <- dbGetQuery(pg, annualsDatesQuery) %>% 
      mutate(tissue_type = 'Pectocarya recurvata')
    
  } # close if Pectocarya    
  
  
  # process data
  data_to_process <- read_excel(dataFile, skip = 1)
  
  processed_data <- data_to_process %>%
    mutate(plot_id = as.integer(str_extract(`Sample ID`, "^[0-9]+$|(?<=PLOT\\s)[0-9.]+|(?<=Plot\\s)[0-9.]+|(?<=plot\\s)[0-9.]+"))) %>% 
    left_join(sampling_event, by = c("plot_id" = "plot_id")) %>% 
    mutate(
      season_year = case_when(
        month(date) <= 6 ~ paste0("spring", "_", year(date)),
        month(date) > 6 ~ paste0("fall", "_", year(date))
      )
    ) %>% 
    mutate(source_file = dataFile)
  
  # output count of plots/site for error checking
  print(
    processed_data %>% 
      filter(!is.na(site_code)) %>% 
      group_by(site_code) %>% 
      summarise(count = n()) %>% 
      arrange(count)
  )
  
  return(processed_data)  
  
}

# sensu:
# chn_raw <- chn_to_raw(dataFile = 'CHN_Larrea_Fall 2015.xls',
#                       tissueType = "larrea",
#                       surveyYear = 2015,
#                       season = "fall")

# after previewing chn_raw, insert those data into the chn table
# a subset (up to 50) samples are returned for confirmation
insert_raw_chn <- function(processed_chn) {

  # write chn_raw data to temporary table
  if (dbExistsTable(pg, c('urbancndep', 'chn_raw'))) dbRemoveTable(pg, c('urbancndep', 'chn_raw')) 
  dbWriteTable(pg, c('urbancndep', 'chn_raw'), value = processed_chn, row.names = F) 
  
  # change run date to type date
  dbExecute(pg,'
            ALTER TABLE urbancndep.chn_raw
              ALTER COLUMN "Created on" TYPE DATE;')

  # insert statement for raw chn data
  raw_chn_insert <-
  'INSERT INTO urbancndep.plant_tissue_chn
  (
    "Sample ID",
    "Run #",
    "Weight",
    "Created on",
    "Mode",
    "Comment", 
    "Carbon %",
    "Hydrogen %",
    "Nitrogen %",
    "ZR",
    "CR",
    "HR",
    "NR", 
    "Carbon",
    "Hydrogen",
    "Nitrogen",
    "Seconds",
    --"Messages",
    plot_id, 
    site_code,
    treatment_code,
    sample_date,
    tissue_type,
    season_year,
    source_file
  )
  (
    SELECT
      "Sample ID",
      "Run #",
      "Weight (mg)",
      "Created on",
      "Mode",
      "Comments", 
      "Carbon %",
      "Hydrogen %",
      "Nitrogen %",
      "ZR",
      "CR",
      "HR",
      "NR", 
      "Carbon",
      "Hydrogen",
      "Nitrogen",
      "Seconds",
      --"Messages",
      plot_id, 
      site_code,
      treatment_code,
      date,
      tissue_type,
      season_year,
      source_file
    FROM urbancndep.chn_raw
  );'

  # execute insect query
  dbExecute(pg, raw_chn_insert)

  # clean up
  dbRemoveTable(pg, c('urbancndep', 'chn_raw'))

  # show some inserted data for confirmation
  verify_chn <- 'SELECT * FROM urbancndep.plant_tissue_chn ORDER BY plant_tissue_chn.id DESC LIMIT 50;'
  dbGetQuery(pg, verify_chn)

}

# sensu:
# insert_raw_chn(chn_raw)


# data processing (cumulative record) ------------------------------------------

chn_raw <- chn_to_raw(dataFile = 'CHN_Larrea_Spring 2018.xls',
                      tissueType = "larrea",
                      surveyYear = 2018,
                      season = "spr")
insert_raw_chn(chn_raw)


chn_raw <- chn_to_raw(dataFile = 'CHN_Larrea_Fall 2015.xls',
                      tissueType = "larrea",
                      surveyYear = 2015,
                      season = "fall")
insert_raw_chn(chn_raw)


chn_raw <- chn_to_raw(dataFile = 'CHN_Larrea_Fall 2016.xls',
                      tissueType = "larrea",
                      surveyYear = 2016,
                      season = "fall")
insert_raw_chn(chn_raw)


chn_raw <- chn_to_raw(dataFile = 'CHN_Larrea_Fall 2017.xls',
                      tissueType = "larrea",
                      surveyYear = 2017,
                      season = "fall")
insert_raw_chn(chn_raw)


chn_raw <- chn_to_raw(dataFile = 'CHN_ Larrea_Spring 2015.xls',
                      tissueType = "larrea",
                      surveyYear = 2015,
                      season = "spring")
insert_raw_chn(chn_raw)


chn_raw <- chn_to_raw(dataFile = 'CHN_Larrea_Spring 2016.xls',
                      tissueType = "larrea",
                      surveyYear = 2016,
                      season = "spring")
insert_raw_chn(chn_raw)


chn_raw <- chn_to_raw(dataFile = 'CHN_Larrea_Spring 2017.xls',
                      tissueType = "larrea",
                      surveyYear = 2017,
                      season = "spring")
insert_raw_chn(chn_raw)


chn_raw <- chn_to_raw(dataFile = 'CHN_Pectocarya_Spring 2016.xls',
                      tissueType = "pecto",
                      surveyYear = 2016,
                      season = "spring")
insert_raw_chn(chn_raw)


chn_raw <- chn_to_raw(dataFile = 'CHN_Pectocarya_Spring 2017.xls',
                      tissueType = "pecto",
                      surveyYear = 2017,
                      season = "spring")
# seems there was a mis-coding for this data set; Marisa had entered plot_id =
# 53 but that is a C2 plot, however, there was not a P plot (id == 55); sample
# changed accordinly
chn_raw <- chn_raw %>% 
  mutate(
    plot_id = replace(plot_id, `Sample ID` == 53, 55),
    site_code = replace(site_code, `Sample ID` == 53, "SNE"),
    treatment_code = replace(treatment_code, `Sample ID` == 53, "P"),
    Comment = replace(Comment, `Sample ID` == 53, "possible data entry error; recoded to plot id = 55"),
    date = replace(date, `Sample ID` == 53, "2017-03-14"),
    tissue_type = replace(tissue_type, `Sample ID` == 53, "Pectocarya recurvata"),
    season_year = replace(season_year, `Sample ID` == 53, "spring_2017")
  )
insert_raw_chn(chn_raw)


chn_raw <- chn_to_raw(dataFile = 'CHN_Larrea_Pecto_retests.xls',
                      tissueType = "pec",
                      surveyYear = 2017,
                      season = "spring")
# a file with retests has a mix of dates and tissue types requiring additional
# processing to facilitate correct sample identification
chn_raw <- chn_raw %>% 
  mutate(Comment = replace(Comment, `Run #` %in% c(64:76), "Pecto Spring 2017 retests")) %>% 
  mutate(Comment = replace(Comment, `Run #` %in% c(58:63), "Larrea Spring 2016 retests")) %>% 
  mutate(Comment = replace(Comment, `Run #` %in% c(56:57), "Larrea Fall 2015 retests")) %>% 
  mutate(tissue_type = replace(tissue_type, `Run #` %in% c(58:63), "Larrea tridentata")) %>% 
  mutate(tissue_type = replace(tissue_type, `Run #` %in% c(56:57), "Larrea tridentata")) %>% 
  mutate(season_year = replace(season_year, `Run #` %in% c(58:63), "spring_2016")) %>% 
  mutate(season_year = replace(season_year, `Run #` %in% c(56:57), "fall_2015")) %>% 
  mutate(date = replace(date, `Run #` %in% c(60:63), "2016-05-19")) %>% 
  mutate(date = replace(date, `Run #` %in% c(58:59), "2016-05-24")) %>% 
  mutate(date = replace(date, `Run #` %in% c(56:57), "2015-10-07"))
insert_raw_chn(chn_raw)



# ARCHIVE: create urbancndep.plant_tissue_chn table ----

# ran in PG, included here for documentation only

CREATE TABLE urbancndep.plant_tissue_chn
(
  id serial, 
  "Sample ID" text,
  "Run #" double precision,
  "Weight" double precision,
  "Created on" date,
  "Mode" text,
  "Comment" text,
  "Carbon %" double precision,
  "Hydrogen %" double precision,
  "Nitrogen %" double precision,
  "ZR" double precision,
  "CR" double precision,
  "HR" double precision,
  "NR" double precision,
  "Carbon" double precision,
  "Hydrogen" double precision,
  "Nitrogen" double precision,
  "Seconds" double precision,
  "Messages" boolean,
  plot_id integer,
  site_code text,
  treatment_code text,
  sample_date date,
  tissue_type text,
  season_year text,
  source_file text
)
WITH (
  OIDS=FALSE
);

COMMENT ON TABLE urbancndep.plant_tissue_chn IS 'this table houses the raw CHN data from plant tissue analyses';

ALTER TABLE urbancndep.plant_tissue_chn
  OWNER TO urbancndep;

# ARCHIVE: db queries for sample dates (relic code from manual processing) -----

# get dates from stems for larrea tissue
stemsDates <- dbGetQuery(pg,
     "SELECT
       sh.plot_id,
       s.code AS site,
       t.code AS trt,
       st.post_date
     FROM urbancndep.stems st
     JOIN urbancndep.shrubs sh ON st.shrub_id = sh.id
     JOIN urbancndep.plots pl ON sh.plot_id = pl.id
     JOIN urbancndep.treatments t ON pl.treatment_id = t.id
     JOIN urbancndep.sites s ON pl.site_id = s.id
     WHERE
       EXTRACT (YEAR FROM st.post_date) = 2015 AND
       EXTRACT (MONTH FROM st.post_date) = 5 AND
       st.post_date IS NOT NULL
     GROUP BY plot_id, t.code, s.code, post_date
     ORDER BY post_date, plot_id;") %>% 
  mutate(tissue_type = 'Larrea tridentata') %>%
  rename(date = post_date)

oct_15_stems <- stemsDates 

# get dates from annuals biomass for pectocarya tissue
annualsDates <- dbGetQuery(pg,
  "SELECT
    ab.plot_id,
    s.code AS site,
    t.code AS trt,
    ab.date
  FROM urbancndep.annuals_biomass ab
  JOIN urbancndep.plots pl ON ab.plot_id = pl.id
  JOIN urbancndep.treatments t ON pl.treatment_id = t.id
  JOIN urbancndep.sites s ON pl.site_id = s.id
  WHERE 
    year = 2015
  GROUP BY plot_id, site, trt, date
  ORDER BY date, plot_id;") %>% 
  mutate(tissue_type = 'Pectocarya recurvata')

spring_15_anns <- annualsDates 

