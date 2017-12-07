
# README ------------------------------------------------------------------


# project it --------------------------------------------------------------
projectid <- '632'


# libraries ---------------------------------------------------------------
library("EML")
library('RPostgreSQL')
library("tidyverse")
library("readxl")


# database connections ----------------------------------------------------
source('~/Documents/localSettings/pg_prod.R')
source('~/Documents/localSettings/pg_local.R')
  
pg <- pg_prod
pg <- pg_local


# functions and working dir -----------------------------------------------
source('~/localRepos/reml-helper-tools/writeAttributesFn.R')
source('~/localRepos/reml-helper-tools/createDataTableFromFileFn.R')
source('~/localRepos/reml-helper-tools/createFactorsDataframe.R')


# generate eml components -------------------------------------------------
writeAttributes(tissue_chn) # write data frame attributes to a csv in current dir to edit metadata
tissue_chn_desc <- 'Elemental composition of Larrea tridentata leaf tissue and Pectocarya recurvata (whole plant) tissue collected from control plots at Desert Fertilization study sites. Most analyses are by ICP-MS except for Sulfur (S), which is typically analyzed by ICP-OES with the instrument type noted in the instrument field.'

site_code <- c(DBG = "core region, Desert Botanical Garden",
               MVP = "core region, North Mountain",
               PWP = "core region, Piestewa Peak",
               SME = "core region, South Mountain Park East",
               SMW = "core region, South Mountain Park West",
               LDP = "east region, Lost Dutchman State Park",
               MCN = "east region, McDowell Mountain Regional north",
               MCS = "east region, McDowell Mountain Regional south",
               SRR = "east region, Salt River Recreation Area (Tonto NF)",
               UMP = "east region, Usery Mountain Regional Park",
               EME = "west region, Estrella Mountain Regional Park East",
               EMW = "west region, Estrella Mountain Regional Park West",
               SNE = "west region, Sonoran Desert National Monument East",
               SNW = "west region, Sonoran Desert National Monument West",
               WTM = "west region, White Tanks Mountain Regional Park")
treatment_code = c(C1 = "control plot 1")
tissue_type <- c(`Larrea tridentata` = "Larrea tridentata leaf tissue",
                 `Pectocarya recurvata` = "Pectocarya recurvata whole plant tissue")
instrument <- c(`ICP-MS` = "Thermo Scientific X Series 2 quadrapole ICP-MS and Cetac ASX-520",
                `ICP-OES` = 'Thermo Scientific iCAP 6300 optical emission spectrometer and Cetac ASX-520 autosampler')

tissue_icp_factors <- factorsToFrame(tissue_icp)

custom_units <- data.frame(id = "milligramPerKilogram",
                           unitType = "massPerMass",
                           parentSI = "gramsPerGram",
                           multiplierToSI = 0.000001,
                           description = "millgram of isotope per kilogram of plant tissue")
unitList <- set_unitList(custom_units)

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
tissue_icp_DT <- createDTFF(dfname = tissue_icp,
                            factors = tissue_icp_factors,
                            description = tissue_icp_desc)

# construct eml -----------------------------------------------------------
dataset <- new("dataset",
               dataTable = c(tissue_icp_DT))

# as this is a revision, only the xml for the tables is required
eml <- new("eml",
           dataset = dataset,
           additionalMetadata = as(unitList, "additionalMetadata"))

write_eml(eml, "icp_2017.xml")


# data processing ---------------------------------------------------------

# get sample collection dates from stems and annuals collections ----
# get dates from stems for larrea tissue ----
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
       EXTRACT (YEAR FROM st.post_date) = 2016 AND
       EXTRACT (MONTH FROM st.post_date) = 5 AND
       st.post_date IS NOT NULL
     GROUP BY plot_id, t.code, s.code, post_date
     ORDER BY post_date, plot_id;") %>% 
  mutate(tissue_type = 'Larrea tridentata') %>%
  rename(date = post_date)

oct_15_stems <- stemsDates 

# get dates from annuals biomass for pectocarya tissue ----
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



# chn processing functions -------------------------------------------------

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
      month <- 5 
    } else {
      month <- 10 
    }
    
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
       EXTRACT (MONTH FROM st.post_date) = ?survey_month AND
       st.post_date IS NOT NULL
     GROUP BY plot_id, t.code, s.code, post_date
     ORDER BY post_date, plot_id;"
    
    stemsDatesQuery <- sqlInterpolate(ANSI(),
                                      stemsDatesBaseQuery,
                                      survey_year = surveyYear,
                                      survey_month = month)
    
    sampling_event <- dbGetQuery(pg, stemsDatesQuery) %>% 
      mutate(tissue_type = 'Larrea tridentata')
    
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
  
  return(processed_data)  
  
}

# sensu
chn_raw <- chn_to_raw(dataFile = 'CHN_Larrea_Fall 2015.xls',
                      tissueType = "larrea",
                      surveyYear = 2015,
                      season = "fall")

chn_raw <- chn_to_raw(dataFile = 'CHN_Larrea_Fall 2016.xls',
                      tissueType = "larrea",
                      surveyYear = 2016,
                      season = "fall")

chn_raw <- chn_to_raw(dataFile = 'CHN_Larrea_Fall 2017.xls',
                      tissueType = "larrea",
                      surveyYear = 2017,
                      season = "fall")

chn_raw <- chn_to_raw(dataFile = 'CHN_ Larrea_Spring 2015.xls',
                      tissueType = "larrea",
                      surveyYear = 2015,
                      season = "spring")

chn_raw <- chn_to_raw(dataFile = 'CHN_Larrea_Spring 2016.xls',
                      tissueType = "larrea",
                      surveyYear = 2016,
                      season = "spring")

chn_raw <- chn_to_raw(dataFile = 'CHN_Larrea_Spring 2017.xls',
                      tissueType = "larrea",
                      surveyYear = 2017,
                      season = "spring")

chn_raw <- chn_to_raw(dataFile = 'CHN_Pectocarya_Spring 2016.xls',
                      tissueType = "pecto",
                      surveyYear = 2016,
                      season = "spring")

chn_raw <- chn_to_raw(dataFile = 'CHN_Pectocarya_Spring 2017.xls',
                      tissueType = "pecto",
                      surveyYear = 2017,
                      season = "spring")

chn_raw <- chn_to_raw(dataFile = 'CHN_Larrea_Pecto_retests.xls',
                      tissueType = "pec",
                      surveyYear = 2017,
                      season = "spring")

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
    "Messages",
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
      "Messages",
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

# sensu: insert_raw_chn(chn_raw)


# query db and process results --------------------------------------------

chnDataQuery <- '
  SELECT
    site_code,
    plot_id, 
    treatment_code,
    sample_date,
    season_year, 
    tissue_type,
    "Weight",
    "Comment", 
    "Carbon %",
    "Hydrogen %",
    "Nitrogen %"
  FROM urbancndep.plant_tissue_chn
  WHERE plot_id IS NOT NULL;'

chn_data <- dbGetQuery(pg, chnDataQuery)


# scratch

chnfile <- 'CHN_Larrea_Fall 2015.xls'
fall_2015 <- read_excel(chnfile, skip = 1)
fall_2015_larrea <- fall_2015 %>%
  filter(grepl("PLOT|^[0-9]+$", `Sample ID`, ignore.case = T)) %>% 
  filter(!grepl("retest", Comment, ignore.case = T)) %>% 
  rename(sample_id = `Sample ID`) %>%
  mutate(sample_id = str_extract(sample_id, "[0-9]+")) %>%
  mutate(sample_id = as.integer(sample_id)) %>%
  inner_join(oct_15_stems, by = c(sample_id = "plot_id")) %>% 
  rename(treatment_code = trt) %>% 
  select(site_code = site, plot_id = sample_id, treatment_code, sample_date = date, tissue_type, Weight, `Carbon %`, `Hydrogen %`, `Nitrogen %`) %>%
  gather(key = analyte, value = percent_composition, -site_code, -plot_id, -treatment_code, -sample_date, -tissue_type, -Weight) %>%
  mutate(season_year = "fall_2015") %>% 
  mutate(source_file = chnfile)
 