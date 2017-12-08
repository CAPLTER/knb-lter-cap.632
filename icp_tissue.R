# README ----

# Here we are creating a set of clean, easily-interpretable ICP tissue data to
# publish; will include the raw data (as unmolested .xlsm/.xls files) as an
# otherEntity instead of storing these complicated raw data in the database. As
# always, we will add the raw xslm file to the zipped file included with this
# dataset.

# In November 2017, Sharon Hall discovered an error in the ICP data. It turned
# out to be an error that Marisa had already corrected (the correction about
# which Sharon was not aware), but investigating that error led me to discover
# another error that was present in much of the data that Marisa had provided.
# Thus, all data were re-processed and re-published in the winter of 2017. The
# workflow below details that effort. In the past, new data were added to the
# existing set of data by simply appending new data (modified to fit) to the
# corpus of already processed data. In this sense, we are essentially using the
# CSV of processed data as a database and just adding to it. Because the
# workflow below reflected re-processing the entire corpus of data, the workflow
# of appending to an existing corpus is not reflected - but that is where you
# will pick up with the next set of ICP data.

# project id ----
projectid <- '632'

# libraries ----
library("tools")
library("EML")
library('RPostgreSQL')
library("tidyverse")
library("readxl")
library("lubridate")

# database connections ----
source('~/Documents/localSettings/pg_prod.R')
source('~/Documents/localSettings/pg_local.R')
  
pg <- pg_prod
pg <- pg_local

# functions and working dir ----
source('~/localRepos/reml-helper-tools/writeAttributesFn.R')
source('~/localRepos/reml-helper-tools/createDataTableFromFileFn.R')
source('~/localRepos/reml-helper-tools/createFactorsDataframe.R')

# generate eml components ----
writeAttributes(tissue_icp) # write data frame attributes to a csv in current dir to edit metadata
tissue_icp_desc <- 'Elemental composition of Larrea tridentata leaf tissue and Pectocarya recurvata (whole plant) tissue collected from control plots at Desert Fertilization study sites. Most analyses are by ICP-MS except for Sulfur (S), which is typically analyzed by ICP-OES with the instrument type noted in the instrument field.'

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
                            description = tissue_icp_desc,
                            dateRangeField = 'sample_date')

# construct eml ----

dataset <- new("dataset",
               dataTable = c(tissue_icp_DT))

# as this is a revision, only the xml for the tables is required
eml <- new("eml",
           dataset = dataset,
           additionalMetadata = as(unitList, "additionalMetadata"))

write_eml(eml, "icp_2017.xml")



# raw data as otherEntity -------------------------------------------------

# zip raw data
icp_raw_data <- createOE(object = '*.zip',
                         description = "This zipped file contains the raw ICP-MS and ICP-OES data (as XSLM and xls files) pertaining to the analyses of Larrea tridentata leaf tissue and Pectocarya recurvata plant tissue samples. The calculated concentrations are presented in an analysis-friendly format in the data entity 'tissue_icp' that is part of this dataset; the raw data file from which the calculated concentrations were derived is referenced in the source_file field of the tissue_icp data entity.")

write_eml(icp_raw_data, "icp_raw_data.xml")


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
    EXTRACT (MONTH FROM st.post_date) = 10 AND
    st.post_date IS NOT NULL AND
    t.code LIKE 'C1'
  GROUP BY plot_id, t.code, s.code, post_date
  ORDER BY post_date, plot_id;") %>% 
  mutate(tissue_type = 'Larrea tridentata') %>%
  rename(date = post_date)

oct_16_stems <- stemsDates 

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
	year = 2015 AND
	t.code LIKE 'C1'
  GROUP BY plot_id, site, trt, date
  ORDER BY date, plot_id;") %>% 
  mutate(tissue_type = 'Pectocarya recurvata')

spring_15_anns <- annualsDates 


# RUN: Larrea: May, Oct 2009; May 2010; Oct 2013 ----

may_oct_09_may_10_oct_13 <- read_excel('./marisa_corrected/Cndep_ICP_MS_02252014 _(Corrected on 11302017).xlsm')
may_oct_09_may_10_oct_13 <- may_oct_09_may_10_oct_13 %>%
  separate(`Orig Conc.`, into = c("site_code", "plot_id", "treatment_code"), sep = "-") %>% 
  select(-`45Sc-CCT`, -`45Sc`, -`72Ge-CCT`, -`72Ge`, -`89Y-H2`, -`89Y`, -`115In-NH3`, -`115In`, -`209Bi-NH3`, -`209Bi`)

# Larrea May 2009

may_09_larrea <- may_oct_09_may_10_oct_13 %>%
  slice(14:28) %>% 
  mutate(plot_id = as.integer(plot_id)) %>% 
  inner_join(may_09_stems, by = c("plot_id")) %>% 
  mutate(season_year = "spring_2009")

# Larrea Oct 2009

oct_09_larrea <- may_oct_09_may_10_oct_13 %>%
  slice(31:45) %>% 
  mutate(plot_id = as.integer(plot_id)) %>% 
  inner_join(oct_09_stems, by = c("plot_id")) %>% 
  mutate(season_year = "fall_2009")

# May 2010 there are two dates for SRR in May 2010, not sure which day is
# correct but we can hazard a reasonable assumption based on the typical site
# grouping
may_10_stems <- may_10_stems %>% 
  filter(!(plot_id == 13 & date == "2010-05-12"))
  
may_10_larrea <- may_oct_09_may_10_oct_13 %>%
  slice(56:70) %>% 
  mutate(plot_id = as.integer(plot_id)) %>% 
  inner_join(may_10_stems, by = c("plot_id")) %>% 
  mutate(season_year = "fall_2010")

# Oct 2013

oct_13_larrea <- may_oct_09_may_10_oct_13 %>%
  slice(73:87) %>% 
  mutate(plot_id = as.integer(plot_id)) %>% 
  inner_join(oct_13_stems, by = c("plot_id")) %>% 
  mutate(season_year = "fall_2013")

# merge all data set from run
larrea_09_13 <- bind_rows(may_09_larrea,
                          oct_09_larrea,
                          may_10_larrea,
                          oct_13_larrea) %>%
  select(-site, -trt) %>% 
  gather(key = isotope_element, value = concentration, -site_code, -plot_id, -treatment_code, -date, -tissue_type, -season_year) %>% 
  mutate(instrument = "ICP-MS") %>%
  mutate(source_file = grep("4 _",
                            list.files("./marisa_corrected/"),
                            value=T)) %>% 
  select(site_code, plot_id, treatment_code, sample_date = date, season_year, tissue_type, instrument, isotope_element, concentration, source_file)


# RUN: Larrea: Oct 2010 ----

oct_10 <- read_excel('./marisa_corrected/CNdep_ICP-MS_Oct2010_(corrected on 11302017).xlsx')
oct_10 <- oct_10  %>%
  separate(`Orig Conc.`, into = c("site_code", "plot_id", "treatment_code"), sep = "-") %>% 
  select(-`45Sc-CCT`, -`45Sc`, -`72Ge-CCT`, -`72Ge`, -`89Y-H2`, -`89Y`, -`115In-NH3`, -`115In`, -`209Bi-NH3`, -`209Bi`)

oct_10_larrea <- oct_10 %>%
  slice(13:27) %>% 
  mutate(plot_id = as.integer(plot_id)) %>% 
  inner_join(oct_10_stems, by = c("plot_id")) %>% 
  mutate(treatment_code = "C1") %>%
  select(-site, -trt) %>% 
  gather(key = isotope_element, value = concentration, -site_code, -plot_id, -treatment_code, -date, -tissue_type) %>% 
  mutate(instrument = "ICP-MS") %>%
  mutate(season_year = "fall_2010") %>% 
  mutate(source_file = grep("10_",
                            list.files("./marisa_corrected/"),
                            value=T)) %>% 
  select(site_code, plot_id, treatment_code, sample_date = date, season_year, tissue_type, instrument, isotope_element, concentration, source_file)


# RUN: Larrea: Oct 2015 (includes Sulfur) ----

oct_15 <- read_excel('./from_repo/raw_icp/CNDep_Larrea_Metals_Oct2015.xlsm')
oct_15 <- oct_15  %>%
  separate(`Orig Conc.`, into = c("site_code", "treatment_code"), sep = "-") %>% 
  select(-`72Ge-CCT`, -`72Ge`, -`115In-NH3`, -`115In`, -`209Bi-NH3`, -`209Bi`) %>% 
  select(-contains("X__"))

oct_15_larrea <- oct_15 %>%
  slice(14:28) %>% 
  # mutate(plot_id = as.integer(plot_id)) %>% 
  inner_join(oct_15_stems, by = c("site_code" = "site")) %>% 
  mutate(treatment_code = "C1") %>%
  select(-trt) %>%
  gather(key = isotope_element, value = concentration, -site_code, -plot_id, -treatment_code, -date, -tissue_type) %>% 
  mutate(instrument = "ICP-MS") %>%
  mutate(instrument = replace(instrument, isotope_element == "S_182.0", "ICP-OES")) %>%
  mutate(instrument = replace(isotope_element, isotope_element == "S_182.0", "S")) %>%
  mutate(season_year = "fall_2015") %>% 
  mutate(source_file = grep("Oct2015",
                            list.files("./from_repo/raw_icp/"),
                            value=T)) %>% 
  select(site_code, plot_id, treatment_code, sample_date = date, season_year, tissue_type, instrument, isotope_element, concentration, source_file)


# RUN: Larrea: May 2016, Oct 2016; Pecto: spring 2015, 2016 ----

may_oct_15_16 <- read_excel('./from_repo/raw_icp/Cndep_Larrea_Pecto_10202016.XLSM')
may_oct_15_16 <- may_oct_15_16 %>%
  separate(`Orig Conc.`, into = c("site_code", "plot_id", "treatment_code"), sep = "-") %>% 
  select(-`72Ge-CCT`, -`72Ge`, -`115In-NH3`, -`115In`, -`209Bi-NH3`, -`209Bi`) %>% 
  select(-contains("X__"))

# Larrea: May 2016

may_16_larrea <- may_oct_15_16 %>%
  slice(14:28) %>% 
  mutate(plot_id = as.integer(plot_id)) %>% 
  inner_join(may_16_stems, by = c("plot_id")) %>% 
  mutate(season_year = "spring_2016")

# Larrea: Oct 2016

oct_16_larrea <- may_oct_15_16 %>%
  slice(54:68) %>% 
  mutate(plot_id = as.integer(plot_id)) %>% 
  inner_join(oct_16_stems, by = c("plot_id")) %>% 
  mutate(season_year = "fall_2016")

# Pecto: spring 2016
spring_16_pecto <- may_oct_15_16 %>%
  slice(31:44) %>% 
  mutate(plot_id = as.integer(plot_id)) %>% 
  inner_join(spring_16_anns, by = c("plot_id")) %>% 
  mutate(season_year = "spring_2016")

# Pecto: spring 2015

spring_15_pecto <- may_oct_15_16 %>%
  slice(47:51) %>% 
  mutate(plot_id = as.integer(plot_id)) %>% 
  inner_join(spring_15_anns, by = c("plot_id")) %>% 
  mutate(season_year = "spring_2015")

# merge all data set from run
larrea_pecto_15_16 <- bind_rows(may_16_larrea,
                                oct_16_larrea,
                                spring_16_pecto,
                                spring_15_pecto) %>%
  select(-site, -trt) %>%
  gather(key = isotope_element, value = concentration, -site_code, -plot_id, -treatment_code, -date, -tissue_type, -season_year) %>%
  mutate(instrument = "ICP-MS") %>%
  mutate(source_file = grep("Pecto",
                            list.files("./from_repo/raw_icp/"),
                            value = T)) %>%
  select(site_code, plot_id, treatment_code, sample_date = date, season_year, tissue_type, instrument, isotope_element, concentration, source_file)


# RUN: Sulfur: Larrea: spring, fall 16; pecto: spring 15, fall 16 ----

sulfur_only <- read_excel('./from_repo/raw_icp/Larrea_S_11032016.xls',
                          skip = 11,
                          col_names = FALSE)

colnames(sulfur_only)[1] <- "run_details"
colnames(sulfur_only)[2] <- "sample_id"
colnames(sulfur_only)[5] <- "concentration"

sulfur_only <- sulfur_only %>% 
  filter(!grepl("QC", sample_id)) %>% 
  separate(sample_id, into = c("site_code", "treatment_code"), sep = "-") %>% 
  select(-contains("X__"))
  
# sulfur larrea spring 2016

larrea_spring_2016_sulfur <- sulfur_only %>%
  slice(2:16) %>% 
  inner_join(may_16_stems, by = c("site_code" = "site")) %>% 
  mutate(season_year = "spring_2016")

# sulfur larrea fall 2016

larrea_fall_2016_sulfur <- sulfur_only %>%
  slice(39:53) %>% 
  inner_join(oct_16_stems, by = c("site_code" = "site")) %>% 
  mutate(season_year = "fall_2016")

# sulfur pecto spring 2016

pecto_fall_2016_sulfur <- sulfur_only %>%
  slice(18:31) %>% 
  inner_join(spring_16_anns, by = c("site_code" = "site")) %>% 
  mutate(season_year = "spring_2016")

# sulfur pecto spring 2015

pecto_fall_2015_sulfur <- sulfur_only %>%
  slice(33:37) %>% 
  inner_join(spring_15_anns, by = c("site_code" = "site")) %>% 
  mutate(season_year = "spring_2015")

# merge all data set from run
sulfur_15_16 <- bind_rows(larrea_spring_2016_sulfur,
                          larrea_fall_2016_sulfur,
                          pecto_fall_2016_sulfur,
                          pecto_fall_2015_sulfur) %>%
  select(-trt, -run_details) %>% 
  gather(key = isotope_element, value = concentration, -site_code, -plot_id, -treatment_code, -date, -tissue_type, -season_year) %>%
  mutate(instrument = "ICP-OES") %>%
  mutate(isotope_element = "S") %>% 
  mutate(source_file = grep("_S_",
                            list.files("./from_repo/raw_icp/"),
                            value = T)) %>%
  select(site_code, plot_id, treatment_code, sample_date = date, season_year, tissue_type, instrument, isotope_element, concentration, source_file)


# combine all data (from outset through fall 2016) ----

tissue_icp <- bind_rows(larrea_09_13,
                        oct_10_larrea,
                        oct_15_larrea,
                        larrea_pecto_15_16,
                        sulfur_15_16) %>%
  mutate(concentration = as.numeric(concentration)) %>% 
  mutate(concentration = round(concentration, digits = 3)) %>% 
  arrange(tissue_type, season_year, isotope_element, sample_date, site_code) %>% 
  mutate(site_code = as.factor(site_code)) %>% 
  mutate(treatment_code = as.factor(treatment_code)) %>% 
  mutate(tissue_type = as.factor(tissue_type)) %>% 
  mutate(instrument = as.factor(instrument)) %>%
  mutate(plot_id = as.character(plot_id)) %>%
  as.data.frame()