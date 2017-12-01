projectid <- '632'

# here we are creating a set of clean, easily-interpretable data to publish; 
# will include the raw data (as unmolested .xlsm/.xls files) as an otherEntity instead
# of storing these complicated raw data in the database. As always, we will add
# the raw xslm file to the zipped file included with this dataset.

# This iteration is for Larrea tridentata leaf tissue data for the spring and
# fall of 2016, and Pectocarya recurvata plant tissue data for the spring of
# 2015 and 2016 for sulfur as analyzed by ICP-OES.

# Discovered a problem with the previous iteration in that I had converted the
# concentration values to integers, so re worked the IPC-MS data (took them out
# of the existing data file, and reloaded them) before adding these data.

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
source('~/Dropbox (ASU)/localRepos/reml-helper-tools/writeAttributesFn.R')
source('~/Dropbox (ASU)/localRepos/reml-helper-tools/createDataTableFromFileFn.R')
source('~/Dropbox (ASU)/localRepos/reml-helper-tools/createFactorsDataframe.R')

# acquire and process data ----

newicpdata <- read_excel('~/Dropbox (ASU)/CNDep Larrea_Pectocarya/Larrea_S_11032016.xls', skip = 4)
colnames(newicpdata)[1] <- "run datetime"
colnames(newicpdata)[2] <- "sampleid"
colnames(newicpdata)[3] <- "operator"
colnames(newicpdata)[4] <- "run file details"

newicpdata$plant_date <- NA
newicpdata[8:23,]$plant_date <- 'Larrea Spring 2016'
newicpdata[25:39,]$plant_date <- 'Pectocarya Spring 2016'
newicpdata[41:46,]$plant_date <- 'Pectocarya Spring 2015'
newicpdata[48:63,]$plant_date <- 'Larrea Fall 2016'

newicpdata <- newicpdata[-c(1:7),]
newicpdata <- newicpdata[-c(57:60),]

newicpdata <- newicpdata %>% 
  filter(!grepl("10ppm", sampleid)) %>% 
  filter(!is.na(plant_date)) %>% 
  separate(
    col = sampleid,
    into = c("site_code", "treatment_code"),
    sep = "-",
    remove = FALSE) %>%
  mutate(year = ifelse(grepl("2016", plant_date), 2016, 2015)) %>%
  mutate(month = ifelse(grepl("spring", plant_date, ignore.case = T), 5, 10)) %>%
  mutate(month = ifelse(grepl("pectocarya", plant_date, ignore.case = T), 3, month)) %>%
  mutate(tissue_type = ifelse(grepl("larrea", plant_date, ignore.case = T), "Larrea", "Pectocarya"))

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
    EXTRACT (YEAR FROM st.pre_date) > 2014 AND
    st.post_date IS NOT NULL AND
    t.code LIKE 'C1'
  GROUP BY plot_id, t.code, s.code, post_date
  ORDER BY post_date, plot_id;")

stemsDates <- stemsDates %>%
  mutate(tissue_type = 'Larrea') %>%
  rename(date = post_date)

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
	year > 2014 AND
	t.code LIKE 'C1'
  GROUP BY plot_id, site, trt, date
  ORDER BY date, plot_id;")

annualsDates <- annualsDates %>%
  mutate(tissue_type = 'Pectocarya')

# put pecto and larrea sample dates together
sampleDates <- bind_rows(stemsDates, annualsDates) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date))

# join tissue data and dates to get exact date
newicpdata <- left_join(newicpdata, sampleDates, by = c("site_code" = "site", "treatment_code" = "trt", "tissue_type", "year", "month"))

# munge tissue data and stack it (if applicable)
newicpdata <- newicpdata  %>%
  mutate(tissue_type = ifelse(grepl("Larrea", tissue_type, ignore.case = T), "Larrea tridentata", "Pectocarya recurvata")) %>%
  mutate(instrument = "ICP-OES") %>%
  mutate(isotope_element = 'S') %>% 
  select(site_code, plot_id, treatment_code, sample_date = date, tissue_type, instrument, isotope_element, concentration = S_1820)

# get the existing tissue data so that we can add to it
existTissue <- read_csv('~/Desktop/632_tissue_icp_c89c822c955ee9913fd901c1988a204f.csv') 

# grrrr, make concentration a number
newicpdata <- newicpdata %>%
  mutate(concentration = as.numeric(concentration))

# combine old and new data, and change cols types as needed
tissue_data <- bind_rows(existTissue, newicpdata) %>%
  mutate(site_code = as.factor(site_code)) %>%
  mutate(treatment_code = as.factor(treatment_code)) %>%
  mutate(tissue_type = as.factor(tissue_type)) %>%
  mutate(instrument = as.factor(instrument)) %>% 
  mutate(plot_id = as.character(plot_id))

tissue_icp <- tissue_data # just a name change

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

tissue_icp_factors <- factorsToFrame(tissue_icp) # function not working had to to run it manually

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

# construct eml ----

dataset <- new("dataset",
               dataTable = c(tissue_icp_DT))

# as this is a revision, only the xml for the tables is required
eml <- new("eml",
           dataset = dataset,
           additionalMetadata = as(unitList, "additionalMetadata"))

write_eml(eml, "icp_2017.xml")

# starting over? ----

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
    EXTRACT (YEAR FROM st.post_date) = 2013 AND
    EXTRACT (MONTH FROM st.post_date) = 10 AND
    st.post_date IS NOT NULL AND
    t.code LIKE 'C1'
  GROUP BY plot_id, t.code, s.code, post_date
  ORDER BY post_date, plot_id;") %>% 
  mutate(tissue_type = 'Larrea tridentata') %>%
  rename(date = post_date)

oct_10_stems <- stemsDates 

# RUN: May, Oct 2009; May 2010; Oct 2013 ----

may_oct_09_may_10_oct_13 <- read_excel('~/Dropbox/development/plant_tissue/marisa_corrected/Cndep_ICP_MS_02252014 _(Corrected on 11302017).xlsm')
may_oct_09_may_10_oct_13 <- may_oct_09_may_10_oct_13 %>%
  separate(`Orig Conc.`, into = c("site_code", "plot_id", "treatment_code"), sep = "-") %>% 
  select(-`45Sc-CCT`, -`45Sc`, -`72Ge-CCT`, -`72Ge`, -`89Y-H2`, -`89Y`, -`115In-NH3`, -`115In`, -`209Bi-NH3`, -`209Bi`)

# May 2009

may_09 <- may_oct_09_may_10_oct_13 %>%
  slice(14:28) %>% 
  mutate(plot_id = as.integer(plot_id)) %>% 
  inner_join(may_09_stems, by = c("plot_id"))

# Oct 2009

oct_09 <- may_oct_09_may_10_oct_13 %>%
  slice(31:45) %>% 
  mutate(plot_id = as.integer(plot_id)) %>% 
  inner_join(oct_09_stems, by = c("plot_id"))

# May 2010 there are two dates for SRR in May 2010, not sure which day is
# correct but we can hazard a reasonable assumption based on the typical site
# grouping
may_10_stems <- may_10_stems %>% 
  filter(!(plot_id == 13 & date == "2010-05-12"))
  
may_10 <- may_oct_09_may_10_oct_13 %>%
  slice(56:70) %>% 
  mutate(plot_id = as.integer(plot_id)) %>% 
  inner_join(may_10_stems, by = c("plot_id"))

# Oct 2013

oct_13 <- may_oct_09_may_10_oct_13 %>%
  slice(73:87) %>% 
  mutate(plot_id = as.integer(plot_id)) %>% 
  inner_join(oct_10_stems, by = c("plot_id"))

# merge all data set from run
large_run <- bind_rows(may_09, oct_09, may_10, oct_13) %>%
  select(-site, -trt) %>% 
  gather(key = isotope_element, value = concentration, -site_code, -plot_id, -treatment_code, -date, -tissue_type) %>% 
  mutate(instrument = "ICP-MS") %>% 
  select(site_code, plot_id, treatment_code, sample_date = date, tissue_type, instrument, isotope_element, concentration)

