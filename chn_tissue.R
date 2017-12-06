
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
       EXTRACT (YEAR FROM st.post_date) = 2015 AND
       EXTRACT (MONTH FROM st.post_date) = 10 AND
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


# RUN: Larrea fall 2015 ---------------------------------------------------

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
  

 
