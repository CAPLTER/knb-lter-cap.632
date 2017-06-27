projectid <- '632'

# libraries ----
library(tidyverse)
library(RPostgreSQL)
library(readxl)
library(EML)
# library(tools)
# library(zoo)

# db connections ----

source('~/Documents/localSettings/pg_prod.R')
source('~/Documents/localSettings/pg_local.R')

pg <- pg_prod
pg <- pg_local

dbGetInfo(pg)


# reml-helper-functions ----
# source('~/localRepos/reml-helper-tools/createdataTableFn.R')
source('~/localRepos/reml-helper-tools/writeAttributesFn.R')
source('~/localRepos/reml-helper-tools/createDataTableFromFileFn.R')
source('~/localRepos/reml-helper-tools/createKMLFn.R')
source('~/localRepos/reml-helper-tools/address_publisher_contact_language_rights.R')
source('~/localRepos/reml-helper-tools/createOtherEntityFn.R')
source('~/localRepos/reml-helper-tools/createPeople.R')
source('~/localRepos/reml-helper-tools/createFactorsDataframe.R')

# data entity ----

plant_root_simulator <- dbGetQuery(pg,'
SELECT
   s.code AS site_code,
   p.id AS plot_id,
   t.code AS treatment_code,
   pa.start_date,
   pa.end_date,
   pa.analyte,
   pa.final_value,
   pa.flag,
   pa.location_within_plot,
   pa.num_cation_probes,
   pa.num_anion_probes
 FROM urbancndep.sites s
   JOIN urbancndep.plots p ON s.id = p.site_id
   JOIN urbancndep.treatments t ON p.treatment_id = t.id
   JOIN urbancndep.prs_analysis pa ON p.id = pa.plot_id
ORDER BY pa.start_date;')

# standardize sample type names
plant_root_simulator <- plant_root_simulator %>%
  filter(!grepl("NH4", location_within_plot)) %>% # remove the washing experiment samples
  mutate(location_within_plot = replace(location_within_plot, location_within_plot == 'between plants', 'between_plant')) %>%
  mutate(location_within_plot = tolower(location_within_plot)) %>% 
  mutate(location_within_plot = gsub(" ", "_", location_within_plot)) %>% 
  mutate(location_within_plot = gsub("-", "_", location_within_plot))

# change column types as appropriate
plant_root_simulator <- plant_root_simulator %>%
  mutate(site_code = as.factor(site_code)) %>%
  mutate(treatment_code = as.factor(treatment_code)) %>%
  mutate(analyte = as.factor(analyte)) %>%
  mutate(location_within_plot = as.factor(location_within_plot)) %>% 
  mutate(plot_id = as.character(plot_id))

 # write data frame attributes to a csv in current dir to edit metadata
writeAttributes(plant_root_simulator)

plant_root_simulator_desc <- 'Soil ion concentrations as determined with Plant Root Simulator (PRS®) probes (ion exchange resin membranes). Probes for the analyses of soil anions have a positively-charged membrane to simultaneously attract and adsorb all negatively-charged anions, such as nitrate (NO3-), phosphate (H2PO4-, HPO42-), and sulphate (SO42-), whereas cation probes have a negatively-charged membrane to simultaneously attract and adsorb all positively-charged cations, such as ammonium (NH4+), potassium (K+), calcium (Ca2+), and magnesium (Mg2+).'

# address factors if needed
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
treatment_code <- c(C1 = "control plot 1",
                    N = "nitrogen amendment",
                    C2 = "control plot 2",
                    P = "phosphorus amendment",
                    NP = "nitrogen + phosphorus amendment")
analyte <- c(`NH4-N` = "ammonium-nitrogen",
             `Total-N` = "sum of NO3-N and NH4-N",
             `NO3-N` = "nitrate-nitrogen",
             Zn = "zinc",
             Al = "aluminum",
             Cu = "copper",
             Mn = "Manganese",
             Pb = "lead",
             Fe = "iron",
             P = "phosphorus",
             S = "sulfur",
             K = "potassium",
             Cd = "cadmium",
             Mg = "magnesium",
             B = "boron",
             Ca = "calcium")
location_within_plot <- c(under_plant = "probes buried beneath the canopy of Larrea tridentata",
                          between_plant = "probes buried between plant canopies",
                          blk_washed = "field blank (washed): probe stored in a clean container under refrigeration during the deployment period, and washed as per sample probes",
                          blank = "field blank: probe stored in a clean container under refrigeration during the deployment period")

plant_root_simulator_factors <- factorsToFrame(plant_root_simulator)

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
plant_root_simulator_DT <- createDTFF(dfname = plant_root_simulator,
                                      factors = plant_root_simulator_factors,
                                      description = plant_root_simulator_desc,
                                      dateRangeField = 'start_date')

# will have to deal with the fact that the min and max of eithr start or end
# date will yield different date ranges, so need to do max(end_date) and edit by
# hand - or whatever solution

# construct eml ----

dataset <- new("dataset",
               dataTable = c(plant_root_simulator_DT))

# as this is a revision, only the xml for the tables is required
eml <- new("eml",
           dataset = dataset)

write_eml(eml, "plant_root_simulator_2017.xml")