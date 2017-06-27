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

# add filter to remove 2008 data from publication
# add filter to remove total comparable annual cover from publication (could be removed from DB as well)
annuals_biomass <- dbGetQuery(pg,"
SELECT
  s.code as site_code,
  ab.plot_id,
  t.code as treatment_code,
  ab.location_within_plot as location_within_plot,
  ab.replicate as subplot,
  ab.subquad_orientation,
  ab.date,
  ab.year,
  ab.mass,
  ab.notes
FROM
  urbancndep.annuals_biomass ab
  JOIN urbancndep.plots p ON (ab.plot_id = p.id)
  JOIN urbancndep.sites s ON (s.id = p.site_id)
  JOIN urbancndep.treatments t ON (t.id = p.treatment_id)
ORDER BY year, plot_id, location_within_plot, subplot, subquad_orientation;")

# change column types as appropriate
annuals_biomass <- annuals_biomass %>%
  mutate(site_code = as.factor(site_code)) %>%
  mutate(treatment_code = as.factor(treatment_code)) %>%
  mutate(location_within_plot = as.factor(location_within_plot)) %>% 
  mutate(subplot = as.factor(subplot)) %>% 
  mutate(plot_id = as.character(plot_id))

 # write data frame attributes to a csv in current dir to edit metadata
writeAttributes(annuals_biomass)

annuals_biomass_desc <- 'Biomass (g) of annual plants harvested from subplots within Desert Fertilization study plots. One-meter subplots include locations around a Larrea tridentata plant and locations in the interplant space between shrubs. Material is harested from 0.25 square meter quadrats within each subplot. All harvests occur during the spring.'

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
location_within_plot <- c(P = "subplot features a Larrea tridentata plant",
                          IP = "subplot located in an interplant space")
subplot <- c(`1` = "one of two replicates",
             `2` = "second of two replicates")

annuals_biomass_factors <- factorsToFrame(annuals_biomass)

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
annuals_biomass_DT <- createDTFF(dfname = annuals_biomass,
                                 factors = annuals_biomass_factors,
                                 description = annuals_biomass_desc,
                                 dateRangeField = 'year')

# construct eml ----

dataset <- new("dataset",
               dataTable = c(annuals_biomass_DT))

# as this is a revision, only the xml for the tables is required
eml <- new("eml",
           dataset = dataset)

write_eml(eml, "annuals_biomass_2017.xml")