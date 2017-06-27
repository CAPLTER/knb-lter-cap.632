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

fertilizer_application <- dbGetQuery(pg,'
SELECT
  s.code as site_code,
  f.date as application_date,
  f."N" as nitrogen,
  f."P" as phosphorus
FROM urbancndep.fertilizer_applications f
  JOIN urbancndep.sites s ON (f.site_id = s.id)
ORDER BY f.date, s.code
;')

# change column types as appropriate
fertilizer_application <- fertilizer_application  %>%
  mutate(site_code = as.factor(site_code))

 # write data frame attributes to a csv in current dir to edit metadata
writeAttributes(fertilizer_application)

fertilizer_application_desc <- 'catalog of amounts and timing of nitrogen and phosphorus fertilizer applications to nitrogen (N), phosphorus (P), and nitrogen+phosphorus (N+P) treatment plots'

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

fertilizer_application_factors <- factorsToFrame(fertilizer_application)

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
fertilizer_application_DT <- createDTFF(dfname = fertilizer_application,
                                        factors = fertilizer_application_factors,
                                        description = fertilizer_application_desc,
                                        dateRangeField = 'application_date')

# construct eml ----

dataset <- new("dataset",
               dataTable = c(fertilizer_application_DT))

# as this is a revision, only the xml for the tables is required
eml <- new("eml",
           dataset = dataset)

write_eml(eml, "fertilizer_application_2017.xml")

# custom unit ----

# this data entity does entail a custom unit but creating this is not required
# if you are recycling previously constructed EML/XML with just updated data,
# but this will be required if you recreate 632 from scratch.
microgramPerTenSquareCentimeterPerBurialLength <- eml_define_unit(id = "microgramPerTenSquareCentimeterPerBurialLength",
                                                                  parentSI = "unknown",
                                                                  unitType = "unknown",
                                                                  multiplierToSI = "unknown",
                                                                  description = "net rate of nutrient ion adsorption by the PRS® Probe expressed as the weight of nutrient adsorbed per surface area of ion-exchange membrane over time")
