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

# note that the two where clause statements are to omit duplicate entries for
# those two sites and dates. In each case, the pre-dates are incorrect but
# there are entries with the correct pre-dates

stem_growth <- dbGetQuery(pg,"
SELECT
  s.code AS site_code,
  p.id AS plot_id,
  t.code AS treatment_code,
  sp.scientific_name,
  sh.code AS shrub_code,
  st.direction,
  st.pre_date,
  st.post_date,
  st.post_note,
  sl.post_measurement,
  sl.length_in_mm as stem_length,
  sc.comment
FROM urbancndep.stems st
JOIN urbancndep.shrubs sh ON sh.id = st.shrub_id
JOIN urbancndep.plots p ON sh.plot_id = p.id
JOIN urbancndep.sites s ON p.site_id = s.id
JOIN urbancndep.treatments t ON p.treatment_id = t.id
JOIN urbancndep.shrub_species sp ON sh.shrub_species_id = sp.id
LEFT JOIN urbancndep.stem_lengths sl ON st.id = sl.stem_id
LEFT JOIN urbancndep.stem_comment sc ON (sc.stem_id = st.id AND sl.post_measurement = sc.post_measurement)
WHERE
   NOT (p.id = 13 AND st.pre_date = '2010-05-10') AND
   NOT (p.id = 12 AND st.pre_date = '2010-05-11')
ORDER BY st.pre_date, p.id, sl.post_measurement, sh.code, st.direction;")

stem_growth <- stem_growth %>%
  mutate(site_code = as.factor(site_code)) %>%
  mutate(treatment_code = as.factor(treatment_code)) %>%
  mutate(shrub_code = as.factor(shrub_code)) %>%
  mutate(direction = as.factor(direction)) %>%
  mutate(plot_id = as.character(plot_id)) %>%
  mutate(post_measurement = as.factor(plot_id))

# write data frame attributes to a csv in current dir to edit metadata
writeAttributes(stem_growth)

stem_growth_desc <- 'Biannual measures of stem growth on five Larrea tridentata study plants in Desert Fertilization experiment treatment and control plots'

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
shrub_code <- c(L1 = "Larrea tridentata study plant #1",
                L2 = "Larrea tridentata study plant #2",
                L3 = "Larrea tridentata study plant #3",
                L4 = "Larrea tridentata study plant #4",
                L5 = "Larrea tridentata study plant #5")
direction <- c(South = "stem in south quadrant of plant",
               West = "stem in west quadrant of plant",
               North = "stem in north quadrant of plant",
               East = "stem in east quadrant of plant")
post_measurement <- c(`TRUE` = "final: post stem-length measurement",
                      `FALSE` = "initial: pre stem-length measurement")

stem_growth_factors <- factorsToFrame(stem_growth)

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
stem_growth_DT <- createDTFF(dfname = stem_growth,
                             factors = stem_growth_factors,
                             description = stem_growth_desc,
                             dateRangeField = 'pre_date')

# construct eml ----

dataset <- new("dataset",
               dataTable = c(stem_growth_DT))

# as this is a revision, only the xml for the tables is required
eml <- new("eml",
           dataset = dataset)

write_eml(eml, "stem_growth_2017.xml")
