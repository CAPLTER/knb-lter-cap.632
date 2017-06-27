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
annuals_composition <- dbGetQuery(pg,
"SELECT
  s.code as site_code,
  ce.plot as plot_id,
  t.code as treatment_code,
  ce.patch_type as location_within_plot,
  ce.subplot,
  ce.sample_date as date,
  ce.year,
  ce.collector,
  ct.cover_type,
  ct.cover_category,
  cc.cover_amt as cover_amount
FROM
  urbancndep.cover_composition cc
  JOIN urbancndep.cover_events ce ON (cc.cover_event_id = ce.cover_event_id)
  JOIN urbancndep.cover_types ct ON (cc.cover_type_id = ct.cover_type_id)
  JOIN urbancndep.plots p ON (ce.plot = p.id)
  JOIN urbancndep.sites s ON (s.id = p.site_id)
  JOIN urbancndep.treatments t ON (t.id = p.treatment_id)
WHERE
  ce.year > 2008 AND
  ct.cover_type != 'total_comparable_annual_cover'
ORDER BY year, plot, location_within_plot, subplot, cover_type;")

# remove underscores from names of annuals. addressing this with base R and I am
# baffled as to why the dplyr approach commented below runs but mangles the data
annuals_composition[annuals_composition$cover_category == 'annual',]$cover_type <- str_replace(annuals_composition[annuals_composition$cover_category == 'annual',]$cover_type, "_", " ")

  # why is the dplyr approach not working for this?
  # also tried case_when and other approaches but consistently mangles the data
  # alpha <- annuals_composition %>% 
  #   mutate(cover_type = replace(cover_type, cover_category == "annual", str_replace(cover_type, "_", " ")))

# spread data and remove the sampled dummy variable
annuals_composition <- annuals_composition %>%
  select(-cover_category) %>%
  spread(cover_type, cover_amount) %>%
  arrange(year, plot_id) %>% 
  select(-sampled)

# change column types as appropriate
annuals_composition <- annuals_composition %>%
  mutate(site_code = as.factor(site_code)) %>%
  mutate(treatment_code = as.factor(treatment_code)) %>%
  mutate(location_within_plot = as.factor(location_within_plot)) %>% 
  mutate(subplot = as.factor(subplot)) %>% 
  mutate(plot_id = as.character(plot_id))

 # write data frame attributes to a csv in current dir to edit metadata
writeAttributes(annuals_composition, 'species or plot characteristic not present')

annuals_composition_desc <- 'Composition of annual plants and some other characteristics (e.g., bare soil, base or canopy of perennial plants) at subplots within Desert Fertilization study plots. One-meter subplots include locations around a Larrea tridentata plant and locations in the interplant space between shrubs. Estimates are based on 0.25 square meter quadrats within each subplot. All measurements collected in the spring.'

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

annuals_composition_factors <- factorsToFrame(annuals_composition)

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
annuals_composition_DT <- createDTFF(dfname = annuals_composition,
                                     factors = annuals_composition_factors,
                                     description = annuals_composition_desc,
                                     dateRangeField = 'year')

# viewEML(annuals_composition_DT)

# construct eml ----

dataset <- new("dataset",
               dataTable = c(annuals_composition_DT))

# as this is a revision, only the xml for the tables is required
eml <- new("eml",
           dataset = dataset)

write_eml(eml, "annuals_composition_2017.xml")


# assess and address taxonomic names ----

library(taxize)

annuals_taxa <- dbGetQuery(pg,"
SELECT
  DISTINCT(cover_type)
FROM urbancndep.cover_types
WHERE cover_category LIKE 'annual';")

annuals_taxa %<>% 
  mutate(cover_type = gsub("_", " ", cover_type)) %>%
  filter(!str_detect(cover_type, "unident"))

correctNames <- tnrs(query = annuals_taxa, source = "iPlant_TNRS") %>% 
  filter(score > 0.5 & score < 1) %>% 
  mutate(submittedname = str_replace(submittedname, " ", "_")) %>% 
  mutate(acceptedname = str_replace(acceptedname, " ", "_"))

for (i in 1:length(correctNames)) {

  rs <- "UPDATE urbancndep.cover_types SET cover_type = ?newName WHERE cover_type LIKE ?oldName;"
  alpha <- sqlInterpolate(ANSI(), rs, newName = correctNames[i,]$acceptedname, oldName = correctNames[i,]$submittedname)
  dbExecute(pg, alpha)

}

