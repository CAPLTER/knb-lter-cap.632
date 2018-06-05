
# README ------------------------------------------------------------------

# Code and metadata here is related to version 632.4. Des Fert (632) includes
# the following data entities: biovolume, soil pH, tissue, annuals comp 2008,
# annuals comp, annuals biomass, fertilization, PRS, raw ICP data. Of those,
# biovolume, soil pH, and annuals comp 2008 are unlikely to ever be updates (for
# sure not annuals comp 2008). As such, those metadata need to be extracted from
# the EML of previous versions since they will never actually be updated.

# 2017-06-28

# Note that the ICP and CHN worflows are separate from this main workflow owing
# to the longitudinal nature of those analyses.

# Note that we may need to add checksums and row numbers to meet future PASTA+
# data quality checks. The code here reflects some early updates to the workflow
# based on EMLassemblyline so look for departures from this workflow and the
# reml template in the future.

# Note that code is included to write each data entity separetely instead of a
# huge update. The decision to recreate all the metadata as presented here or
# just, for example, write updated stems data into an upversioned existing EML
# document will be a case-by-case basis depending, probably, on the extent of
# the update.

# There are separate workflows for addressing the taxonomic names of annuals in
# the sense of checking them against ITIS and updating to the correct
# name/spelling. There is also a separate workflow for generateing the
# taxonomicCoverage from these data, though Bryce and Carl are supposedly
# working on this and this may not need be a separate step in the future


# dataset details to set first ----
projectid <- '632'
packageIdent <- 'knb-lter-cap.632.5'
pubDate <- '2017-12-08'

# libraries ----
library(tidyverse)
library(RPostgreSQL)
library(RMySQL)
library(readxl)
library(EML)
library(stringr)


# connections -------------------------------------------------------------

# Amazon
source('~/Documents/localSettings/aws.s3')
  
# postgres
source('~/Documents/localSettings/pg_prod.R')
source('~/Documents/localSettings/pg_local.R')
  
pg <- pg_prod
pg <- pg_local

# mysql
source('~/Documents/localSettings/mysql_prod.R')
prod <- mysql_prod


# reml-helper-functions ----
# source('~/localRepos/reml-helper-tools/createdataTableFn.R')
source('~/localRepos/reml-helper-tools/writeAttributesFn.R')
source('~/localRepos/reml-helper-tools/createDataTableFromFileFn.R')
source('~/localRepos/reml-helper-tools/createKMLFn.R')
source('~/localRepos/reml-helper-tools/address_publisher_contact_language_rights.R')
source('~/localRepos/reml-helper-tools/createOtherEntityFn.R')
source('~/localRepos/reml-helper-tools/createPeople.R')
source('~/localRepos/reml-helper-tools/createFactorsDataframe.R')

# annuals_biomass ----

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

# # construct eml 
# 
# dataset <- new("dataset",
#                dataTable = c(annuals_biomass_DT))
# 
# # as this is a revision, only the xml for the tables is required
# eml <- new("eml",
#            dataset = dataset)
# 
# write_eml(eml, "annuals_biomass_2017.xml")


# annuals_composition ----

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

# # construct eml 
# 
# dataset <- new("dataset",
#                dataTable = c(annuals_composition_DT))
# 
# # as this is a revision, only the xml for the tables is required
# eml <- new("eml",
#            dataset = dataset)
# 
# write_eml(eml, "annuals_composition_2017.xml")


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


# fertilizer ----

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

# # construct eml 
# 
# dataset <- new("dataset",
#                dataTable = c(fertilizer_application_DT))
# 
# # as this is a revision, only the xml for the tables is required
# eml <- new("eml",
#            dataset = dataset)
# 
# write_eml(eml, "fertilizer_application_2017.xml")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
# stem growth -------------------------------------------------------------

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
      	NOT (
            -- omit post data not collected when smart sampling was implemented
        		EXTRACT (YEAR FROM st.pre_date) = 2016 AND
        		EXTRACT (MONTH FROM st.pre_date) = 10 AND
        		st.post_date IS NULL
      	) AND
        NOT (p.id = 13 AND st.pre_date = '2010-05-10') AND
        NOT (p.id = 12 AND st.pre_date = '2010-05-11')
      ORDER BY
        st.pre_date,
        p.id,
        sl.post_measurement,
        sh.code,
        st.direction;")

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


stem_growth_DT <- createDTFF(dfname = stem_growth,
                             factors = stem_growth_factors,
                             description = stem_growth_desc,
                             dateRangeField = 'pre_date')

# assuming you do not remove the NULL post_date values that have not yet been
# measured, then the pre_date is appropriate for both the min and max datasets;
# otherwise, will have to deal with the fact that the min and max of either the
# pre_or post_date will yield different date ranges, so need to do
# max(post_date) and edit by hand - or whatever solution
# dbGetQuery(pg, 'SELECT max(post_date) FROM urbancndep.stems;')

# construct eml
dataset <- new("dataset",
               dataTable = c(stem_growth_DT))

# as this is a revision, only the xml for the tables is required
eml <- new("eml",
           dataset = dataset)

write_eml(eml, "stem_growth_2017.xml")

# send data file to Amazon
dataToAmz <- function(fileToUpload) {
  
  put_object(file = fileToUpload,
             object = paste0('/datasets/cap/', basename(fileToUpload)),
             bucket = 'gios-data') 
  
}

# sensu:
dataToAmz('~/localRepos/knb-lter-cap.632/632_stem_growth_592318cb473e05c78364247b1c45abbf.csv')


# plant_root_simulator ----

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
# writeAttributes(plant_root_simulator)

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

dbGetQuery(pg, 'SELECT max(end_date) FROM urbancndep.prs_analysis;')

# construct eml
dataset <- new("dataset",
               dataTable = c(plant_root_simulator_DT))

# as this is a revision, only the xml for the tables is required
eml <- new("eml",
           dataset = dataset,
           additionalMetadata = as(unitList, "additionalMetadata"))

write_eml(eml, "plant_root_simulator_2017.xml")

# send data file to Amazon
dataToAmz <- function(fileToUpload) {
  
  put_object(file = fileToUpload,
             object = paste0('/datasets/cap/', basename(fileToUpload)),
             bucket = 'gios-data') 
  
}

# sensu:
dataToAmz('~/localRepos/knb-lter-cap.632/632_plant_root_simulator_5ebc781cc4444ee517f828603f10dece.csv')


# tissue ICP --------------------------------------------------------------
# record of data processing is kept for these data so the processing is detailed
# in a separate workflow: icp_tissue.R


# tissue CHN --------------------------------------------------------------
# record of data processing is kept for these data so the processing is detailed
# in a separate workflow: chn_tissue.R


# title and abstract ------------------------------------------------------

title <- 'Desert Fertilization Experiment: investigation of Sonoran desert ecosystem response to atmospheric deposition and experimental nutrient addition, ongoing since 2006'

# abstract from file or directly as text
# abstract <- as(set_TextType("abstract_as_md_file.md"), "abstract") 
abstract <- 'Launched in 2006 with support from the National Science Foundation (NSF) and leveraged by the CAP LTER, the Carbon and Nitrogen deposition (CNdep) project sought to answer the fundamental question of whether elemental cycles in urban ecosystems are qualitatively different from those in non-urban ecosystems. Ecosystem scientists, atmospheric chemists, and biogeochemists tested the hypothesis that distinct biogeochemical pathways result from elevated inorganic nitrogen and organic carbon deposition from the atmosphere to the land. To test the hypothesis, scientists examined the responsiveness of Sonoran desert ecosystems to nutrient enrichment by capitalizing on a gradient of atmospheric deposition in and around the greater Phoenix metropolitan area. Fifteen desert study sites were established, with five locations each west and east of the urban core, and in the urban core in desert preserves. In addition to the gradient of atmospheric deposition in and around the urban core, select study plots at each of the fifteen desert locations receive amendments of nitrogen, phosphorus, or nitrogen + phosphorus fertilizer. Measured variables include soil properties, perennial and annual plant growth, and atmospheric deposition of nitrogen. At the close of the initial grant period, the CAP LTER assumed responsibility for the project, renamed the Desert Fertilization Experiment, which provides a remarkable platform to study the long-term effects of nutrient enrichment on ecosystem properties.'


# people ----

jonAllen <- addCreator('jon', 'allen')
nancyGrimm <- addCreator('n', 'grimm')
sharonHall <- addCreator('s', 'hall')
jasonKaye <- addCreator('j', 'kaye')

creators <- c(as(jonAllen, 'creator'),
              as(nancyGrimm, 'creator'),
              as(sharonHall, 'creator'),
              as(jasonKaye, 'creator'))

chrisClark <- addMetadataProvider('c', 'clark')
elizabethCook <- addMetadataProvider('eli', 'cook')
stevanEarl <- addMetadataProvider('s', 'earl')
marisaMasles <- addMetadataProvider('m', 'masles')
quincyStewart <- addMetadataProvider('q', 'stewart')
sallyWittlinger <- addMetadataProvider('s', 'wittlinger')

metadataProvider <-c(as(chrisClark, 'metadataProvider'),
                     as(elizabethCook, 'metadataProvider'),
                     as(stevanEarl, 'metadataProvider'),
                     as(marisaMasles, 'metadataProvider'),
                     as(quincyStewart, 'metadataProvider'),
                     as(sallyWittlinger, 'metadataProvider'))

# keywords ----

# CAP IRTs for reference: https://sustainability.asu.edu/caplter/research/
# be sure to include these as appropriate

keywordSet <-
  c(new("keywordSet",
        keywordThesaurus = "LTER controlled vocabulary",
        keyword =  c("plants",
                     "vegetation",
                     "soil",
                     "soil chemistry",
                     "soil nitrogen",
                     "soil nutrients",
                     "soil ph",
                     "soil phosphorus",
                     "soil properties",
                     "soil samples",
                     "nitrogen deposition",
                     "plant biomass",
                     "plant growth",
                     "atmospheric deposition",
                     "bulk deposition",
                     "primary production",
                     "nitrogen",
                     "phosphorus")),
    new("keywordSet",
        keywordThesaurus = "LTER core areas",
        keyword =  c("disturbance patterns",
                     "primary production",
                     "movement of inorganic matter")),
    new("keywordSet",
        keywordThesaurus = "CAPLTER Keyword Set List",
        keyword =  c("cap lter",
                     "cap",
                     "caplter",
                     "central arizona phoenix long term ecological research",
                     "arizona",
                     "az",
                     "arid land",
                     "desert preserve"))
  )

# methods and coverages ----
methods <- set_methods("methods.md")

# if relevant, pulling dates from a DB is nice
# begindate <- dbGetQuery(con, "SELECT MIN(sample_date) AS date FROM database.table;")
# begindate <- begindate$date

getMaxDate <- function() {
  
  maxDates <- data.frame()
  maxDates <- rbind(
    dbGetQuery(pg, 'SELECT MAX(sample_date) FROM urbancndep.cover_events;'),
    dbGetQuery(pg, 'SELECT MAX(date) FROM urbancndep.annuals_biomass;'),
    dbGetQuery(pg, 'SELECT MAX(date) FROM urbancndep.fertilizer_applications;'),
    dbGetQuery(pg, 'SELECT MAX(post_date) FROM urbancndep.stems;'),
    dbGetQuery(pg, 'SELECT MAX(end_date) FROM urbancndep.prs_analysis;')
  )
 
  theMaxDate <- max(maxDates$max)
  
  return(theMaxDate)
  
}

begindate <- "2005-12-07"
enddate <- as.character(getMaxDate())
geographicDescription <- "desert and desert-remnant regional parks in the CAP LTER study area"
coverage <- set_coverage(begin = begindate,
                         end = enddate,
                         geographicDescription = geographicDescription,
                         west = -112.547375174279352, east = -111.482761068522152,
                         north = +33.726771147871851, south = +33.013262396669028)

# add taxonomic coverage derived separately (see below)
coverage@taxonomicCoverage <- c(desFertTaxa)

# construct the dataset ----

# address, publisher, contact, and rights come from a sourced file

# XML DISTRUBUTION
  xml_url <- new("online",
                 onlineDescription = "CAPLTER Metadata URL",
                 url = paste0("https://sustainability.asu.edu/caplter/data/data-catalog/view/", packageIdent, "/xml/"))
metadata_dist <- new("distribution",
                 online = xml_url)

# DATASET
dataset <- new("dataset",
               title = title,
               creator = creators,
               pubDate = pubDate,
               metadataProvider = metadataProvider,
               # associatedParty = associatedParty,
               intellectualRights = rights,
               abstract = abstract,
               keywordSet = keywordSet,
               coverage = coverage,
               contact = contact,
               methods = methods,
               distribution = metadata_dist,
               dataTable = c(annuals_biomass_DT,
                             annuals_composition_DT,
                             fertilizer_application_DT,
                             plant_root_simulator_DT))
               # otherEntity = c(core_arthropod_locations)) # if other entity is relevant

# ls(pattern= "_DT") # can help to pull out DTs

# assembly line flow that would be good to incorporate - build the list of DTs at creation
# data_tables_stored <- list()
# data_tables_stored[[i]] <- data_table
# dataset@dataTable <- new("ListOfdataTable",
#                      data_tables_stored)
  
  
# construct the eml ----

# ACCESS
allow_cap <- new("allow",
                 principal = "uid=CAP,o=LTER,dc=ecoinformatics,dc=org",
                 permission = "all")
allow_public <- new("allow",
                    principal = "public",
                    permission = "read")
lter_access <- new("access",
                   authSystem = "knb",
                   order = "allowFirst",
                   scope = "document",
                   allow = c(allow_cap,
                             allow_public))

# CUSTOM UNITS
# standardUnits <- get_unitList()
# unique(standardUnits$unitTypes$id) # unique unit types

# the PRS data does include a custom unit but creating this is not required
# if you are recycling previously constructed EML/XML with just updated data,
# but this will be required if you recreate 632 from scratch.

custom_units <- rbind(
  data.frame(id = "microgramPerTenSquareCentimeterPerBurialLength",
             unitType = "unknown",
             parentSI = "unknown",
             multiplierToSI = "unknown",
             description = "net rate of nutrient ion adsorption by the PRS® Probe expressed as the weight of nutrient adsorbed per surface area of ion-exchange membrane over time"))
unitList <- set_unitList(custom_units)

# note schemaLocation is new, not yet tried!
eml <- new("eml",
           schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
           packageId = packageIdent,
           scope = "system",
           system = "knb",
           access = lter_access,
           dataset = dataset,
           additionalMetadata = as(unitList, "additionalMetadata"))


# assembly line code to incorporate next round!

# if (custom_units == "yes"){
#   eml <- new("eml",
#              schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
#              packageId = data_package_id,
#              system = root_system,
#              access = access,
#              dataset = dataset,
#              additionalMetadata = as(unitsList, "additionalMetadata"))
# } else {
#   eml <- new("eml",
#              schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
#              packageId = data_package_id,
#              system = root_system,
#              access = access,
#              dataset = dataset)
# }
# 
# # Write EML
# 
# print("Writing EML ...")
# 
# write_eml(eml, paste(path, "/", data_package_id, ".xml", sep = ""))
# 
# # Validate EML
# 
# print("Validating EML ...")
# 
# validation_result <- eml_validate(eml)
# 
# if (validation_result == "TRUE"){
#   
#   print("EML passed validation!")
#   
# } else {
#   
#   print("EML validaton failed. See warnings for details.")
#   
# }

# write the xml to file ----
eml_validate(eml)
write_eml(eml, "knb-lter-cap.632.4.xml")


# taxonomicCoverage ----

getSlots("coverage")
getSlots("taxonomicCoverage")

# cool but lets let rEML do this for us 
# delta <- classification(annuals_taxa[c(1:3),], db = 'itis')

# get all taxa names from the DB
annuals_taxa <- dbGetQuery(pg,"
SELECT
  DISTINCT(cover_type)
FROM urbancndep.cover_types
WHERE cover_category LIKE 'annual';")

# take out the underscores and filter out the unknowns
annuals_taxa %<>% 
  mutate(cover_type = gsub("_", " ", cover_type)) %>%
  filter(!str_detect(cover_type, "unident"))

# add Larrea tridentata
annuals_taxa <- annuals_taxa %>% bind_rows(annuals_taxa, c(cover_type = "Larrea tridentata"))

# convert dataframe column to a vector of the names
binomial <- annuals_taxa[,"cover_type"]

# pass vector of sci names to set_taxonomicCoverage with the optional elements expand (set to TRUE) and a database to use. Note that set_taxonomicCoverage in the package did not seem to work, and I had to copy the code from the repo and overwrite the function (and embedded functions) in the current session. Note that you will get an error for any taxonomic names that are not identified so be sure to to a pre-cleaning step.
# set_taxonomicCoverage(binomial, expand = T, db = 'itis')
desFertTaxa <- set_taxonomicCoverage(binomial, expand = T, db = 'itis')

coverage@taxonomicCoverage <- c(desFertTaxa)

# setTaxonomicCoverage() ----
set_taxonomicCoverage <- function(sci_names, expand=FALSE, db = 'itis') {
  # Expand using taxize and ITIS if the user passes in just scientific names
  if (is(sci_names, "character") && expand) {
    if (!requireNamespace("taxize", quietly = TRUE)) {
      stop(call. = FALSE,
           "Expansion of scientific names requires the 'taxize' package to be installed. Install taxize or set expand to FALSE.")
    }
    
    classifications <- taxize::classification(sci_names, db = db)
    
    # Remove any NAs and warn for each
    if (any(is.na(classifications))) {
      warning(call. = FALSE,
              paste0("Some scientific names were not found in the taxonomic database and were not expanded: ", paste0(sci_names[which(is.na(classifications))], collapse = ","), "."))
    }
    
    # Turn result into a list of named lists where names are the rank name and
    # values are the rank value
    sci_names <- mapply(function(cls, sci_name) {
      # If the species name isn't found in the database, NA is returned
      if (is.list(cls)) {
        x <- as.list(cls[["name"]])
        names(x) <- cls[["rank"]]
        x
      } else {
        x <- list(list("species" = as.character(sci_name)))
        names(x) <- sci_name
        x
      }
    }, classifications, names(classifications), SIMPLIFY = FALSE)
  }
  
  if (class(sci_names) == "character") {
    taxa <- lapply(sci_names, function(sci_name) {
      s <- strsplit(sci_name, " ")[[1]]
      new(
        "taxonomicClassification",
        taxonRankName = "genus",
        taxonRankValue = s[[1]],
        taxonomicClassification = c(
          new(
            "taxonomicClassification",
            taxonRankName = "species",
            taxonRankValue = sci_name
          )
        )
      )
    })
    new("taxonomicCoverage",
        taxonomicClassification = do.call(c, taxa))
  } else if (class(sci_names) == "data.frame") {
    taxon_classification <- colnames(sci_names)
    new <- as.data.frame(t(sci_names))
    colnames(new) <- NULL
    taxa <- lapply(new, function(sci_name) {
      tc <- lapply(taxon_classification, function(name) {
        new(
          "taxonomicClassification",
          taxonRankName = name,
          taxonRankValue = as.character(sci_name[name])
        )
      })
      tc <- formRecursiveTree(tc)[[1]]
    })
    new("taxonomicCoverage",
        taxonomicClassification = do.call(c, taxa))
  } else if (class(sci_names) == "list") {
    # Warn if not a list of lists
    if (!all(vapply(sci_names, class, "") == "list")) {
      message("sci_names should be a list of lists. Your input was automatically wrapped up in a list.")
      sci_names <- list(sci_names)
    }
    
    taxa <- lapply(sci_names, function(sci_name) {
      taxonRankNames <- as.list(names(sci_name))
      
      taxa <- lapply(taxonRankNames, function(name) {
        new(
          "taxonomicClassification",
          taxonRankName = as.character(name),
          taxonRankValue = as.character(sci_name[[name]])
        )
      })
      formRecursiveTree(taxa)
    })
    
    new("taxonomicCoverage",
        taxonomicClassification = new("ListOftaxonomicClassification", do.call(c, taxa)))
  } else {
    stop("Incorrect format: sci_names can only be character string, data.frame or list")
  }
}

# helper function: form a nested tree recursively
formRecursiveTree <- function(listOfelements) {
  if (length(listOfelements) == 1 ||
      length(listOfelements) == 2 && is.null(listOfelements[[2]])) {
    return(do.call(c, listOfelements[1]))
  } else if (is.null(listOfelements[[1]])) {
    formRecursiveTree(listOfelements[2:length(listOfelements)])
  } else {
    listOfelements[[1]]@taxonomicClassification <- formRecursiveTree(listOfelements[2:length(listOfelements)])
    return(do.call(c, listOfelements[1]))
  }
}