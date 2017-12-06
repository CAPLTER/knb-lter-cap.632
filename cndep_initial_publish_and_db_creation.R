# CNdep -- there is a mix here of database generation for new items, such as the
# annuals, and publishing (for all). there is also a mix of use of the new and
# old versions of REML. the dataset was originally constructed with the old
# versions, but the new version was used when addressing updates to the icpms
# (adding the 2015 data) and fert applications data. xml for those tables was
# substituted into the originally constructed xml document

# working on icp 2016 data and it is apparant that this document is going to
# become unmanageable with the endless updates that cndep will experience.
# beginning with icp 2016 and stems 2016, will put updates in separate scripts.
# particularly reasonable here since most of the document will not change with
# any given update.

# update 2017-06-21. Working with 2017 annuals data, I noticed that in a case 
# where there were no data for a particular plot (that is, no annual or 
# perennial plant details - completely barren) that I was unable to join sample 
# event data on to the cover data giving the appearance that the site was not 
# visited when in fact it was visited but was, again, barren. To rectify this, I
# am adding a dummy variable called 'sampled' in the cover data that should be 
# added to all cover data. This should create a record for all sampling events 
# even rare instances where the plot is completely barren. Having recognized 
# this problem not until 2017, I was worried that this issue may have occurred 
# in pre-2017 data that were part of the initial database creation and 
# publication. When perusing the earlier data for this potential issue, I 
# noticed that I had culled records where collector metadata were not provided. 
# This, I believe in hindsight, was a mistake. To address this and add the 
# aforementioned sample dummy variable, I am having to completely re-do the 
# pre-2017 data. The annuals code in this file thus reflects a mix of the 
# initial database creation, revised data processing/database manipulations, and
# publishing. Much of the code for data processing was able to be recycled, and 
# I have retained most of the code for the initial database creation. Some of 
# the data processing steps were changed during the re-do. The revised data 
# insertion draws on the fact that the cover_type data from the inital database 
# creation were still just fine. Also, the initial database creation drew on the
# approach of creating a sequence in R for the serial key in postgres where the 
# revised approach includes letting the database create the key then re-pulling 
# those data for matching. Note that I was able to truncate both the
# cover_events and cover_composition tables so they did not have to be
# recreated, just repopulated

# libraries ----
# library("EML")
library("tidyverse")
library('RPostgreSQL')
library("devtools")
library("tools")
library("googlesheets")
library("zoo")

# functions and working dir ----
source('~/Dropbox (ASU)/localRepos/dataPublishing/createKMLFn.R')
source('~/Dropbox (ASU)/localRepos/dataPublishing/createOtherEntityFn.R')
source('~/Dropbox (ASU)/localRepos/dataPublishing/createdataTableFn.R')
source('~/Dropbox (ASU)/localRepos/dataPublishing/address_publisher_contact_language_rights.R')

# DB connections ----
source('~/Documents/localSettings/pg_prod.R')
source('~/Documents/localSettings/pg_local.R')

pg <- pg_prod
pg <- pg_local

dbGetInfo(pg)

# annuals cover ----

# steps:
# (1) clean up the data from Sharon
# (2) split up and write database-oriented tables to pg
# (3) pull from pg as would be done for future iterations

# one-time DB ops to truncate the cover_event and _composition data from the
# initial publication 
# dbExecute(pg, 'TRUNCATE TABLE urbancndep.cover_composition;') 
# dbExecute(pg, 'ALTER SEQUENCE urbancndep.cover_composition_cover_id_seq RESTART WITH 1;')
# 
# dbExecute(pg, 'TRUNCATE TABLE urbancndep.cover_events CASCADE;') dbExecute(pg,
# 'ALTER SEQUENCE urbancndep.cover_events_cover_event_id_seq RESTART WITH 1;')

annuals_cover <- read_csv('~/Dropbox (ASU)/development/annuals/Newest_Master_Annuals dataset_2008-2015+2016.csv', 
                          col_names = T, 
                          trim_ws = T)

# KLB had double-reported subplot type at MVP plot 37, change IP -> P for the two P plots
# KLB had double-reported subplot type at LDP plot 5, change IP -> P for the two P plots
annuals_cover[c(1083:1084),]$`Subplot type` = 'P'
annuals_cover[c(975:976),]$`Subplot type` = 'P'

# clean up names
colnames(annuals_cover) <- gsub("%", "", colnames(annuals_cover))
colnames(annuals_cover) <- gsub("/", "_", colnames(annuals_cover))
colnames(annuals_cover) <- str_trim(colnames(annuals_cover), "both")
colnames(annuals_cover) <- gsub(" - ", "_", colnames(annuals_cover), fixed = T)
colnames(annuals_cover) <- gsub("-", "_", colnames(annuals_cover))
colnames(annuals_cover) <- gsub("\\r\\n", " ", colnames(annuals_cover))

# add note regarding when personnel data are not included
annuals_cover <- annuals_cover %>% 
  mutate(Collector = replace(Collector, is.na(Collector), 'not reported'))

# annuals cover events -----

# pare down to relevant cols for events
cover_event <- annuals_cover %>%
  mutate(sample_date = as.POSIXct(Date, format = "%Y/%m/%d")) %>%
  select(sample_date,
         year = Year,
         plot = `Plot num`,
         patch_type = `Subplot type`,
         subplot = `Subplot num`,
         collector = Collector)

# check for uniquness - should not be any duplicates
cover_event %>%
  count(year, plot, patch_type, subplot) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  filter(n > 1)

# cover_events to pg
if (dbExistsTable(pg, c('urbancndep', 'temp_cover_events_old'))) dbRemoveTable(pg, c('urbancndep', 'temp_cover_events_old'))
dbWriteTable(pg, c('urbancndep', 'temp_cover_events_old'), value = cover_event, row.names = F)

dbExecute(pg,'
          ALTER TABLE urbancndep.temp_cover_events_old
          ALTER COLUMN sample_date TYPE DATE;')

insert_events_query <-
'INSERT INTO urbancndep.cover_events
(
  sample_date,
  year,
  plot,
  patch_type,
  subplot,
  collector
)
(
  SELECT
    sample_date,
    year,
    plot,
    patch_type,
    subplot,
    collector
  FROM
  urbancndep.temp_cover_events_old
);'

dbExecute(pg, insert_events_query)

# remove temp table
if (dbExistsTable(pg, c('urbancndep', 'temp_cover_events_old'))) dbRemoveTable(pg, c('urbancndep', 'temp_cover_events_old'))

# relic code for initial creation of the cover_events table
# # change owner to urbancndep before writing to stego
# # update events table as appropriate
# # careful sometimes you get an error but it still worked so check before doing anything,
# # sometimes this fails and the fix is to reload pg
# dbSendQuery(pg,
#             '
#             ALTER TABLE urbancndep.cover_events OWNER TO urbancndep;
# 
#             CREATE SEQUENCE urbancndep.cover_events_cover_event_id_seq;
# 
#             ALTER TABLE urbancndep.cover_events
#             ALTER COLUMN cover_event_id SET DEFAULT nextval(\'urbancndep.cover_events_cover_event_id_seq\'),
#             ALTER COLUMN cover_event_id SET NOT NULL,
#             ADD CONSTRAINT cover_events_fk_plot_id
#                 FOREIGN KEY (plot)
#                 REFERENCES urbancndep.plots(id),
#             ADD PRIMARY KEY(cover_event_id);
# 
#             ALTER SEQUENCE urbancndep.cover_events_cover_event_id_seq OWNER TO urbancndep;
#             ALTER SEQUENCE urbancndep.cover_events_cover_event_id_seq OWNED BY urbancndep.cover_events.cover_event_id;
# 
#             SELECT setval(\'urbancndep.cover_events_cover_event_id_seq\', mx.mx)
#             FROM (SELECT MAX(cover_event_id) AS mx FROM urbancndep.cover_events) mx;'
#             )

# cover data ----

# converting to numeric throws some error messages. looks like some misc text
# (e.g., 'nd') interspersed here and there

  # helpful snooping tools:
    # apply(annuals_cover, 2, function(x) unique(x))
    # justchars <- annuals_cover[lapply(annuals_cover, class) == "character"]
    # apply(justchars, 2, function(x) unique(x))
    # str(annuals_cover, list.len = ncol(annuals_cover))
    # annuals_cover[!is.na(annuals_cover$`Chorizanthe rigida`),]$`Chorizanthe rigida`

# beginning after Date or so convert cols (there are some of type char) to# num
annuals_cover[,c(12:ncol(annuals_cover))] <- apply(annuals_cover[,c(12:ncol(annuals_cover))], 2, function(x) as.numeric(as.character(x)))

# get event data that was previously added
event_id <- dbGetQuery(pg,'
                        SELECT
                          cover_event_id,
                          year,
                          plot,
                          patch_type,
                          subplot
                        FROM urbancndep.cover_events;')

# join annuals_cover on event_id to add event_id
annuals_cover <- annuals_cover %>% 
  left_join(event_id, by = c("Plot num" = "plot", "Subplot type" = "patch_type", "Subplot num" = "subplot", "Year" = "year"))

# combine unknowns, pectocaryas, lots of renaming and adding
annuals_cover <- annuals_cover %>%
  rowwise() %>%
  mutate(Pectocarya = sum(`CONVERTED  Pectocarya spp`, `Unknown Pectocarya sp.`, na.rm = T)) %>%
  mutate(unidentified = sum(`Unknown 3 LDP_2013`, `Unknown 1 LDP_2013`, `Unknown 1 SRR_2013`,`Unknown 1 PWP_2013`,`Unknown 1 DBG_2013`,`Unknown 1 SNE_2013`,`Unknown UMP_2015`, `Unknown 2 PWP_2013`, `Unknown 2 LDP GRASS_2013`, na.rm = T)) %>%
  mutate(`Lotus strigosus` = sum(`Lotus strigosus`, `Lotus tomentellus`, na.rm = T)) %>%
  mutate(Cryptantha = sum(`Cryptantha Unknown spp`, `Unknown Cryptantha`, na.rm = T)) %>%
  ungroup() %>% # turn off rowwise
  # mutate(Pectocarya = replace(Pectocarya, Pectocarya == 0, NA)) %>%
  # mutate(unidentified = replace(unidentified, unidentified == 0, NA)) %>%
  # mutate(`Lotus strigosus` = replace(`Lotus strigosus`, `Lotus strigosus` == 0, NA)) %>%
  # mutate(Cryptantha = replace(Cryptantha, Cryptantha  == 0, NA)) %>%
  select(cover_event_id,
         total_comparable_annual_cover = `Total comparable annual cover (1_bare)`,
         soil_crusts = `soil crusts`,
         total_shrub_cover = `Total  cover shrub`,
         Ambrosia_base = `Ambrosia base`,
         Ambrosia_stem = `Ambrosia stem`,
         Ambrosia_cover = `Ambrosia cover`,
         Larrea_base = `LATR base`,
         Larrea_cover = `LATR Cover`,
         Simmondsia_chinensis_cover = `Simmondsia chinensis (Jojoba) Cover`,
         Cylindropuntia_acanthocarpa_base = `Cylindropuntia acanthocarpa Base`,
         Cylindropuntia_acanthocarpa_cover = `Cylindropuntia acanthocarpa Cover`,
         Cylindropuntia_bigelovii_base = `Cylindropuntia bigelovii base`,
         Cylindropuntia_bigelovii_cover = `Cylindropuntia bigelovii Cover`,
         Lycium_base = `Lycium sp. Base`,
         Lycium_cover = `Lycium sp. Cover`,
         Krameria_grayi_base = `Krameria grayi base`,
         Krameria_grayi_cover = `Krameria grayi Cover`,
         Parkinsonia_microphylla_base = `Parkinsonia microphylla base`,
         Parkinsonia_microphylla_cover = `Parkinsonia microphylla Cover`,
         `Pectocarya heterocarpa`:`Lotus salsuginosus`,
         `Bowlesia incana`:`Eschscholzia californica`,
         Caulanthus_lasiophyllus = `Guillenia lasiophylla`,
         `Lepidium lasiocarpum`:`Herniaria hirusta`,
         Chaenactis_stevioides = `Chaenactis steviades`,
         `Rafinesquia neomexicana`:`Dichelostemma capitatum`,
         Eriophyllum_lanosum = `Antheropeas (Eriophyllum) lanosum`,
         `Draba cuneifolia`:`Eucrypta chrysamthemifolia`,
         Chorizanthe = `Chorizanthe rigida_brevicornu`,
         `Chorizanthe brevicornu`:`Cylindropuntia acanthocarpa`,
         Ditaxis_lanceolata = `Argythamnia lanceolata`,
         `Ditaxis neomexicana`,
         Boraginaceae = `Unknown Boraginaceae`,
         `Aristida adsencionis`,
         `Eriogonum thomasii`,
         Brassicaceae = `Unknown Brassicaceae`,
         Linanthus = `Uknown Linanthus`,
         Polemoniaceae = `Uknown Polemoniaceae`,
         Astragalus = `Astragalus Unknown spp_2013`,
         Chamaesyce = `Chamaesyce Unknown spp_2013`,
         Pectocarya:Cryptantha) %>%
  rename(Logfia_arizonica = `Logfia (Filago) arizonica`)

# spaces to underscores
colnames(annuals_cover) <- gsub(" ", "_", colnames(annuals_cover))

# add a dummy variable that will facilitate creating a record when pulled from
# the database even when no plants or plot characteristics were present
annuals_cover <- annuals_cover %>% 
  mutate(sampled = TRUE)
  
# stack the data and filter out the zeroes
annuals_cover <- annuals_cover %>%
  gather(cover_type, cover_amt, 2:length(annuals_cover)) %>% 
  filter(cover_amt != 0) %>% 
  filter(!is.na(cover_amt))

# get cover types and merge to composition data

# one-time add the new sampled dummy variable
dbExecute(pg,"
          INSERT INTO urbancndep.cover_types (cover_category, cover_type) VALUES
          ('record_keeping','sampled');")

# get the cover types that had already been added to the DB at initial creation
cover_types <- dbGetQuery(pg, '
                          SELECT
                            cover_type_id,
                            cover_category,
                            cover_type
                          FROM urbancndep.cover_types;') %>% 
  mutate(cover_type = tolower(cover_type)) # to lower so we can ignore case

# join the annuals cover data with the cover_types data and pare down to just
# the cover_type_id
annuals_cover <- annuals_cover %>% 
  mutate(cover_type = tolower(cover_type)) %>% # to lower so we can ignore case
  left_join(cover_types, by = c("cover_type" = "cover_type")) %>% 
  select(cover_event_id,
         cover_type_id,
         cover_amt)

# annuals_cover to pg
if (dbExistsTable(pg, c('urbancndep', 'temp_annuals_composition'))) dbRemoveTable(pg, c('urbancndep', 'temp_annuals_composition'))
dbWriteTable(pg, c('urbancndep', 'temp_annuals_composition'), value = annuals_cover, row.names = F)

insert_composition_query <-
'INSERT INTO urbancndep.cover_composition
(
  cover_event_id,
  cover_type_id,
  cover_amt
)
(
  SELECT
    cover_event_id,
    cover_type_id,
    cover_amt
  FROM
  urbancndep.temp_annuals_composition
);'

dbExecute(pg, insert_composition_query)

# remove the temp table
if (dbExistsTable(pg, c('urbancndep', 'temp_annuals_composition'))) dbRemoveTable(pg, c('urbancndep', 'temp_annuals_composition'))
  
# relic initial code for writing composition data
# write cover data to pg
# if (dbExistsTable(pg, c('urbancndep', 'cover_composition'))) dbRemoveTable(pg, c('urbancndep', 'cover_composition')) # make sure tbl does not exist
# dbWriteTable(pg, c('urbancndep', 'cover_composition'), value = cover, row.names = F)
# 
# # change owner to urbancndep before writing to stego
# # update events table as appropriate
# # careful sometimes you get an error but it still worked so check before doing anything,
# # sometimes this fails and the fix is to reload pg
# dbSendQuery(pg,
#             '
#             ALTER TABLE urbancndep.cover_composition OWNER TO urbancndep;
# 
#             CREATE SEQUENCE urbancndep.cover_composition_cover_id_seq;
# 
#             ALTER TABLE urbancndep.cover_composition
#             ALTER COLUMN cover_id SET DEFAULT nextval(\'urbancndep.cover_composition_cover_id_seq\'),
#             ALTER COLUMN cover_id SET NOT NULL,
#             ADD CONSTRAINT cover_composition_fk_cover_event_id
#                 FOREIGN KEY (cover_event_id)
#                 REFERENCES urbancndep.cover_events(cover_event_id),
#             ADD CONSTRAINT cover_composition_fk_cover_type_id
#                 FOREIGN KEY (cover_type_id)
#                 REFERENCES urbancndep.cover_types(cover_type_id),
#             ADD PRIMARY KEY(cover_id);
# 
#             ALTER SEQUENCE urbancndep.cover_composition_cover_id_seq OWNER TO urbancndep;
#             ALTER SEQUENCE urbancndep.cover_composition_cover_id_seq OWNED BY urbancndep.cover_composition.cover_id;
# 
#             SELECT setval(\'urbancndep.cover_composition_cover_id_seq\', mx.mx)
#             FROM (SELECT MAX(cover_id) AS mx FROM urbancndep.cover_composition) mx;'
#             )


# initial creation of cover_types ----
# # cover_types <- unique(annuals_cover[,c('cover_type')])
# # cover_types <- cover_types %>%
# #   mutate(cover_type_id = seq_along(cover_type)) %>%
# #   mutate(cover_category = 'annual') %>%
# #   mutate(cover_category = ifelse(cover_type_id <= 19, 'plot characteristic', cover_category)) %>%
# #   select(cover_type_id, cover_category, cover_type) %>%
# #   as.data.frame()
# 
# # # join names and the stacked data and prep for write to pg
# # cover <- inner_join(annuals_cover, cover_types, by = c("cover_type" = "cover_type")) %>%
# #   filter(!is.na(cover_amt)) %>%
# #   mutate(cover_id = seq_along(cover_event_id)) %>%
# #   select(cover_id, cover_event_id, cover_type_id, cover_amt) %>%
# #   as.data.frame()
# 
# # cover types to pg
# # write cover type data to pg
# if (dbExistsTable(pg, c('urbancndep', 'cover_types'))) dbRemoveTable(pg, c('urbancndep', 'cover_types')) # make sure tbl does not exist
# dbWriteTable(pg, c('urbancndep', 'cover_types'), value = cover_types, row.names = F)
# 
# # change owner to urbancndep before writing to stego
# # update events table as appropriate
# # careful sometimes you get an error but it still worked so check before doing anything,
# # sometimes this fails and the fix is to reload pg
# dbSendQuery(pg,
#             '
#             ALTER TABLE urbancndep.cover_types OWNER TO urbancndep;
# 
#             CREATE SEQUENCE urbancndep.cover_types_cover_type_id_seq;
# 
#             ALTER TABLE urbancndep.cover_types
#             ALTER COLUMN cover_type_id SET DEFAULT nextval(\'urbancndep.cover_types_cover_type_id_seq\'),
#             ALTER COLUMN cover_type_id SET NOT NULL,
#             ADD PRIMARY KEY(cover_type_id);
# 
#             ALTER SEQUENCE urbancndep.cover_types_cover_type_id_seq OWNER TO urbancndep;
#             ALTER SEQUENCE urbancndep.cover_types_cover_type_id_seq OWNED BY urbancndep.cover_types.cover_type_id;
# 
#             SELECT setval(\'urbancndep.cover_types_cover_type_id_seq\', mx.mx)
#             FROM (SELECT MAX(cover_type_id) AS mx FROM urbancndep.cover_types) mx;'
#             )

# cover data for publishing ----
# now that the data is in pg, go fetch it from pg as you would for future iterations
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

# spread data for ASU folks to review
annuals_comp_for_review <- annuals_composition %>%
  select(-cover_category) %>%
  spread(cover_type, cover_amount) %>%
  arrange(year, plot_id)

# change column types as appropriate
# add or omit cover_type depending on how you treat it once the 2016 are in
# lapply(names(annuals_composition), write, '~/Desktop/names', append=T)
annuals_composition <- annuals_composition %>%
  mutate(site_code = as.factor(site_code)) %>%
  mutate(treatment_code = as.factor(treatment_code)) %>%
  mutate(location_within_plot = as.factor(location_within_plot)) %>%
  mutate(cover_category = as.factor(cover_category)) %>%
  mutate(cover_type = as.factor(cover_type)) %>%
  mutate(plot_id = as.character(plot_id))

col.defs.annuals_composition <- c(
  "site_code" = "site name abbreviation",
  "plot_id" = "plot id number",
  "treatment_code" = "treatment name abbreviation",
  "location_within_plot" = "habitat within plot where probes were deployed",
  "subplot" = "subplot number (one of two replicates)",
  "date" = "date of plot assessment",
  "Year" = "year of plot assessment",
  "Collector" = "initials of technicians",
  "cover_type" = "composition of annual plant species to taxonomic resolution listed, or other plot characteristic",
  "cover_category" = "category of cover type",
  "cover_amount" = "amount of subplot covered by the cover type described as a decimal fraction")

unit.defs.annuals_composition <- list(
  "site_code" = list("DBG" = "core region, Desert Botanical Garden",
                  "MVP" = "core region, North Mountain",
                  "PWP" = "core region, Squaw (Piestewa) Peak",
                  "SME" = "core region, South Mountain Park East",
                  "SMW" = "core region, South Mountain Park West",
                  "LDP" = "east region, Lost Dutchman State Park",
                  "MCN" = "east region, McDowell Mountain Regional north",
                  "MCS" = "east region, McDowell Mountain Regional south",
                  "SRR" = "east region, Salt River Recreation Area (Tonto NF)",
                  "UMP" = "east region, Usery Mountain Regional Park",
                  "EME" = "west region, Estrella Mountain Regional Park East",
                  "EMW" = "west region, Estrella Mountain Regional Park West",
                  "SNE" = "west region, Sonoran Desert National Monument East",
                  "SNW" = "west region, Sonoran Desert National Monument West",
                  "WTM" = "west region, White Tanks Mountain Regional Park"),
  "plot_id" = "plot id number",
  "treatment_code" = c("N" = "nitrogen amendment",
                       "C1" = "control plot 1",
                       "P" = "phosphorus amendment",
                       "NP" = "nitrogen + phosphorus amendment"),
  "location_within_plot" = c("P" = "subplot features a Larrea tridentata plant",
                             "IP" = "subplot located in an interplant space"),
  "subplot" = "number",
  "date" = c(format = "YYYY-MM-DD"),
  "year" = "number", # easiest to change this to year type in the xml
  "collector" = "initials of technicians",
  "cover_type" = c(
                   'soil_crusts' = 'cover of soil crust',
                   'total_shrub_cover' = 'this is the total cover of shrub (Larrea + Ambrosia) in the plot excluding the base. Cover from shrubs is broken into two layers, the bottom layer (base) and a top layer (stems and leaves). The number in this column represents the top layer.',
                   'Ambrosia_base' = 'base of the trunk of Ambrosia plants where it enters the ground',
                   'Ambrosia_stem' = 'dead or broken Ambrosia branches that have fallen into the plot (sometimes they can take up a significant amount of plot cover)',
                   'Ambrosia_cover' = 'cover of the overstory of Ambrosia plants within the plot or overhanging the plot',
                   'Larrea_base' = 'base of the trunk of Larrea tridentata plants where it enters the ground',
                   'Larrea_cover' = 'cover of the overstory of Larrea tridentata plants within the plot or overhanging the plot',
                   'Simmondsia_chinensis_cover' = 'cover of the overstory of Simmondsia chinensis plants within the plot or overhanging the plot',
                   'Cylindropuntia_acanthocarpa_base' = 'base of the trunk of Cylindropuntia acanthocarpa plants where it enters the ground',
                   'Cylindropuntia_acanthocarpa_cover' = 'cover of the overstory of Cylindropuntia acanthocarpa plants within the plot or overhanging the plot',
                   'Cylindropuntia_bigelovii_base' = 'base of the trunk of Cylindropuntia bigelovii plants where it enters the ground',
                   'Cylindropuntia_bigelovii_cover' = 'cover of the overstory of Cylindropuntia bigelovii plants within the plot or overhanging the plot',
                   'Lycium_base' = 'base of the trunk of Lycium plants where it enters the ground',
                   'Lycium_cover' = 'cover of the overstory of Lycium plants within the plot or overhanging the plot',
                   'Krameria_grayi_base' = 'base of the trunk of Krameria grayi plants where it enters the ground',
                   'Krameria_grayi_cover' = 'cover of the overstory of Krameria grayi plants within the plot or overhanging the plot',
                   'Parkinsonia_microphylla_base' = 'base of the trunk of Parkinsonia microphylla plants where it enters the ground',
                   'Parkinsonia_microphylla_cover' = 'cover of the overstory of Krameria grayi plants within the plot or overhanging the plot',
                   'Pectocarya_heterocarpa' = 'Pectocarya heterocarpa',
                   'Pectocarya_platycarpa' = 'Pectocarya platycarpa',
                   'Pectocarya_recurvata' = 'Pectocarya recurvata',
                   'Schismus_arabicus' = 'Schismus arabicus',
                   'Amsinckia_menziesii' = 'Amsinckia menziesii',
                   'Lotus_salsuginosus' = 'Lotus salsuginosus',
                   'Bowlesia_incana' = 'Bowlesia incana',
                   'Crassula_connata' = 'Crassula connata',
                   'Cryptantha_decipiens' = 'Cryptantha decipiens',
                   'Cryptantha_barbigera' = 'Cryptantha barbigera',
                   'Daucus_pusillus' = 'Daucus pusillus',
                   'Erodium_cicutarium' = 'Erodium cicutarium',
                   'Erodium_texanum' = 'Erodium texanum',
                   'Eschscholzia_californica' = 'Eschscholzia californica',
                   'Caulanthus_lasiophyllus' = 'Caulanthus lasiophyllus',
                   'Lepidium_lasiocarpum' = 'Lepidium lasiocarpum',
                   'Lesquerella_gordonii' = 'Lesquerella gordonii',
                   'Logfia_arizonica' = 'Logfia arizonica',
                   'Lupinus_concinnus' = 'Lupinus concinnus',
                   'Lupinus_sparsiflorus' = 'Lupinus sparsiflorus',
                   'Mirabilis_bigelovii' = 'Mirabilis bigelovii',
                   'Orthocarpus_purpurascens' = 'Orthocarpus purpurascens',
                   'Parietaria_hespera' = 'Parietaria hespera',
                   'Phacelia_distans' = 'Phacelia distans',
                   'Pholistoma_auritum' = 'Pholistoma auritum',
                   'Plagiobothrys_arizonicus' = 'Plagiobothrys arizonicus',
                   'Plantago_ovata' = 'Plantago ovata',
                   'Plantago_patagonica' = 'Plantago patagonica',
                   'Salvia_columbariae' = 'Salvia columbariae',
                   'Sisymbrium_irio' = 'Sisymbrium irio',
                   'Vulpia_octoflora' = 'Vulpia octoflora',
                   'Herniaria_hirusta' = 'Herniaria hirusta',
                   'Chaenactis_stevioides' = 'Chaenactis stevioides',
                   'Rafinesquia_neomexicana' = 'Rafinesquia neomexicana',
                   'Descurainia_pinnata' = 'Descurainia pinnata',
                   'Eriastrum_diffusum' = 'Eriastrum diffusum',
                   'Cryptantha_pterocarya' = 'Cryptantha pterocarya',
                   'Mentzelia_affinis' = 'Mentzelia affinis',
                   'Sonchus_oleraceus' = 'Sonchus oleraceus',
                   'Poa_bigelovii' = 'Poa bigelovii',
                   'Lotus_humistratus' = 'Lotus humistratus',
                   'Silene_antirrhina' = 'Silene antirrhina',
                   'Calandrinia_ciliata' = 'Calandrinia ciliata',
                   'Conyza_canadensis' = 'Conyza canadensis',
                   'Bromus_rubens' = 'Bromus rubens',
                   'Lasthenia_californica' = 'Lasthenia californica',
                   'Houstonia_nama' = 'Houstonia nama',
                   'Dichelostemma_capitatum' = 'Dichelostemma capitatum',
                   'Eriophyllum_lanosum' = 'Eriophyllum lanosum',
                   'Draba_cuneifolia' = 'Draba cuneifolia',
                   'Astragalus_nuttallianus' = 'Astragalus nuttallianus',
                   'Eucrypta_chrysamthemifolia' = 'Eucrypta chrysamthemifolia',
                   'Chorizanthe' = 'Chorizanthe',
                   'Chorizanthe_brevicornu' = 'Chorizanthe brevicornu',
                   'Chorizanthe_rigida' = 'Chorizanthe rigida',
                   'Linanthus_demissus' = 'Linanthus demissus',
                   'Linanthus_bigelovii' = 'Linanthus bigelovii',
                   'Chamaesyce_albomarginata' = 'Chamaesyce albomarginata',
                   'Eriochloa_aristrata' = 'Eriochloa aristrata',
                   'Plantago_rotundofilia' = 'Plantago rotundofilia',
                   'Baileya_multiradiata' = 'Baileya multiradiata',
                   'Chamaesyce_polycarpa' = 'Chamaesyce polycarpa',
                   'Cryptantha_angustifolia' = 'Cryptantha angustifolia',
                   'Cryptantha_maritima' = 'Cryptantha maritima',
                   'Eriastrum_eremicum' = 'Eriastrum eremicum',
                   'Gilia_stellata' = 'Gilia stellata',
                   'Hordeum_murinum' = 'Hordeum murinum',
                   'Minuartia_douglasii' = 'Minuartia douglasii',
                   'Nemacladus_rigidus' = 'Nemacladus rigidus',
                   'Stylocline_micropoides' = 'Stylocline micropoides',
                   'Uropappus_lindleyi' = 'Uropappus lindleyi',
                   'Vulpia_microstachys' = 'Vulpia microstachys',
                   'Lotus_strigosus' = 'Lotus strigosus',
                   'Oncosiphon_piluliferum' = 'Oncosiphon piluliferum',
                   'Orobanche_cooperi' = 'Orobanche cooperi',
                   'Loeflingia_squarrosa' = 'Loeflingia squarrosa',
                   'Stylocline_gnaphaloides' = 'Stylocline gnaphaloides',
                   'Cylindropuntia_acanthocarpa' = 'Cylindropuntia acanthocarpa',
                   'Ditaxis_lanceolata' = 'Ditaxis lanceolata',
                   'Ditaxis_neomexicana' = 'Ditaxis neomexicana',
                   'Boraginaceae' = 'Boraginaceae',
                   'Aristida_adsencionis' = 'Aristida adsencionis',
                   'Eriogonum_thomasii' = 'Eriogonum thomasii',
                   'Brassicaceae' = 'Brassicaceae',
                   'Linanthus' = 'Linanthus',
                   'Polemoniaceae' = 'Polemoniaceae',
                   'Astragalus' = 'Astragalus',
                   'Chamaesyce' = 'Chamaesyce',
                   'Pectocarya' = 'Pectocarya',
                   'unidentified' = 'unidentified',
                   'Cryptantha' = 'Cryptantha'),
  "cover_category" = c("annual" = "annual plant",
                       "plot characteristic" = "measured variable other than the composition of annual plants"),
  "cover_amount" = "dimensionless")

comp_desc <- 'Composition of annual plants and some other characteristics (e.g., bare soil, base or canopy of perennial plants) at subplots within Desert Fertilization study plots. One-meter subplots include locations around a Larrea tridentata plant and locations in the interplant space between shrubs. Estimates are based on 0.25 square meter quadrats within each subplot. All measurements occur during the spring.'
annuals_composition.DT <- createtbl(annuals_composition, comp_desc)

# annuals biomass ----
# identify in drive
bio_08 <- gs_title('CNdep_Spring_08_AnnualBiomassFINAL_1_21_08.xls')
bio_09 <- gs_title('CNdep_Spring_09_AnnualBiomass.xls')
bio_10 <- gs_title('Spring 2010 CNDepBiomassFinal Calculations__mks.xlsx')
bio_13 <- gs_title('CNDep_Biomass2013_Final.xlsx')
bio_13 <- gs_title('CNDep_Biomass2013_Final.xlsx')
bio_15 <- gs_title('CNDep_SpringAnnualBiomassSampling_2015_wdata.xlsx')
bio_16 <- gs_title('CNDep_SpringAnnualBiomassSampling_2016_wdata.xlsx')

# read from drive
bio08 <-gs_read(bio_08, ws = "Raw data")
bio09 <-gs_read(bio_09, ws = "Raw data")
bio10 <-gs_read(bio_10, ws = "Summary")
bio13 <-gs_read(bio_13, ws = "Final Data")
bio15 <-gs_read(bio_15, ws = "2015_Data")
bio16 <-gs_read(bio_16, ws = "Data Entry")

# modify the 2008 data
bio08 <- inner_join(bio08, plotdetails[c('plot', 'treatment')], by = c("Plot" = "plot"))
bio08 <- bio08 %>%
  mutate(date = as.Date(Date, format = "%m/%d/%Y")) %>%
  mutate(year = 2008) %>%
  mutate(subquad_orientation = NA) %>%
  select(site_code = Site, plot_id = Plot, treatment_code = treatment, location_within_plot = Patch, replicate = Rep, subquad_orientation, date, year, mass = `Dry Wgt g/ 0.25 m2`, notes = Notes)

# modify the 2009 data
bio09 <- inner_join(bio09, plotdetails[c('plot', 'treatment')], by = c("Plot" = "plot"))
bio09 <- bio09 %>%
  mutate(date = as.Date(NA)) %>%
  mutate(year = 2009) %>%
  mutate(subquad_orientation = NA) %>%
  select(site_code = Site, plot_id = Plot, treatment_code = treatment, location_within_plot = Patch, replicate = Rep, subquad_orientation, date, year, mass = `Dry Wgt 0.25 m2`, notes = Notes)

# modify the 2010 data
bio10 <- inner_join(bio10, plotdetails[c('plot', 'treatment')], by = c("Plot" = "plot"))
bio10 <- bio10 %>%
  mutate(date = NA) %>%
  mutate(year = as.Date(2010)) %>%
  mutate(subquad_orientation = NA) %>%
  select(site_code = Site, plot_id = Plot, treatment_code = treatment, location_within_plot = Subplot, replicate = Rep, subquad_orientation, date, year, mass = Weight, notes = Notes)

# modify the 2013 data
bio13[bio13 == "?"]<-NA # convert question marks that Elizabeth has for unknown to NA
bio13 <- inner_join(bio13, plotdetails[c('plot', 'treatment')], by = c("Plots" = "plot"))
bio13 <- bio13 %>%
  mutate(date = as.Date(`Date Biomass Collected`, format = "%m/%d/%Y")) %>%
  mutate(year = 2013) %>%
  mutate(X4 = as.integer(str_extract(X4, "[[:digit:]]"))) %>%
  mutate(notes = NA) %>%
  select(site_code = Site, plot_id = Plots, treatment_code = treatment, location_within_plot = X3, replicate = X4, subquad_orientation = `Subquadrat Orientation (N, S, E, W)`, date, year, mass = `Aboveground Dry Mass (g) - Bag Mass (g)`, notes) %>%
  mutate(subquad_orientation = replace(subquad_orientation, subquad_orientation == "P", NA))

# modify the 2015 data
bio15 <- inner_join(bio15, plotdetails[c('plot', 'treatment')], by = c("Plots" = "plot"))
bio15 <- bio15 %>%
  mutate(`Aboveground Dry Mass (g) - Bag Mass (g)` = replace(`Aboveground Dry Mass (g) - Bag Mass (g)`, `Aboveground Dry Mass (g) - Bag Mass (g)` == "*", NA)) %>%
  mutate(`Aboveground Dry Mass (g) - Bag Mass (g)` = as.numeric(`Aboveground Dry Mass (g) - Bag Mass (g)`)) %>%
  filter(!is.na(`Collection Date`)) %>%
  mutate(date = as.Date(`Collection Date`, format = "%m/%d/%Y")) %>%
  mutate(year = 2015) %>%
  mutate(X5 = as.integer(str_extract(X5, "[[:digit:]]"))) %>%
  select(site_code = Site, plot_id = Plots, treatment_code = treatment, location_within_plot = X4, replicate = X5, subquad_orientation = `Subquadrat Orientation (N, S, E, W)`, date, year, mass = `Aboveground Dry Mass (g) - Bag Mass (g)`, notes = NOTES)

# modify the 2016 data
bio16 <- inner_join(bio16, plotdetails[c('plot', 'treatment')], by = c("Trmt Plots" = "plot"))
bio16 <- bio16 %>%
  mutate(date = as.Date(`Collection Date`, format = "%d-%b-%y")) %>%
  mutate(year = 2016) %>%
  select(site_code = Site, plot_id = `Trmt Plots`, treatment_code = treatment, location_within_plot = `Sub-plots`, replicate = X6, subquad_orientation = `Subquadrat Orientation (N, S, E, W)`, date, year, mass = `Aboveground Dry Mass (g) - Bag Mass (g)`, notes = NOTES)

# bind all
annuals_biomass <- bind_rows(bio08, bio09, bio10, bio13, bio15, bio16) %>%
  mutate(site_code = as.factor(site_code)) %>%
  mutate(treatment_code = as.factor(treatment_code)) %>%
  mutate(location_within_plot = as.factor(location_within_plot)) %>%
  mutate(subquad_orientation = as.factor(subquad_orientation))

# prepare data for database insertion
# the gist here is that there are no more than two data points per event
# where event is a plot/date combination, and the two data points being
# mass values from each of the two replicates. As such, I think a single
# table is appropriate in this case. This could be a two-table structure
# consisting of (1) events and (2) replicates with values, and, really,
# that would be the best form from a DB standpoint but I think not worth
# the overhead
annuals_biomass_db <- annuals_biomass %>%
  mutate(ann_biomass_id = seq_along(site_code)) %>%
  select(ann_biomass_id, plot_id, location_within_plot:notes) %>%
  as.data.frame()

if (dbExistsTable(pg, c('urbancndep', 'annuals_biomass'))) dbRemoveTable(pg, c('urbancndep', 'annuals_biomass')) # make sure tbl does not exist
dbWriteTable(pg, c('urbancndep', 'annuals_biomass'), value = annuals_biomass_db, row.names = F)

# change owner to urbancndep before writing to stego
# update events table as appropriate
# careful sometimes you get an error but it still worked so check before doing anything,
# sometimes this fails and the fix is to reload pg
dbSendQuery(pg,
            '
            ALTER TABLE urbancndep.annuals_biomass OWNER TO urbancndep;

            CREATE SEQUENCE urbancndep.annuals_biomass_ann_biomass_id_seq;

            ALTER TABLE urbancndep.annuals_biomass
            ALTER COLUMN ann_biomass_id SET DEFAULT nextval(\'urbancndep.annuals_biomass_ann_biomass_id_seq\'),
            ALTER COLUMN ann_biomass_id SET NOT NULL;

            ALTER SEQUENCE urbancndep.annuals_biomass_ann_biomass_id_seq OWNER TO urbancndep;
            ALTER SEQUENCE urbancndep.annuals_biomass_ann_biomass_id_seq OWNED BY urbancndep.annuals_biomass.ann_biomass_id;

            SELECT setval(\'urbancndep.annuals_biomass_ann_biomass_id_seq\', mx.mx)
            FROM (SELECT MAX(ann_biomass_id) AS mx FROM urbancndep.annuals_biomass) mx;'
            )

# pull from database for future publications of these data
# CAREFUL, this was done after the write to the DB so do not use annuals_biomass again
# without recreating it
annuals_biomass <- annuals_biomass %>% mutate(plot_id = as.character(plot_id))

col.defs.annuals_biomass <- c(
  "site_code" = "site name abbreviation",
  "plot_id" = "plot id number",
  "treatment_code" = "treatment name abbreviation",
  "location_within_plot" = "habitat within plot where probes were deployed",
  "replicate" = "sample replicate number",
  "subquad_orientation" = "direction of subquadrat from which biomass was harvested",
  "date" = "date of biomass harvest",
  "year" = "year of biomass harvest",
  "mass" = "mass of biomass harvested in 0.25 meterSquare quadrat",
  "notes" = "field and_or processing notes")

unit.defs.annuals_biomass <- list(
  "site_code" = c("DBG" = "core region, Desert Botanical Garden",
                  "MVP" = "core region, North Mountain",
                  "PWP" = "core region, Squaw (Piestewa) Peak",
                  "SME" = "core region, South Mountain Park East",
                  "SMW" = "core region, South Mountain Park West",
                  "LDP" = "east region, Lost Dutchman State Park",
                  "MCN" = "east region, McDowell Mountain Regional north",
                  "MCS" = "east region, McDowell Mountain Regional south",
                  "SRR" = "east region, Salt River Recreation Area (Tonto NF)",
                  "UMP" = "east region, Usery Mountain Regional Park",
                  "EME" = "west region, Estrella Mountain Regional Park East",
                  "EMW" = "west region, Estrella Mountain Regional Park West",
                  "SNE" = "west region, Sonoran Desert National Monument East",
                  "SNW" = "west region, Sonoran Desert National Monument West",
                  "WTM" = "west region, White Tanks Mountain Regional Park"),
  "plot_id" = "plot id number",
  "treatment_code" = c("N" = "nitrogen amendment",
                       "C1" = "control plot 1",
                       "P" = "phosphorus amendment",
                       "NP" = "nitrogen + phosphorus amendment"),
  "location_within_plot" = c("P" = "subplot features a Larrea tridentata plant",
                             "IP" = "subplot located in an interplant space"),
  "replicate" = "number",
  "subquad_orientation" = c("N" = "north",
                            "W" = "west",
                            "E" = "east",
                            "S" = "south"),
  "date" = c(format = "YYYY-MM-DD"),
  "year" = "number", # change this in the eml
  "mass" = "gram",
  "notes" = "field and_or processing notes")

bio_desc <- 'Biomass (g) of annual plants harvested from subplots within Desert Fertilization study plots. One-meter subplots include locations around a Larrea tridentata plant and locations in the interplant space between shrubs. Material is harested from 0.25 square meter quadrats within each subplot. All harvests occur during the spring.'
annuals_biomass.DT <- createtbl(annuals_biomass, bio_desc)

# biovolume ----

# get the data from googledrive
biovol_reg <- gs_title('CNdep_biomass_measurements_spr2012')
biovol_sheets <- gs_ws_ls(biovol_reg)
plot_52 <- gs_read(biovol_reg, ws = 1, skip = 1)
plot_60 <- gs_read(biovol_reg, ws = 2, skip = 1)
plot_56 <- gs_read(biovol_reg, ws = 3, skip = 1)
plot_26 <- gs_read(biovol_reg, ws = 4, skip = 1)
plot_29 <- gs_read(biovol_reg, ws = 5, skip = 1)
plot_5 <- gs_read(biovol_reg, ws = 6, skip = 1)
plot_1 <- gs_read(biovol_reg, ws = 7, skip = 1)
plot_64 <- gs_read(biovol_reg, ws = 8, skip = 1)
plot_65 <- gs_read(biovol_reg, ws = 9, skip = 1)

# add plot and date to each dataset
expand <- function(sheet, plotnum) {
  samdate <- gs_read(biovol_reg, ws = sheet, range = "A1:A1", col_names = 'date')
  tempframe <- data.frame(plot = plotnum, samdate)
  newframe <- cbind(tempframe, get(paste0("plot_", plotnum)))
  return(newframe)
  }

plot_52 <- expand(1, 52)
plot_60 <- expand(2, 60)
plot_56 <- expand(3, 56)
plot_26 <- expand(4, 26)
plot_29 <- expand(5, 29)
plot_5  <- expand(6, 5)
plot_1  <- expand(7, 1)
plot_64 <- expand(8, 64)
plot_65 <- expand(8, 65)

# adjust extra column in plot 64 data
plot_64 <- plot_64 %>% mutate(notes = replace(notes, !is.na(X9), 'looks dead')) %>%
  select(-X9)

# adjust non-numeric data
plot_5 <- plot_5 %>% mutate(N = as.numeric(N))
plot_64 <- plot_64 %>% mutate(E = as.numeric(E))

# adjust names in 5 to match 65
plot_5 <- plot_5 %>% rename(Top = up) %>%
  rename(Base = down)

# bind to single DF
biovolume <- bind_rows(plot_1, plot_26, plot_29, plot_5, plot_52, plot_56, plot_60, plot_64, plot_65) %>%
  mutate(quadrant = na.locf(X1)) %>% # fill quadrants for all rows
  mutate(notes = ifelse(grepl("location", X1), X1, notes)) %>% # copy notes that are in old quadrant column
  filter(!is.na(Plant)) %>% # filter non-data
  mutate(sample_date = as.POSIXct(date, format = "%m/%d/%Y")) %>% # format date
  select(plot, quadrant, sample_date, Plant, N_S = `N/S`, E_W = `E/W`, H:Base) %>% # pare to relevant cols
  mutate(plot = as.character(plot)) %>%
  rename(distance = `Distance(m)`) %>%
  mutate(Plant = replace(Plant, Plant == "AMDE", "Ambrosia deltoidea")) %>%
  mutate(Plant = replace(Plant, Plant == "MADE", "Ambrosia deltoidea")) %>%
  mutate(Plant = replace(Plant, Plant == "AMDU", "Ambrosia dumosa")) %>%
  mutate(Plant = replace(Plant, Plant == "LATR", "Larrea tridentata")) %>%
  mutate(Plant = replace(Plant, Plant == "ENFA", "Encelia farinosa")) %>%
  mutate(Plant = replace(Plant, grepl("uro", Plant), "Carnegiea gigantea")) %>%
  mutate(Plant = replace(Plant, grepl("A[0-9]", Plant), "Ambrosia spp.")) %>%
  mutate(Plant = replace(Plant, grepl("L[0-9]", Plant), "Larrea tridentata"))

# site plot trt detail
plotdetails <- dbGetQuery(pg,
'SELECT
  p.id as plot,
  s.code as site_code,
  s.name as site_name,
  t.code as treatment,
  t.description as treatment_description
FROM urbancndep.plots p
  JOIN urbancndep.sites s ON (p.site_id = s.id)
  JOIN urbancndep.treatments t ON (p.treatment_id = t.id)
;')

# merge with other site details and order cols
biovolume <- join(biovolume, plotdetails, by = c("plot" = "plot")) %>%
  select(site_code, treatment, plot:Base) %>%
  mutate(site_code = as.factor(site_code)) %>%
  mutate(treatment = as.factor(treatment)) %>%
  mutate(quadrant = as.factor(quadrant))

biovolume <- biovolume %>% rename(treatment_code = treatment)

col.defs.biovolume <- c(
  "site_code" = "site name abbreviation",
  "treatment_code" = "treatment name abbreviation",
  "plot" = "plot id number",
  "quadrant" = "plot quadrant in which plant is located",
  "sample_date" = "measurement date",
  "Plant" = "type of plant",
  "N_S" = "maximum extent of plant along N-S axis",
  "E_W" = "maximum extent of plant along E-W axis",
  "H" = "maximum height of plant",
  "N" = "relative distance north from reference plot corner",
  "E" = "relative distance east from reference plot corner",
  "notes" = "notes",
  "distance" = "for tall plants (some trees, Saguaros), the distance from the base of the plant to the point of measurement",
  "Top" = "for tall plants (some trees, Saguaros), degrees to the apex of the plant",
  "Base" = "for tall plants (some trees, Saguaros), degrees to the base of the plant")

unit.defs.biovolume <- list(
  "site_code" = c("LDP" = "Lost Dutchman State Park",
                  "SME" = "South Mountain East",
                  "SNE" = "Sonoran National Monument East",
                  "SNW" = "Sonoran National Monument West",
                  "EMW" = "Estrella Mountain West"),
  "treatment_code" = c("N" = "nitrogen amendment",
                  "C1" = "control plot 1"),
  "plot" = "plot id number",
  "quadrant" = c("Q1" = "southwest quadrant of plot",
                 "Q2" = "northwest quadrant of plot",
                 "Q3" = "northeast quadrant of plot",
                 "Q4" = "southeast quadrant of plot"),
  "sample_date" = c(format = "YYYY-MM-DD"),
  "Plant" = "type of plant",
  "N_S" = "meter",
  "E_W" = "meter",
  "H" = "meter",
  "N" = "meter",
  "E" = "meter",
  "notes" = "notes",
  "distance" = "meter",
  "Top" = "degree",
  "Base" = "degree")

# generate datatables
# slotNames(new('dataTable'))

biovol_desc <- 'Inventory and biovolume of all plants at select Desert Fertilization Experiment plots measured in spring 2012.'
biovolume.DT <- createtbl(biovolume, biovol_desc)

# fertilizer ----

# database cleanup, here as SQL statements, run in PG....this is done (run in PG, here for ref only)
# DELETE FROM urbancndep.fertilizer_applications
# WHERE id = 101;
#
# DELETE FROM urbancndep.fertilizer_applications
# WHERE EXTRACT (YEAR FROM date) = 1970;
#
# UPDATE urbancndep.fertilizer_applications
# SET "N"=3.429;
#
# UPDATE urbancndep.fertilizer_applications
# SET "P"=1.224
# WHERE date > '2009-07-01' AND "P" = 12;
#
# UPDATE urbancndep.fertilizer_applications
# SET "N_and_P" = "N" || ' & ' || "P";

# fert data
fertilizer_application <- dbGetQuery(pg,
'SELECT
  s.code as site_code,
  f.date as application_date,
  f."N" as nitrogen,
  f."P" as phosphorus
FROM urbancndep.fertilizer_applications f
  JOIN urbancndep.sites s ON (f.site_id = s.id)
ORDER BY f.date, s.code
;')

fertilizer_application <- fertilizer_application  %>%
  mutate(site_code = as.factor(site_code))

rows <- ncol(fertilizer_application)
fert_attrs <- data.frame(attributeName = character(rows),
                         formatString = character(rows),
                         unit = character(rows),
                         numberType = character(rows),
                         definition = character(rows),
                         attributeDefinition = character(rows),
                         columnClasses = character(rows),
                         minimum = character(rows),
                         maximum = character(rows),
                         stringsAsFactors = FALSE)

fert_attrs$attributeName <- names(fertilizer_application) # attributeNames
fert_attrs$columnClasses <- sapply(fertilizer_application, class) # variable types

# write attribute df to file, edit in editor, then read back to workspace, sorta
# blasphemous but really easy and possibly a good format for future data
# submissions
write.csv(fert_attrs, file = "fert_attrs.csv", row.names = FALSE) # write and edit in editor
fert_attrs <- read.csv("fert_attrs.csv", header = TRUE, sep = ",", quote = "\"", as.is = TRUE)

fert_classes <- fert_attrs[,"columnClasses"] # get the column classes into a vector as required by the set_attribute function

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

fert_factors <- rbind(
  data.frame(
    attributeName = "site_code",
    code = names(site_code),
    definition = unname(site_code)
  ))

fert_desc <- 'catalog of amounts and timing of nitrogen and phosphorus fertilizer applications to nitrogen (N), phosphorus (P), and nitrogen+phosphorus (N+P) treatment plots'
fertilizer_application_DT <- createDT(dfname = fertilizer_application,
                                      attributes = fert_attrs,
                                      factors = fert_factors,
                                      classes = fert_classes,
                                      description = fert_desc)

# using old version of REML
# fertilizer_application.DT <- createtbl(fertilizer_application, fert_desc)
#
# col.defs.fertilizer_application <- c(
#   "site_code" = "site name abbreviation",
#   "application_date" = "application date",
#   "nitrogen" = "amount of ammonium-nitrate nitrogen fertilizer applied to 20mx20m nitrogen (N) and nitrogen+phosphorus (N+P) treatment plots",
#   "phosphorus" = "amount of triple-super phosphate phosphorus fertilizer applied to 20m*20m phosphorus (P) and nitrogen+phosphorus (N+P) treatment plots")
#
# unit.defs.fertilizer_application <- list(
#   "site_code" = c("DBG" = "core region, Desert Botanical Garden",
#                   "MVP" = "core region, North Mountain",
#                   "PWP" = "core region, Squaw (Piestewa) Peak",
#                   "SME" = "core region, South Mountain Park East",
#                   "SMW" = "core region, South Mountain Park West",
#                   "LDP" = "east region, Lost Dutchman State Park",
#                   "MCN" = "east region, McDowell Mountain Regional north",
#                   "MCS" = "east region, McDowell Mountain Regional south",
#                   "SRR" = "east region, Salt River Recreation Area (Tonto NF)",
#                   "UMP" = "east region, Usery Mountain Regional Park",
#                   "EME" = "west region, Estrella Mountain Regional Park East",
#                   "EMW" = "west region, Estrella Mountain Regional Park West",
#                   "SNE" = "west region, Sonoran Desert National Monument East",
#                   "SNW" = "west region, Sonoran Desert National Monument West",
#                   "WTM" = "west region, White Tanks Mountain Regional Park"),
#   "application_date" = c(format = "YYYY-MM-DD"),
#   "nitrogen" = "kilogram",
#   "phosphorus" = "kilogram")
#
# fert_desc <- 'catalog of amounts and timing of nitrogen and phosphorus fertilizer applications to nitrogen (N), phosphorus (P), and nitrogen+phosphorus (N+P) treatment plots'
# fertilizer_application.DT <- createtbl(fertilizer_application, fert_desc)

# soil pH ----

soil_ph <- dbGetQuery(pg,
'SELECT
  s.code AS site_code,
  p.id AS plot_id,
  t.code AS treatment_code,
  sps.date AS sample_date,
  rl.location_code,
  sps.soil_ph,
  sps.temperature,
  sps.slope,
  sps.processing_notes
FROM urbancndep.sites s
  JOIN urbancndep.plots p ON s.id = p.site_id
  JOIN urbancndep.treatments t ON p.treatment_id = t.id
  JOIN urbancndep.soil_ph_samples sps ON p.id = sps.plot_id
  JOIN urbancndep.resin_locations rl ON sps.location_id = rl.location_id
ORDER BY sps.date;')

soil_ph <- soil_ph %>%
  mutate(site_code = as.factor(site_code)) %>%
  mutate(treatment_code = as.factor(treatment_code)) %>%
  mutate(location_code = as.factor(location_code))

soil_ph <- soil_ph %>% mutate(plot_id = as.character(plot_id))

col.defs.soil_ph <- c(
  "site_code" = "site name abbreviation",
  "plot_id" = "plot id number",
  "treatment_code" = "treatment name abbreviation",
  "sample_date" = "date of soil collection",
  "location_code" = "soil sampling habitat within study plot",
  "soil_ph" = "soil pH value",
  "temperature" = "temperature of solution",
  "slope" = "slope of calibration curve",
  "processing_notes" = "processing_notes")

unit.defs.soil_ph <- list(
  "site_code" = c("DBG" = "core region, Desert Botanical Garden",
                  "MVP" = "core region, North Mountain",
                  "PWP" = "core region, Squaw (Piestewa) Peak",
                  "SME" = "core region, South Mountain Park East",
                  "SMW" = "core region, South Mountain Park West",
                  "LDP" = "east region, Lost Dutchman State Park",
                  "MCN" = "east region, McDowell Mountain Regional north",
                  "MCS" = "east region, McDowell Mountain Regional south",
                  "SRR" = "east region, Salt River Recreation Area (Tonto NF)",
                  "UMP" = "east region, Usery Mountain Regional Park",
                  "EME" = "west region, Estrella Mountain Regional Park East",
                  "EMW" = "west region, Estrella Mountain Regional Park West",
                  "SNE" = "west region, Sonoran Desert National Monument East",
                  "SNW" = "west region, Sonoran Desert National Monument West",
                  "WTM" = "west region, White Tanks Mountain Regional Park"),
  "plot_id" = "plot id number",
  "treatment_code" = c("N" = "nitrogen amendment",
                       "C1" = "control plot 1",
                       "P" = "phosphorus amendment",
                       "NP" = "nitrogen + phosphorus amendment"),
  "sample_date" = c(format = "YYYY-MM-DD"),
  "location_code" = c("LATR" = "beneath Larrea tridentata",
                      "IP" = "between plants"),
  "soil_ph" = "dimensionless",
  "temperature" = "celsius",
  "slope" = "dimensionless",
  "processing_notes" = "processing_notes")

ph_desc <- 'Soil pH in treatment plots, assessed following extraction in 30 mL 0.1 M CaCl2'
soil_ph.DT <- createtbl(soil_ph, ph_desc)

# PRS ----

plant_root_simulator <- dbGetQuery(pg,
'SELECT
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

# standardize names
plant_root_simulator <- plant_root_simulator %>%
  mutate(analyte = replace(analyte, analyte == 'Total N', 'Total-N')) %>%
  mutate(location_within_plot = replace(location_within_plot, location_within_plot == 'under plant', 'under_plant')) %>%
  mutate(location_within_plot = replace(location_within_plot, location_within_plot == 'between plants', 'between_plant')) %>%
  mutate(location_within_plot = replace(location_within_plot, location_within_plot == 'between plant', 'between_plant')) %>%
  mutate(location_within_plot = replace(location_within_plot, location_within_plot == 'BLK-WASHED', 'blank')) %>%
  mutate(location_within_plot = replace(location_within_plot, location_within_plot == 'blk-washed', 'blank')) %>%
  mutate(location_within_plot = replace(location_within_plot, location_within_plot == 'BLANK', 'blank')) %>%
  filter(!grepl("NH4", location_within_plot))

plant_root_simulator <- plant_root_simulator %>%
  mutate(site_code = as.factor(site_code)) %>%
  mutate(treatment_code = as.factor(treatment_code)) %>%
  mutate(analyte = as.factor(analyte)) %>%
  mutate(location_within_plot = as.factor(location_within_plot))

plant_root_simulator <- plant_root_simulator %>% mutate(plot_id = as.character(plot_id))

col.defs.plant_root_simulator <- c(
  "site_code" = "site name abbreviation",
  "plot_id" = "plot id number",
  "treatment_code" = "treatment name abbreviation",
  "start_date" = "date of probe deployment",
  "end_date" = "date of probe extraction",
  "analyte" = "analyte measured",
  "final_value" = "analyte concentration",
  "flag" = "data quality flag",
  "location_within_plot" = "habitat within plot where probes were deployed",
  "num_cation_probes" = "number of probes designated for analysis of cations recovered (of four)",
  "num_anion_probes" = "number of probes designated for analysis of anions recovered (of four)")

unit.defs.plant_root_simulator <- list(
  "site_code" = c("DBG" = "core region, Desert Botanical Garden",
                  "MVP" = "core region, North Mountain",
                  "PWP" = "core region, Squaw (Piestewa) Peak",
                  "SME" = "core region, South Mountain Park East",
                  "SMW" = "core region, South Mountain Park West",
                  "LDP" = "east region, Lost Dutchman State Park",
                  "MCN" = "east region, McDowell Mountain Regional north",
                  "MCS" = "east region, McDowell Mountain Regional south",
                  "SRR" = "east region, Salt River Recreation Area (Tonto NF)",
                  "UMP" = "east region, Usery Mountain Regional Park",
                  "EME" = "west region, Estrella Mountain Regional Park East",
                  "EMW" = "west region, Estrella Mountain Regional Park West",
                  "SNE" = "west region, Sonoran Desert National Monument East",
                  "SNW" = "west region, Sonoran Desert National Monument West",
                  "WTM" = "west region, White Tanks Mountain Regional Park"),
  "plot_id" = "plot id number",
  "treatment_code" = c("N" = "nitrogen amendment",
                       "C1" = "control plot 1",
                       "C2" = "control plot 2",
                       "P" = "phosphorus amendment",
                       "NP" = "nitrogen + phosphorus amendment"),
  "start_date" = c(format = "YYYY-MM-DD"),
  "end_date" = c(format = "YYYY-MM-DD"),
  "analyte" = c("NH4-N" = "ammonium-nitrogen",
                "Total-N" = "sum of NO3-N and NH4-N",
                "NO3-N" = "nitrate-nitrogen",
                "Zn" = "zinc",
                "Al" = "aluminum",
                "Cu" = "copper",
                "Mn" = "Manganese",
                "Pb" = "lead",
                "Fe" = "iron",
                "P" = "phosphorus",
                "S" = "sulfur",
                "K" = "potassium",
                "Cd" = "cadmium",
                "Mg" = "magnesium",
                "B" = "boron",
                "Ca" = "calcium"),
  "final_value" = "microgramPerTenSquareCentimeterPerBurialLength",
  "flag" = "data quality flag",
  "location_within_plot" = c("under_plant" = "probes buried beneath the canopy of Larrea tridentata",
                             "between_plant" = "probes buried between plant canopies",
                             "washed blank" = "field blank (washed): probe stored in a clean container under refrigeration during the deployment period, and washed as per sample probes",
                             "blank" = "field blank: probe stored in a clean container under refrigeration during the deployment period"),
  "num_cation_probes" = "number",
  "num_anion_probes" = "number")

# custom unit
microgramPerTenSquareCentimeterPerBurialLength <- eml_define_unit(id = "microgramPerTenSquareCentimeterPerBurialLength",
                                  parentSI = "NA",
                                  unitType = "areal_mass_flux",
                                  multiplierToSI = "NA",
                                  description = "net rate of nutrient ion adsorption by the PRS® Probe expressed as the weight of nutrient adsorbed per surface area of ion-exchange membrane over time")

prs_desc <- 'Soil ion concentrations as determined with Plant Root Simulator (PRS®) probes (ion exchange resin membranes). Probes for the analysis of soil anions have a positively-charged membrane to simultaneously attract and adsorb all negatively-charged anions, such as nitrate (NO3-), phosphate (H2PO4-, HPO42-), and sulphate (SO42-), whereas cation probes have a negatively-charged membrane to simultaneously attract and adsorb all positively-charged cations, such as ammonium (NH4+), potassium (K+), calcium (Ca2+), and magnesium (Mg2+).'
plant_root_simulator.DT <- createtbl(plant_root_simulator, prs_desc)

# stems ----
# note that the two where clause statements are to omit duplicate entries for
# those two sites and dates. In each case, the pre-dates are incorrect but
# there are entries with the correct pre-dates
stem_growth <- dbGetQuery(pg,
"SELECT
    s.code AS site_code,
    p.id AS plot_id,
    t.code AS treatment_code,
    sp.scientific_name,
    sh.code AS shrub_code,
    sh.note AS shrub_note,
    st.direction,
    st.pre_date,
    st.post_date,
    st.post_note,
    sl.post_measurement,
    sl.length_in_mm as stem_length
FROM urbancndep.sites s
   JOIN urbancndep.plots p ON s.id = p.site_id
   JOIN urbancndep.treatments t ON p.treatment_id = t.id
   JOIN urbancndep.shrubs sh ON p.id = sh.plot_id
   JOIN urbancndep.shrub_species sp ON sh.shrub_species_id = sp.id
   JOIN urbancndep.stems st ON sh.id = st.shrub_id
   JOIN urbancndep.stem_lengths sl ON st.id = sl.stem_id
WHERE
   NOT (p.id = 13 AND st.pre_date = '2010-05-10') AND
   NOT (p.id = 12 AND st.pre_date = '2010-05-11')
ORDER BY st.pre_date, p.id, sl.post_measurement, sh.code, st.direction;")

stem_growth <- stem_growth %>%
  mutate(site_code = as.factor(site_code)) %>%
  mutate(treatment_code = as.factor(treatment_code)) %>%
  mutate(shrub_code = as.factor(shrub_code)) %>%
  mutate(direction = as.factor(direction)) %>%
  mutate(plot_id = as.character(plot_id))
  # post_measurement in a logical and I have treated as a factor, let's see how it comes out

col.defs.stem_growth <- c(
  "site_code" = "site name abbreviation",
  "plot_id" = "plot id number",
  "treatment_code" = "treatment name abbreviation",
  "scientific_name" = "scientific name of plant measured",
  "shrub_code" = "study plant identifier",
  "shrub_note" = "field notes",
  "direction" = "direction or quadrant of plant in which measured stem is positioned",
  "pre_date" = "date of initial or pre stem-length measurement",
  "post_date" = "date of final or post stem-length",
  "post_note" = "field note regarding final or post stem-length measurement",
  "post_measurement" = "boolean indicator denoting whether the measurement is a pre or initial stem-length measurement (FALSE), or a final or post stem-length measurement (TRUE)",
  "stem_length" = "length of stem")

unit.defs.stem_growth <- list(
  "site_code" = c("DBG" = "core region, Desert Botanical Garden",
                  "MVP" = "core region, North Mountain",
                  "PWP" = "core region, Squaw (Piestewa) Peak",
                  "SME" = "core region, South Mountain Park East",
                  "SMW" = "core region, South Mountain Park West",
                  "LDP" = "east region, Lost Dutchman State Park",
                  "MCN" = "east region, McDowell Mountain Regional north",
                  "MCS" = "east region, McDowell Mountain Regional south",
                  "SRR" = "east region, Salt River Recreation Area (Tonto NF)",
                  "UMP" = "east region, Usery Mountain Regional Park",
                  "EME" = "west region, Estrella Mountain Regional Park East",
                  "EMW" = "west region, Estrella Mountain Regional Park West",
                  "SNE" = "west region, Sonoran Desert National Monument East",
                  "SNW" = "west region, Sonoran Desert National Monument West",
                  "WTM" = "west region, White Tanks Mountain Regional Park"),
  "plot_id" = "plot id number",
  "treatment_code" = c("N" = "nitrogen amendment",
                       "C1" = "control plot 1",
                       "P" = "phosphorus amendment",
                       "NP" = "nitrogen + phosphorus amendment"),
  "scientific_name" = "scientific name of plant measured",
  "shrub_code" = c("L1" = "Larrea tridentata study plant #1",
                   "L2" = "Larrea tridentata study plant #2",
                   "L3" = "Larrea tridentata study plant #3",
                   "L4" = "Larrea tridentata study plant #4",
                   "L5" = "Larrea tridentata study plant #5"),
  "shrub_note" = "field notes",
  "direction" = c("South" = "stem in south quadrant of plant",
                  "West" = "stem in west quadrant of plant",
                  "North" = "stem in north quadrant of plant",
                  "East" = "stem in east quadrant of plant"),
  "pre_date" = c(format = "YYYY-MM-DD"),
  "post_date" = c(format = "YYYY-MM-DD"),
  "post_note" = "field note regarding final or post stem-length measurement",
  "post_measurement" = c("TRUE" = "final / post stem-length measurement",
                         "FALSE" = "initial / pre stem-length measurement"),
  "stem_length" = "millimeter")

stems_desc <- 'regular measures of the length of stems on five Larrea tridentata study plants in Desert Fertilization experiment treatment and control plots'
stem_growth.DT <- createtbl(stem_growth, stems_desc)

# larrea icpms ----
# here we are creating a set of clean, easily-interpretable data to publish;
# will include the raw data (as unmolested .xlsm file) as an otherEntity
# instead of storing these comlicated and potentially one-off raw data in the
# database
icp <- gs_title('Cndep_ICP_MS_derived')
leaf_tissue_icpms <- gs_read(icp)

leaf_tissue_icpms <- leaf_tissue_icpms %>%
  separate(sampleid, c("site_code", "plot_id", "treatment_code"), sep = "-", remove=FALSE) %>%
  separate(date, c("month_text", "year"), sep = " ", remove=FALSE) %>%
  mutate(month = as.numeric(ifelse(grepl("oct", month_text, ignore.case = TRUE), 10, 5))) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(plot_id = as.numeric(plot_id))

forDates <- stem_growth %>% select(plot_id, treatment_code, post_date) %>%
  mutate(year = as.numeric(format(post_date, "%Y"))) %>%
  mutate(month = as.numeric(format(post_date, "%m"))) %>%
  filter(year %in% c(2009, 2010, 2013)) %>%
  filter(treatment_code == "C1") %>%
  distinct(plot_id, post_date)

leaf_tissue_icpms <- inner_join(leaf_tissue_icpms, forDates, by = c("plot_id" = "plot_id", "year" = "year", "month" = "month")) %>%
  select(site_code, plot_id, treatment_code = treatment_code.x, sample_date = post_date, `23Na`:`238U`) %>%
  gather(isotope_element, concentration, `23Na`:`238U`)

leaf_tissue_icpms <- leaf_tissue_icpms %>%
  mutate(site_code = as.factor(site_code)) %>%
  mutate(treatment_code = as.factor(treatment_code))

leaf_tissue_icpms <- leaf_tissue_icpms %>%
  mutate(plot_id = as.character(plot_id))


#### add newer leaf tissue data

# here we are specifically adding 2015 data that Marisa provided after data
# from other years had already been processed, so we are adding the 2015 data to
# the other years.

icp_new <- gs_title('CNDep_Larrea_Metals_Oct2015.xlsm')
rev_leaf_tissue <- gs_read(icp_new, cell_rows(c(1, 15:29)), ws = 1)
rev_leaf_tissue <- rev_leaf_tissue[14:28,] # remove standards values

# change the column name to reflect that it is a sample id
colnames(rev_leaf_tissue)[1] <- "sampleid"

# separate the sample id into components, add month, year, remove "processing"
# columns, remove empty columns, and rename Sulfur to just S since the 182.0 in
# this case is a wavelength unlike number for all other elements, which are
# atomic weights
rev_leaf_tissue <- rev_leaf_tissue %>%
  separate(
    col = sampleid,
    into = c("site_code", "treatment_code"),
    sep = "-",
    remove = FALSE
  ) %>%
  mutate(month = 10) %>%
  mutate(year = 2015) %>%
  select(-contains("CCT")) %>%
  select(-contains("NH3")) %>%
  select(-c(X51, X52)) %>%
  rename(S = S_182.0)

# prep data for adding dates via join
forDates2 <- stem_growth %>%
  select(plot_id, treatment_code, post_date) %>%
  mutate(year = as.numeric(format(post_date, "%Y"))) %>%
  mutate(month = as.numeric(format(post_date, "%m"))) %>%
  filter(year %in% c(2015)) %>%
  filter(treatment_code == "C1") %>%
  distinct(plot_id, post_date, .keep_all = T)

# add plot number via join
rev_leaf_tissue <-
  inner_join(
    rev_leaf_tissue,
    plotdetails[, c("plot", "site_code", "treatment")],
    by = c("site_code" = "site_code", "treatment_code" = "treatment")
  )

# add sampling dates via join, select appropriate cols, and stack
rev_leaf_tissue <-
  inner_join(rev_leaf_tissue,
             forDates2,
             by = c(
               "plot" = "plot_id",
               "year" = "year",
               "month" = "month"
             )) %>%
  select(
    site_code,
    plot_id = plot,
    treatment_code = treatment_code.x,
    sample_date = post_date,
    `11B`:`S`
  ) %>%
  gather(isotope_element, concentration, `11B`:`S`)

# change col types as appropriate
rev_leaf_tissue <- rev_leaf_tissue %>%
  mutate(site_code = as.factor(site_code)) %>%
  mutate(treatment_code = as.factor(treatment_code)) %>%
  mutate(concentration = as.numeric(concentration)) %>%
  mutate(plot_id = as.character(plot_id))

# join the Oct 2015 data with the data from the other years
leaf_tissue_icpms <- bind_rows(leaf_tissue_icpms, rev_leaf_tissue)


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# finish adding 2015 icpms data
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# used the new REML package when the 2015 data were added

#set up the data frame for metadata
rows <- ncol(leaf_tissue_icpms)
icpms_attrs <- data.frame(attributeName = character(rows),
                          formatString = character(rows),
                          unit = character(rows),
                          numberType = character(rows),
                          definition = character(rows),
                          attributeDefinition = character(rows),
                          columnClasses = character(rows),
                          minimum = character(rows),
                          maximum = character(rows),
                          stringsAsFactors = FALSE)

icpms_attrs$attributeName <- names(leaf_tissue_icpms) # attributeNames
icpms_attrs$columnClasses <- sapply(leaf_tissue_icpms, class) # variable types

# write attribute df to file, edit in editor, then read back to workspace, sorta
# blasphemous but really easy and possibly a good format for future data
# submissions
write.csv(icpms_attrs, file = "icpms_attrs.csv", row.names = FALSE) # write and edit in editor
icpms_attrs <- read.csv("icpms_attrs.csv", header = TRUE, sep = ",", quote = "\"", as.is = TRUE)

icpms_classes <- icpms_attrs[,"columnClasses"] # get the column classes into a vector as required by the set_attribute function

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

icpms_factors <- rbind(
  data.frame(
    attributeName = "site_code",
    code = names(site_code),
    definition = unname(site_code)
  ),
  data.frame(
    attributeName = "treatment_code",
    code = names(treatment_code),
    definition = unname(treatment_code)
  ))

icpms_desc <- 'Elemental composition of Larrea tridentata leaf tissue collected from control plots at Desert Fertilization study sites; analyses are by ICP-MS, except for Sulfur (S) in 2015, which was analyzed by ICP-OES'
leaf_tissue_icpms_DT <- createDT(dfname = leaf_tissue_icpms,
                                 attributes = icpms_attrs,
                                 factors = icpms_factors,
                                 classes = icpms_classes,
                                 description = icpms_desc)


# begin old reml
#
# col.defs.leaf_tissue_icpms <- c(
#   "site_code" = "site name abbreviation",
#   "plot_id" = "plot id number",
#   "treatment_code" = "treatment name abbreviation",
#   "sample_date" = "date of leaf collection",
#   "isotope_element" = "isotope atomic mass and chemical symbol",
#   "concentration" = "mass of isotope_element per mass of leaf material")
#
# unit.defs.leaf_tissue_icpms <- list(
#   "site_code" = c("DBG" = "core region, Desert Botanical Garden",
#                   "MVP" = "core region, North Mountain",
#                   "PWP" = "core region, Squaw (Piestewa) Peak",
#                   "SME" = "core region, South Mountain Park East",
#                   "SMW" = "core region, South Mountain Park West",
#                   "LDP" = "east region, Lost Dutchman State Park",
#                   "MCN" = "east region, McDowell Mountain Regional north",
#                   "MCS" = "east region, McDowell Mountain Regional south",
#                   "SRR" = "east region, Salt River Recreation Area (Tonto NF)",
#                   "UMP" = "east region, Usery Mountain Regional Park",
#                   "EME" = "west region, Estrella Mountain Regional Park East",
#                   "EMW" = "west region, Estrella Mountain Regional Park West",
#                   "SNE" = "west region, Sonoran Desert National Monument East",
#                   "SNW" = "west region, Sonoran Desert National Monument West",
#                   "WTM" = "west region, White Tanks Mountain Regional Park"),
#   "plot_id" = "plot id number",
#   "treatment_code" = c("C1" = "control plot 1"),
#   "sample_date" = c(format = "YYYY-MM-DD"),
#   "isotope_element" = "isotope atomic mass and chemical symbol",
#   "concentration" = "milligramPerKilogram")
#
# # custom unit
# milligramPerKilogram <- eml_define_unit(id = "milligramPerKilogram",
#                                             parentSI = "gramsPerGram",
#                                             unitType = "massPerMass",
#                                             multiplierToSI = "0.000001",
#                                             description = "millgram of isotope per kilogram of leaf tissue")
#
# icpms_desc <- 'Elemental composition of Larrea tridentata leaf tissue collected from control plots at Desert Fertilization study sites.'
# leaf_tissue_icpms.DT <- createtbl(leaf_tissue_icpms, icpms_desc)

# end old reml

# generate dataset ----
# slotNames(new('dataset'))

# things that will change with each dataset ----

projectid <- '632'

alternateIdentifier <- paste0(projectid, "_0")
pubDate <- '2016-09-14'
title <- 'Desert Fertilization Experiment: investigation of Sonoran desert ecosystem response to atmospheric deposition and experimental nutrient addition, 2006-2016'

abstract <- 'Launched in 2006 with support from the National Science Foundation (NSF) and leveraged by the CAP LTER, the Carbon and Nitrogen deposition (CNdep) project sought to answer the fundamental question of whether elemental cycles in urban ecosystems are qualitatively different from those in non-urban ecosystems. Ecosystem scientists, atmospheric chemists, and biogeochemists tested the hypothesis that distinct biogeochemical pathways result from elevated inorganic nitrogen and organic carbon deposition from the atmosphere to the land. To test the hypothesis, scientists examined the responsiveness of Sonoran desert ecosystems to nutrient enrichment by capitalizing on a gradient of atmospheric deposition in and around the greater Phoenix metropolitan area. Fifteen desert study sites were established, with five locations each west and east of the urban core, and in the urban core in desert preserves. In addition to the gradient of atmospheric deposition in and around the urban core, select study plots at each of the fifteen desert locations receive amendments of nitrogen, phosphorus, or nitrogen + phosphorus fertilizer. Measured variables include soil properties, perennial and annual plant growth, and atmospheric deposition of nitrogen. At the close of the initial grant period, the CAP LTER assumed responsibility for the project, renamed the Desert Fertilization Experiment, which provides a remarkable platform to study the long-term effects of nutrient enrichment on ecosystem properties.'

keys <- eml_keyword(list(
  "LTER Controlled Vocabulary Keyword Set" = c("plants",
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
                                               "phosphorus"),
  "LTER core areas" = c("disturbance patterns",
                        "primary production",
                        "movement of inorganic matter"),
  "CAPLTER Keyword Set List" = c("cap lter",
                                 "cap",
                                 "caplter",
                                 "central arizona phoenix long term ecological research",
                                 "arizona",
                                 "az",
                                 "arid land",
                                 "desert preserves")))

distribution <- new('distribution')
distribution@online@url <- paste0('http://data.gios.asu.edu/cap/HarvestListFileShow.php?id=', projectid)
distribution@online@onlineDescription = 'CAPLTER Metadata URL'

# people ----

stevanEarl <- new('metadataProvider')
stevanEarl@individualName@givenName <- 'Stevan'
stevanEarl@individualName@surName <- 'Earl'
stevanEarl@organizationName <- 'Arizona State University'

marisaMasles <- new('metadataProvider')
marisaMasles@individualName@givenName <- 'Marisa'
marisaMasles@individualName@surName <- 'Masles'
marisaMasles@organizationName <- 'Arizona State University'

quincyStewart <- new('metadataProvider')
quincyStewart@individualName@givenName <- 'Quincy'
quincyStewart@individualName@surName <- 'Stewart'
quincyStewart@organizationName <- 'Arizona State University'

eCook <- new('metadataProvider')
eCook@individualName@givenName <- 'Elizabeth'
eCook@individualName@surName <- 'Cook'
eCook@organizationName <- 'Arizona State University'

chrisClark <- new('metadataProvider')
chrisClark@individualName@givenName <- 'Chris'
chrisClark@individualName@surName <- 'Clark'
chrisClark@organizationName <- 'Environmental Protection Agency'

metadataProvider <-c(as(eCook, 'metadataProvider'),
                     as(chrisClark, 'metadataProvider'),
                     as(stevanEarl, 'metadataProvider'),
                     as(marisaMasles, 'metadataProvider'),
                     as(quincyStewart, 'metadataProvider'))

sharonHall <- new('creator')
sharonHall@individualName@givenName <- 'Sharon'
sharonHall@individualName@surName <- 'Hall'
sharonHall@organizationName <- 'Arizona State University'

nancyGrimm <- new('creator')
nancyGrimm@individualName@givenName <- 'Nancy'
nancyGrimm@individualName@surName <- 'Grimm'
nancyGrimm@organizationName <- 'Arizona State University'

jonathanAllen <- new('creator')
jonathanAllen@individualName@givenName <- 'Jonathan'
jonathanAllen@individualName@surName <- 'Allen'

jasonKaye <- new('creator')
jasonKaye@individualName@givenName <- 'Jason'
jasonKaye@individualName@surName <- 'Kaye'
jasonKaye@organizationName <- 'Penn State'

creator <- c(as(jonathanAllen, 'creator'),
             as(nancyGrimm, 'creator'),
             as(sharonHall, 'creator'),
             as(jasonKaye, 'creator'))

# methods and coverages ----

methods <- new('methods', methodStep = c(new('methodStep', description = 'biovolume: all perennial plants in select study plots were inventoried and measured in spring 2012. Biovolume is estimated from the maximum canopy extent along N-S and E-W axes, and plant height. For those plants (some trees, some Saguaros) that were too tall to measure with tape, height details were collected with a clinometer from level ground. The height of specimens measured by clinometer should be calculated conditionally depending on whether the value Base (deg) is positive or negative: Base < 0, height (m) = (TAN(RADIANS(Top))+TAN(RADIANS(ABS(Base))))*distance; Base > 0, height (m) = (TAN(RADIANS(Top))-TAN(RADIANS(ABS(Base))))*distance.'),
                                         new('methodStep', description = 'fertilizer_application: Fertilization with N, P, and N+P of 20 m x 20 m plots at 4 sites each in upwind, core, and downwind positions will be done twice per year to test the hypothesis that urban N deposition pushes the desert system toward P limitation. The level of N addition (as NH4NO3) is chosen to be ~ twice the current maximum deposition rate, i.e., 60 kg ha-1 y-1; P addition level* is 1/5 (by mass; 1/11 by atoms) that of N at 12 kg ha-1 y-1. Fertilizer is applied to treatment plots each winter and summer to coincide with the bimodal precipitation patterns characteristic of the region. The prescribed weight of dry, granular fertilizer (ammonium-nitrate for nitrogen treatments (N, N+P), triple-super phosphate for phosphorus treatments (P, N+P)) is applied using a hand spreader.'),
                                         new('methodStep', description = 'soil_ph: soil for pH analyses was collected from plant (beneath the canopy of Larrea tridentata) and inter-plant (between plant canopies) spaces within study plots. All soil within a copper ring (X diater) to a depth of 2 cm was collected with a metal spoon. The material was placed in individual plastic bags, transported to the laboratory at Arizona State University, and stored in a cool, dry location until the time of analysis. Lab procedure: (1) Mix a 2:1 ratio (by weight) of DI water and pre-sieved soil into a slurry (usually 30 mL water to 15 g soil). If the soil is highly organic, you may need to increase this ratio. (2) Wait 30 min before taking pH reading on samples. Individual samples should be started ~2 min apart to assure pH measurements are done for each sample at 30 min. During this time, calibrate the pH meter. Consult pH meter manual for calibration methods (also, see below). (3) Measure the pH of the soil slurry using a calibrated portable pH meter (be extremely careful with the tip of the probe!! Do not break it!). Press measure and wait until the pH meter has stabilized (stopped blinking) before you record the value. (4) Rinse electrode (including the bottom!) with DI water into a waste beaker between samples and dry with kim wipe but do not touch the bulb. (5) Every 10 samples check calibration in pH 7.00 buffer. If reading is off +/-0.05 or more, recalibrate the meter. (6) When finished, clean probe with DI water and place in probe storage solution (salt solution). (7) Again, if you are doing lots of soil samples, make sure that you are waiting only 30 minutes before each reading (plan so that your timing is correct by staggering addition of water by ~2 minutes).'),
                                         new('methodStep', description = 'plant_root_simulator: Soil anion and cation availability are quantified twice annually using ion-exchange membranes. Ion-exchange membranes (Plant Root Simulator (PRS) probes) are provided and analyzed by Western Ag Innovations, Saskatchewan, Canada. Probes for the analysis of soil anions and cations are deployed in pairs at locations under the canopy of Larrea tridentata (n=4) and in inter-plant spaces (n=4) within selected study plots. The probes are inserted vertically into the top 15 cm of soil, and left undisturbed until extraction after approximately 6 weeks in the ground. Upon extraction, the 8 probes of each type (anion/cation) from each habitat (plant/inter-plant) are pooled, washed with distilled water in accordance with protocols outlined by Western Ag Innovations Inc., and shipped overnight to Western Ag Innovations Inc. for analysis.'),
                                         new('methodStep', description = 'stem_growth: Stem length on select Larrea shrubs are measured twice per year—once in the fall and again in the spring. Five Larrea shrubs (designated L1 thru L5) have been selected as study plants in each plot at all CN Dep sites. Measurements are taken at 4 points on each study plant—one on a stem facing each of the cardinal directions. Each time the shrubs are measured, a new piece of colored lab tape is placed around the stem, which will be used as the point from which to measure stem elongation during the following period. Field Protocol: Before arriving at the study site, cut ca. 1-inch pieces of lab tape in half length-wise and loosely stick to back of clipboard. Make enough pieces for each direction on each plant (20 per plot). Label each piece with a plant number (L1-L5) on the left side of the tape and a cardinal direction (N, S, E, W) on the right side. Study plants are marked L1 thru L5 with a silver tree tag. For each of the 5 study plants per plot: 1. Measure stem elongation: a. Look for a piece of colored tape in each cardinal direction of the shrub. Make sure the calipers are calibrated to zero and you are measuring in mm. Measure the stem (in mm) from the node directly above the piece of tape to the last node on the stem. b. If the stem branches at any point above the tape, measure each branch separately and record all the branch lengths separated by commas on the datasheet. c. Do not measure the stalks of flowers or fruits. d. If the leaves directly above or below a node have fallen off and the stem is not measurable, make a note on the datasheet. e. If the tape cannot be found, check on the ground around the shrub to see if the tape has fallen off or the branch has died. Note on the datasheet if the tape is missing or the branch is dead. Do not leave any blank spaces on the datasheet! f. Remove the old tape from the stem. 2.Measure re-taped stem lengths: a. Re-tape the stem with a new piece of tape. Place the tape close to the end of the stem, but with a measurable internode between the tape and the end of the stem. b. Measure the re-taped stem lengths as above and record on the datasheet.'),
                                         new('methodStep', description = 'leaf_tissue_icpms: Field collection: (1) Randomly choose 5 creosote plants in each of the 5 study plots for every site (15 sites total). Do not choose shrubs chosen for stem-elongation (growth) measurements. (2) From each plant, randomly select one stem in each of 4 cardinal directions. (3) From each stem, pick 4, fully developed leaf pairs from the green section of the primary stem. We are defining fully developed leaves as those that are just behind the newest leaves (the newest leaves are the 2 undeveloped green ones plus the ones just attached to them). Do not choose leaves that are farther back down the stem or leaves that are brown and/or damaged. (4) Place all leaves from each plot in a coin envelope. (5) Place coin envelopes into a dryer (60 deg. C) upon returning to the laboratory or within 48 h. Lab processing: (1) Rinse all picked leaves with DI water in a clean fine meshed sieve and place in a coin envelope with the site name, plot name, and date processed. (2) Dry all leaves in coin envelopes at 70 deg. C for at least 48 h before grinding. (3) Fill polycarb vial approximately 1/2 full with leaf litter, add one of the polycarb balls and cap vial, place vials in ball mill chamber and mill for 10 minutes. ICPMS: Dried and milled Larrea leaf tissue is microwave digested using a CEM MARSXpress. Approximately 0.2500g of sample was allowed to react with 10mL of Trace Metal Grade nitric acid (Fisher #A509-P212) for 15 minutes, uncapped, then ran on the CEM using the following method: 1600W at 100% power, ramped up to 180 °C over the course of 15 minutes, and held at 180 °C for 15 minutes. Samples were diluted at 100X prior to being run by ICP-MS. Samples were analyzed using a Thermo Fisher Scientific X Series 2 quadrapole ICP-MS and Cetac ASX- 520 autosampler. Sample introduction consisted of a conical spray chamber with impact bead and concentric nebulizer with a flow of ~1ml/min. The spray chamber was cooled to 3 deg C by a Peltier cooling system. Collision Cell Technology (CCT) mode was utilized to reduce interferences by the argon gas used to generate the plasma by using a mixture of 7% hydrogen and 93% helium for the following elements: Fe, Se, As, and Ca. All other elements were run in normal mode. Internal standards (Sc, Ge, Y, In, and Bi) were used to compensate for sample matrix effects and instrument variability.'),
                                         new('methodStep', description = 'annuals_biomass: Each spring when there is sufficient rainfall, we estimate aboveground net primary productivity (ANPP) of herbaceous annual plants in each of the Desert Fertilization Experiment study plots by harvesting the aboveground portion of annual herbs from a 1/2 m x 1/2 m section of established 1 m x 1 m sub-plots.  These plots are distinguished from similar 1 m x 1 m sub-plots that are used for community composition. Take care to avoid the community composition sub-plots! Only harvest the sub-plots that are designated for biomass. Protocol overview: In each CNDep plot, we established 4 biomass sub-plots per plot, two in each of two patch types, between shrubs (IP for inter-plant space) and under the canopies of Larrea tridentata (P for under plant). Each sub-plot is coded with the patch type (IP or P); the letter B for biomass (to distinguish it from the community composition sub-plots, which are not harvested and do not have a letter B after the patch type code); and the sub-plot number (1 or 2).  For example, IPB1 & IPB2 are interplant space sub=plots one and two, and PB1 & PB2 are sub-plots 1 and 2 under Larrea tridentata shrubs. Remember that the biomass sub-plots are distinguished from the community composition sub-plots with the letter B after the patch type code (example, IPB1 = Biomass sub-plot 1, inter-plant space). Within each 1 m x 1 m biomass sub-plot, teams will clip aboveground annual biomass from one 1/2 m x 1/2 m quadrat in each 1-meter biomass sub-plot. This sub-section is rotated each year so that any given patch of ground is harvested at the most once every four years. Teams will carefully write the plot identifier on paper bags. (Double-check the labels! Labeling mistakes are the most common error in this procedure). Then teams will cut all green and brown herbaceous plant biomass that occurs within the boundaries of the 1/2 m x 1/2 m sub section and place it in the pre-labeled bag. After the harvest is complete, teams will dry the bags and biomass at 60 deg C for 48 hr then weigh the biomass (subtracting the weight of the bag) on a scale and record.'),
                                         new('methodStep', description = 'annuals_composition: Overview: In Spring 2008 within each site, the CAP LTER randomly chose and permanently marked four 1 m x 1 m sub-plots within each 20 m x 20 m main plot, split between two patch types, under L. tridentata shrubs and inter-plant spaces (hereafter, under L. tridentata, P and Inter-plant space; IP). At peak herbaceous biomass (in approximately March of each year), percent cover of all annual herbaceous species rooted within these four plots (two each under L. tridentata and in interplant spaces i.e. between shrubs) will be estimated. Note that there are also four 1 m x 1 m Biomass sub-plots in each 20 m x 20 m plot – these are different than the community composition plots described here because they are clipped each year while the community composition plots are not. Care should be taken in the field to identify the correct four plots for community composition and avoid the four plots to be harvested for biomass. Steps: 1. On the top of the data sheet (See Table 3 for sample), record the date; names of the sampling crew (i.e. plant identifier, recorder); site (three letter code); plot number (1 to 75); treatment (C1 – no sampling from C2 plots, Nitrogen [N], Phosphorus [P], and Nitrogen+Phosphorus [NP]). Double check the plot numbers and sub-plot numbers! 2. All observers should first calibrate their percent cover estimates on several test plots in the field prior to percent cover measurements. You want to make sure all botanists are seeing the plots similarly. 3. Place the 1 m x 1 m quadrat over the corner rebar at each 1-m2 plot. Working in teams of two (one recorder, one plant identifier), proceed through the steps below. 4. Estimate and record (at the bottom of the data sheet) the relative areal cover of plant matter (e.g. green material) versus bare ground (e.g. brown material) at the soil surface in the plot (e.g. 70% green, 30% brown). If there are soil crusts, measure those as well in a separate category. 5. Identify which annual species are present in the plot and mark a small dot in the corner of the datasheet to easily find later. Use roughly the same amount of effort in each plot to find plant species. As you get better, it will take less time because you are getting more skilled; as you get tired, it will take more time; in simpler systems, it will take less time. There is no a priori way to know when you are using the same amount of effort, but this is usually approximated by time (in minutes: typically 1-3 minutes per species). Each botanist should do some test plots to see how long it takes him/her to find all the species in the plot. Different botanists will take different amounts of time to apply the same effort– this is ok – but the same botanist should use approximately the same amount of time per plot. 6. Next, estimate and record the cover of each species individually to the nearest whole number (e.g., 1%, 2%, 3%...97%, 98%, 99%, 100%). If a species is present in a plot, but does not cover a full 1%, record it as &lt;1%. The sum of all individual species cover can be greater than 100% since some species overlap. 7. Finally, estimate and record the cover of the “overstory” shrubs within the plot or overhanging the plot. We estimate cover for all annuals rooted in the quadrat, but areal cover of any shrubs rooted in or hanging over the plot. For IP plots, this will be zero most of the time (i.e. unless there is a large shrub nearby that has grown over the plot). For P plots, this will be have two components: (1) the areal projection of the shrub onto the 2-D grid, and (2) the area of the stem base or any other shrub stems. It is expected that the cover of the shrubs overlap the herbaceous species. However if two shrub species overlap in their cover aggregate these into a single larger individual (rare case). 8. Unknown species are recorded with unique identifiers (e.g., Unknown 1, Unknown 2, etc) along with a visual description of the plant and a picture to be used to help identify the unknown in the lab. When possible, a sample of the unknown species should be collected from outside of the plot in a labeled coin envelope and brought to the herbarium to be identified. Unknown species are usually very small and_or undeveloped, and may not be able to be identified to species – this is ok. Err on the side of lumping unknown species into other known species in the same plot. 9. Use caution not to include the percent cover of desiccated/dead plants within the plots from the previous season (these can usually be identified easily as white, old plant material). However, depending on the timing of data collection, some herbaceous plants from the current growing season may have passed peak biomass and already begun to desiccate (dry out or senesce). In this case, estimate the percent cover of these plants as if they were at peak biomass. 10. Before you leave a site, confirm that you have datasheets for all plots and sub-plots, and that there are no duplicates or ambiguous plot labels on your sheet.')))

# note that dates must have a begin and end
# scientific names must have genus and sp (even if just spp) !!!!!
coverage <- eml_coverage(dates = c('2005-12-07', '2016-05-25'),
                         geographic_description = 'CAP LTER study area',
                         NSEWbox = c(+33.726771147871851, +33.013262396669028, -111.482761068522152, -112.547375174279352),
                         scientific_names = c('Pectocarya spp.', 'Larrea tridentata'))

# things that are less likely to change ----

giosAddress <- new('address',
                   deliveryPoint = 'PO Box 875402',
                   city = 'Tempe',
                   administrativeArea = 'AZ',
                   postalCode = '85287',
                   country = 'USA')

contact <- new('contact',
               organizationName = 'Julie Ann Wrigley Global Institute of Sustainability, Arizona State University',
               positionName = 'Data Manager',
               address = giosAddress)

publisher <- new('publisher',
                 organizationName = 'Arizona State University, Julie Ann Wrigley Global Institute of Sustainability')

language <- 'english'

rights <- 'Copyright Board of Regents, Arizona State University. This information is released to the public and may be used for academic, educational, or commercial purposes subject to the following restrictions. While the CAP LTER will make every effort possible to control and document the quality of the data it publishes, the data are made available \'as is\'. The CAP LTER cannot assume responsibility for damages resulting from mis-use or mis-interpretation of datasets, or from errors or omissions that may exist in the data. It is considered a matter of professional ethics to acknowledge the work of other scientists that has resulted in data used in subsequent research. The CAP LTER expects that any use of data from this server will be accompanied with the appropriate citations and acknowledgments. The CAP LTER encourages users to contact the original investigator responsible for the data that they are accessing. Where appropriate, researchers whose projects are integrally dependent on CAP LTER data are encouraged to consider collaboration and/or co-authorship with original investigators. The CAP LTER requests that users submit to the Julie Ann Wrigley Global Institute of Sustainability at Arizona State University reference to any publication(s) resulting from the use of data obtained from this site. The CAP LTER requests that users not redistribute data obtained from this site. However, links or references to this site may be freely posted.'

# construct output ----
# works but cannot incorporate custom_units
delta <- new('dataset',
             #  scope = scope, # not sure the order of this one
             #  system = system, # not sure the order of this one
             alternateIdentifier = alternateIdentifier,
             language = language,
             creator = creator,
             metadataProvider = metadataProvider,
             contact = contact,
             distribution = distribution,
             publisher = publisher,
             title = title,
             pubDate = pubDate,
             keywordSet = keys,
             abstract = abstract,
             intellectualRights = rights,
             methods = methods,
             coverage = coverage,
             dataTable = c(biovolume.DT,
                           fertilizer_application.DT,
                           soil_ph.DT,
                           plant_root_simulator.DT,
                           stem_growth.DT,
                           # leaf_tissue_icpms.DT,
                           annuals_composition.DT,
                           annuals_biomass.DT))
frank <- new('eml',
             packageId = paste0("knb-lter-cap.", projectid, ".0"),
             system = "knb",
             scope = "system",
             dataset = delta)

eml_write(frank,
          file = "./outFile.xml")

# custom_units ----
eml_write(custom_units = c(microgramPerTenSquareCentimeterPerBurialLength,
                           milligramPerKilogram),
                           file = "./customUnits.xml")