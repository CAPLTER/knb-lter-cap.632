# knb-lter-cap.632

dataset publishing: Desert Fertilization (formerly CNdep)

## overview

- This repository is the home of publishing processes for the desert
fertilization project beginning with version 632.3.
- The R script file that documents the creation of the database and initial
publication are also included.

### a note about annuals cover events

We are clarifying the rare cases of annuals sampling where there are not any
annual plants present in a plot with more consistent and diligent use of the
`sampled = 1` value for such records. What that does not cover, however, are
cases when a subplot was not sampled. This may be self evident by lack of a
record for a year*plot*location*subplot combination but there is not a clear
way to make this explicit. In the meantime, below we document cases where
subplots were not sampled for a variety of reasons. Note that this does not
address the COVID era when field work was curtailed and entire sites were
skipped.

> There are a few plots in 2017 and 2018 for which we have not recorded any
> annual plants or plot characteristics despite having recorded observations at
> other, adjacent plots in the same year.

| year| plot_id|site_code |sample_date |treatment_code |location_within_plot | subplot| chars_missing_count|
|----:|-------:|:---------|:-----------|:--------------|:--------------------|-------:|-------------------:|
| 2017|      37|MVP       |2017-03-16  |NP             |IP                   |       1|                   3|
| 2017|      37|MVP       |2017-03-16  |NP             |IP                   |       2|                   3|
| 2017|      37|MVP       |2017-03-16  |NP             |P                    |       1|                   3|
| 2018|      38|MVP       |2018-03-19  |C1             |IP                   |       2|                   3|
| 2018|      38|MVP       |2018-03-19  |C1             |P                    |       1|                   3|
| 2018|      38|MVP       |2018-03-19  |C1             |P                    |       2|                   3|
| 2018|      39|MVP       |2018-03-19  |P              |IP                   |       1|                   3|
| 2018|      39|MVP       |2018-03-19  |P              |P                    |       1|                   3|
| 2018|      39|MVP       |2018-03-19  |P              |P                    |       2|                   3|
| 2018|      15|SRR       |2018-03-19  |N              |IP                   |       1|                   3|
| 2018|      15|SRR       |2018-03-19  |N              |P                    |       1|                   3|
| 2018|      15|SRR       |2018-03-19  |N              |P                    |       2|                   3|
| 2018|       6|UMP       |2018-03-21  |NP             |IP                   |       1|                   3|
| 2018|       6|UMP       |2018-03-21  |NP             |P                    |       1|                   3|
| 2018|       6|UMP       |2018-03-21  |NP             |P                    |       2|                   3|
| 2018|      37|MVP       |2018-03-19  |NP             |IP                   |       1|                   2|
| 2018|      37|MVP       |2018-03-19  |NP             |P                    |       1|                   2|

data assessment by C. Hauck:

- 2017 37 MVP P2 no data were taken because "subplot not found"
- 2018 6 UMP IP2 no data were taken because "rebar missing -> not sampled"
- 2018 15 SRR IP2 no data were taken because "disturbed and markers removed"
- 2018 37 MVP P2 and IP2 no data were taken because "could not find plots"
- 2018 38 MVP IP1 no data were taken because "2nd re-bar missing: not sampled"
- 2018 39 MVP IP2 no data were taken because "1 re-bar missing: not sampled"


### knb-lter-cap.632.18 *2026-01-xx*

- data refresh
- major database changes, particularly related to annuals; the most notable changes include:
  + splitting annuals and plot characteristics into separate tables
  + converting data from long to wide
  + addressing data structure, format, and quality issues catalogued in the following issues:
    - https://github.com/CAPLTER/knb-lter-cap.632/issues/31
    - https://github.com/CAPLTER/knb-lter-cap.632/issues/30
    - https://github.com/CAPLTER/knb-lter-cap.632/issues/29
    - https://github.com/CAPLTER/knb-lter-cap.632/issues/28
    - https://github.com/CAPLTER/knb-lter-cap.632/issues/27
    - https://github.com/CAPLTER/knb-lter-cap.632/issues/26
- minor qmd refactoring

### knb-lter-cap.632.17 *2024-10-11*

- data refresh

### knb-lter-cap.632.16 *2024-08-14*

- data refresh

### knb-lter-cap.632.15 *2023-12-06*

- data refresh
- incorporates more of the yaml approach, here data entities (except site
locations) and people are documented via yaml configuration files
- incorporates QUDT units
- adds for the first time, albeit limited, annotations, mostly in conjunction
with the move to QUDT units but also annotations are added to some attributes
(mostly dates)
- adds a notes field to the annuals composition table


### knb-lter-cap.632.14 *2023-01-21*

- data refresh
- add `capeml::update_attributes` function to workflow
- (most) attribute metadata updated from `csv` to `yaml` format
- fixes some mislabeled PRS probe data (under plant instead of blank)
- Rmd to qmd
- update people attributes to draw from file instead of database


### knb-lter-cap.632.13 *2022-06-02*

- fixing an error in the dates assigned to PRS probe blanks in the most recent deployment
- update config.yaml to newest format


### knb-lter-cap.632.12 *2022-05-12*

- data refresh with a focus on PRS data
- PRS data now includes notes
- annuals composition


### knb-lter-cap.632.11 *2022-04-20*

- data refresh with a focus on PRS data
- fixed an error in PRS data discovered by B. Ball
- changed geographic coverage from a single polygon spanning all points to
  site-specific polygons per [19](https://github.com/CAPLTER/knb-lter-cap.632/issues/19)


### knb-lter-cap.632.10 *2021-12-17*

- added site code to atm deposition per [#18](https://github.com/CAPLTER/knb-lter-cap.632/issues/18)
- added maintenance to config.yaml
- code cleaning: namespace functions, R pipe
- enhanced description of fertilizer per [#18](https://github.com/CAPLTER/knb-lter-cap.632/issues/18)


### knb-lter-cap.632.9 *2020-10-02*

* data refresh
* updated to config.yaml
* added resin data !!!
* updated methods
* fixed labelling errors in tissue_icp
* tissue_icp and icp_raw_data now completely buildable with each update
  regarldess of whether new icp data were added (no need to copy old xml
  sections of data that were not updated)


###  knb-lter-cap.632.8 *2019-12-12*
 
* updated stems and fertilizer data
* adds plots notes to stems data
* moved Jon Allen to end of author list
* updated workflow to use new create_spatialVector function for site locations
* features updated LTER intellectualRights

Instead of piece-mealing the workflow, I ran all chunks regardless of whether
there was new data to push. This worked very well with the only hiccup being the
very complicated icp data. Revisit that workflow so that we can get this to a
point where we can have push-button updates to the entire data set.

The problem noted with the PRS probes in the previous round did occur this
round.


###  knb-lter-cap.632.7 *2019-07-01*

* updated annuals, PRS, stems, and CHN data. The updated data were pulled from
  the database and built into the resulting EML. EML for other data items that
  were not updated (e.g., fertilizer application, GIS, tissue ICP, zipped ICP
  files) were copied from a previous version (632.6). 
* used taxonomyCleanr for taxonomy

Encountered a bizarre error with this round: building the PRS DT sensu below did
not work, it generated the csv but did not create the DT element. Seems that
create_dataTable was getting hung up on the units for final value
(microgramPerTenSquareCentimeterPerBurialLength). The error was not actually
with my code but something in the rEML package set_attributes() function.
Investigated for quite some time without resolution, and it only occurred with
this particular table and attribute. As a result, I left the unit for
final_value empty in the plant_root_simulator_attrs.csv then manually added that
unit back into the resulting EML file.

plant_root_simulator_DT <- create_dataTable(
    dfname = plant_root_simulator,
    description = plant_root_simulator_desc,
    dateRangeField = 'start_date')

In addition, rEML again did not properly construct the customUnits list, and
that XML had to be modified by hand.


###  knb-lter-cap.632.6 *2018-06-05*

* workflow to the Rmd template
* added the approximate spatial data
* moved the data publication component of tissue CHN to this workflow.
* moved 'annuals 2008', 'soil ph (2010 and 2011)' and 'biovolume' to new,
  indepdendent data sets so that those static components of the projet are no
  longer integrated with the on-going aspects.
* moved the tissue CHN publishing code here but the new CHN data processing
  code is in a separate R file. Note also that the publishing component of ICP
  is still separate owing to the very different nature of that data entity.
  For this update and, really, available for all future updates, is to blast
  through the Rmd chunks for everything, and simply paste the dataTable and
  otherEntity objects that correspond to ICP into the new 632 XML.
