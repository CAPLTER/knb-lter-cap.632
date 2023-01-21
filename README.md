# knb-lter-cap.632

dataset publishing: Desert Fertilization

## overview

This repository is the home of publishing processes for the desert fertilization project beginning with version 632.3. Resources, workflows, and processes for database creation, the initial publication, and previous versions (632.1, 632.2) are in the 'cndep' repository that resides with GIOS on AWS. However, the R script file that documents the creation of the database and initial publication have been moved to this repository, and the 'cndep' repository should be able to be ignored going forward. 'cndep' had also held the data, but those are all now moved to the cndep folder of the department drive.

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
