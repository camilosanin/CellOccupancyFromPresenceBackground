This project aims to estimate occupancy in grid cells of a continent based on data not collected systematically.

eBird (http://ebird.org/) data for a set of North American Warblers are used as a initial case-study

##/Scripts
/master.r = workflow for the project
/counteBirdSamplingEvents.r = Read all sampling events recorded on eBird during the breeding season (June 15th to August 15th). Create a .csv file with each sampling event and  a raster of sampling effort based on alt raster

FILES NOT IN THE REPOSITORY

SampligDatabase4Columns.csv (CSV) = all sampling events of eBird. Only 4 fields included: Sampling Event, Long, Lat, Date

##/Data
/eBird/ = eBirdData last updated in Nov2014
/eBird/Raw_eBird_byTaxon/*species*.txt (tab delimited) = all records of each of the six species with all the fields provided by eBird
/VX/ = data derived from analyses in the X version
/V1/samplingEffort/summerEvents.csv  All sampling events of eBird that happened in the study region and time period determined (summers after 2000 see scripts)
/V1/samplingEffort/samplingEffort.grd / grd : Raster of number of sampling events per cell
