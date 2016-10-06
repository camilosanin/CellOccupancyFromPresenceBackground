##############################################################################################################
#Read all sampling events recoerded on eBird during the breeding season (June 15th to August 15th of 2000 on)#
#in the region specified by a basic raster, then create a .csv file with the cleaned up sampling events ######
##############################################################################################################

###Objects from master###

#spSummerEvents = spatial points dataframe with cleaned sampling event points
#baseRaster = basic raster in which sampling effort will be estimated
#basicRasterCRS + CRS of the basic raster. Assumed to be the same for sampling events
#dataVersFolder where processed data should be saved


saveToFolder=paste(dataVersFolder,"/samplingEffort",sep="")
try(dir.create(saveToFolder),silent = T)

spSummerEventsJustPoints=SpatialPoints(spSummerEvents)

SamplingEventsRaster=rasterize(spSummerEventsJustPoints, baseRaster, fun="count") #create a raster in which the value of each cell is the number of sampling events in it
SamplingEventsRaster[is.na(SamplingEventsRaster[])]=0
SamplingEventsRaster[is.na(baseRaster[])]=NA


writeRaster(SamplingEventsRaster,paste(saveToFolder,"/samplingEffort.grd",sep=""))

