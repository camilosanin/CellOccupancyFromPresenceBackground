############################################################################################
##This script reads eBird data for all sampling events and for the species sp
############################################################################################
options(stringsAsFactors = FALSE)

library(raster)

#Set directory and location of all data files

setwd("C:/GitHub/CellOccupancyFromPresenceBackground/")

dataVersFolder="./Data/V1"
try(dir.create(dataVersFolder), silent=T)

altRasterFile="./Data/Basic/alt.grd"    #this raser will be used to define the extent and resulution of study region. Data/NA is also use to restrict sampling events to land
baseRaster=raster(altRasterFile)
basicRasterCRS=CRS("+proj=longlat +datum=WGS84")

#Read all sampling events recoerded on eBird during the breeding season (June 15th to August 15th), limit it to the extent and data of the basic raster and save a csv file#
############################################################################################################################################################################

totalSamEventsFile="~/LargeFilesGitHub/CellOccupancyFromPresenceBackground/SampligDatabase4Columns.csv" #where is the samping event data THIS FILES IS NOT ON GITHUB

#source("./Scripts/cleaneBirdSamplingEvents.r")

#Resulting objects from cleaneBirdSamplingEvents.r:
#spSummerEvents = cleaned Spatial data frame with all sampling events, coordinates and date

spSummerEvents=read.csv(paste(dataVersFolder,"/samplingEffort/summerEvents.csv",sep=""), h=T)
spSummerEvents=SpatialPointsDataFrame(coords= spSummerEvents[,c("Longitude","Latitude")], data=spSummerEvents[,c("SamplingEvent","year","month","day")],proj4string= basicRasterCRS)

#Create a sampling effort raster - right now the most basic approach: counting sampling events#
###############################################################################################
#Resulting objects from samplingEffortRasterCount.r:
#SamplingEventsRaster = raster with extent and NAs of base raster and the number of sampling events in each cell

#source("./Scripts/samplingEffortRasterCount.r")
#samplingEffort=SamplingEventsRaster

samplingEffort=raster(paste(dataVersFolder,"/samplingEffort/samplingEffort.grd",sep=""))

##Extract and clean localities where speces sp has been observed##
##################################################################

sp="Oreothlypis_ruficapilla"
rawLocDataFile=paste("./Data/eBird/Raw_eBird_byTaxon/",sp,".txt",sep="")
#source("./Scripts/cleaneBirdPresenceLocalities.r")
#Resulting objects from cleaneBirdSamplingEvents.r:
#SpSummerLocsSp = cleaned Spatial data frame with all record localities, sampling event id and coordinates and date
#spRecordsCountRaster =  raster with extent and NAs of base raster and the number of sampling events in each cell

summerLocsDF=read.csv(paste(dataVersFolder,"/PresenceLocalities/TotalSummerLocs/",sp,".csv",sep=""), h=T)
SpSummerLocsSp=SpatialPointsDataFrame(coords= summerLocsDF[,c("Longitude","Latitude")], data=summerLocsDF[,c("SamplingEvent_id","year","month","day")],proj4string= basicRasterCRS)
spRecordsCountRaster=raster(paste(dataVersFolder,"/PresenceLocalities/SummerLocalityCount/",sp,".grd",sep=""))

proportion=spRecordsCountRaster/samplingEffort
proportion[samplingEffort[]<50]=NA
#proportion[proportion[]>0.05]=NA
proportion[proportion==0]=NA
sum(is.na(proportion[])==F)
plot(proportion)
hist(log(proportion))
