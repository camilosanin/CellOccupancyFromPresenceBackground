##############################################################################################################
#Read all sampling events recoerded on eBird during the breeding season (June 15th to August 15th of 2000 on)#
#in the region specified by a basic raster, then create a .csv file with the cleaned up sampling events ######
##############################################################################################################

###Objects from master###

#totalSamEventsFile = path to csv file of all sampling events
#baseRaster = basic raster in which sampling effort will be estimated
#basicRasterCRS + CRS of the basic raster. Assumed to be the same for sampling events
#dataVersFolder where processed data should be saved

#Read all records and create an individual column for day month and year

saveToFolder=paste(dataVersFolder,"/samplingEffort",sep="")
try(dir.create(saveToFolder),silent = T)
allRecords=read.csv(totalSamEventsFile, h=T)
dateSampled=do.call(rbind,strsplit(allRecords[,"Date"],'-',fixed=TRUE))
mode(dateSampled)="numeric"
colnames(dateSampled)=c("year","month","day")
allRecords=cbind(allRecords,dateSampled)

#Select summer records (June 15th to August 15th) and limit it to 2000 to 2014 and create a SpatialDataset

summerRecords=which(
  allRecords[,"year"]>=2000&(                              #Only years after 2000
    (allRecords[,"day"]>=15&allRecords[,"month"]==6)|        #Only after june 15th
      (allRecords[,"month"]==7)|                               #Only July
      (allRecords[,"day"]<=15&allRecords[,"month"]==8)         #Only before August 15th
  ) & allRecords[,"day"]!=0                                #No undifined days (0)
)

summerEvents=allRecords[summerRecords,]

length(unique(summerEvents[,"SamplingEvent"]))==dim(summerEvents)[1]    #Are all sampling Events Unique?
sum(is.na(summerEvents[,"Latitude"]|is.na(summerEvents[,"Longitude"]))) #Are there any NA coordinates?

spSummerEvents=SpatialPointsDataFrame(coords= summerEvents[,c("Longitude","Latitude")], data=summerEvents[,c("SamplingEvent","year","month","day")],proj4string= basicRasterCRS)

#Limit the sampling events to those that fall in the study region (as defined by the basicRaster) lat/long range

locsInExtent=which(
  spSummerEvents@coords[,1]>=baseRaster@extent@xmin &
  spSummerEvents@coords[,1]<=baseRaster@extent@xmax &    
  spSummerEvents@coords[,2]>=baseRaster@extent@ymin &    
  spSummerEvents@coords[,2]<=baseRaster@extent@ymax    
      )

spSummerEvents=spSummerEvents[locsInExtent,]

#Limit the sampling events to those that fall on land (as no NA) in the study region (as defined by the basicRaster) lat/long range

locsInLand=which(is.na(extract(baseRaster,spSummerEvents))==F)

spSummerEvents=spSummerEvents[locsInLand,]

uniqueLocalities=unique(paste(spSummerEvents@coords[,1],spSummerEvents@coords[,1],sep="_"))

length(spSummerEvents)[1]/length(uniqueLocalities) #On Average, how many times has a locality been sampled?

#Save a csv file with cleaned sampling localities

summerEventsDF=data.frame(spSummerEvents)
summerEventsDF[,"optional"]=NULL

write.csv(summerEventsDF,paste(saveToFolder,"/summerEvents.csv",sep=""),row.names = F)


#tableUniqueLocs=table(paste(summerEvents[,"Latitude"],summerEvents[,"Longitude"],sep="_"))
