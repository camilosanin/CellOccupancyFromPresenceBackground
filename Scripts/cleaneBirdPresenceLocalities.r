#####################################################################################################################################
#Read all localities in eBird recorded for species spduring the breeding season (June 15th to August 15th of 2000 on)################
#in the region specified by a basic raster, with unique sampling events then create a .csv file with the cleaned up localities ######
#####################################################################################################################################

###Objects from master###

#sp = focal species for which presences are going to be cleanes
#rawLocDataFile = path to txt (delim) file of all localities for taxon sp
#baseRaster = basic raster in which sampling effort will be estimated
#basicRasterCRS + CRS of the basic raster. Assumed to be the same for sampling events
#dataVersFolder where processed data should be saved

#Read all records and create an individual column for day month and year

saveToFolder=paste(dataVersFolder,"/PresenceLocalities/TotalSummerLocs",sep="")
try(dir.create(saveToFolder, recursive = T),silent = T)
spRecords=read.delim(rawLocDataFile, h=T)
spRecords=spRecords[,c("SAMPLING.EVENT.IDENTIFIER","LONGITUDE","LATITUDE","OBSERVATION.DATE")]
names(spRecords)=c("SamplingEvent_id","Longitude","Latitude","Date")

dateSampled=do.call(rbind,strsplit(spRecords[,"Date"],'-',fixed=TRUE))

mode(dateSampled)="numeric"
colnames(dateSampled)=c("year","month","day")
spRecords=cbind(spRecords,dateSampled)

#Select summer records (June 15th to August 15th) and limit it to 2000 to 2014 and create a SpatialDataset

summerRecords=which(
  spRecords[,"year"]>=2000&(                              #Only years after 2000
    (spRecords[,"day"]>=15&spRecords[,"month"]==6)|        #Only after june 15th
      (spRecords[,"month"]==7)|                               #Only July
      (spRecords[,"day"]<=15&spRecords[,"month"]==8)         #Only before August 15th
  ) & spRecords[,"day"]!=0                                #No undifined days (0)
)

SpSummerLocs=spRecords[summerRecords,]

length(unique(SpSummerLocs[,"SamplingEvent_id"]))==dim(SpSummerLocs)[1]    #Are all sampling Events Unique?
sum(is.na(SpSummerLocs[,"Latitude"]|is.na(SpSummerLocs[,"Longitude"]))) #Are there any NA coordinates?

SpSummerLocsSp=SpatialPointsDataFrame(coords= SpSummerLocs[,c("Longitude","Latitude")], data=SpSummerLocs[,c("SamplingEvent_id","year","month","day")],proj4string= basicRasterCRS)

#Limit the sampling events to those that fall in the study region (as defined by the basicRaster) lat/long range

locsInExtent=which(
  SpSummerLocsSp@coords[,1]>=baseRaster@extent@xmin &
  SpSummerLocsSp@coords[,1]<=baseRaster@extent@xmax &    
  SpSummerLocsSp@coords[,2]>=baseRaster@extent@ymin &    
  SpSummerLocsSp@coords[,2]<=baseRaster@extent@ymax    
      )

SpSummerLocsSp=SpSummerLocsSp[locsInExtent,]

#Limit the sampling events to those that fall on land (as no NA) in the study region (as defined by the basicRaster) lat/long range

locsInLand=which(is.na(extract(baseRaster,SpSummerLocsSp))==F)

SpSummerLocsSp=SpSummerLocsSp[locsInLand,]

uniqueLocalities=unique(paste(SpSummerLocsSp@coords[,1],SpSummerLocsSp@coords[,1],sep="_"))

length(SpSummerLocsSp)[1]/length(uniqueLocalities) #On Average, how many times has a locality been sampled?

#Save a csv file with cleaned sampling localities

summerLocsDF=data.frame(SpSummerLocsSp)
summerLocsDF[,"optional"]=NULL

write.csv(summerLocsDF,paste(saveToFolder,"/",sp,".csv",sep=""),row.names = F)

##Create a raster witht he number of times species sp has been observed##
#########################################################################

saveToFolder=paste(dataVersFolder,"/PresenceLocalities/SummerLocalityCount",sep="")
try(dir.create(saveToFolder),silent = T)

SpSummerLocsSpJustPoints=SpatialPoints(SpSummerLocsSp)

spRecordsCountRaster=rasterize(SpSummerLocsSpJustPoints, baseRaster, fun="count") #create a raster in which the value of each cell is the number of sampling events in it
spRecordsCountRaster[is.na(spRecordsCountRaster[])]=0
spRecordsCountRaster[is.na(baseRaster[])]=NA


writeRaster(spRecordsCountRaster,paste(saveToFolder,"/",sp,".grd",sep=""))


