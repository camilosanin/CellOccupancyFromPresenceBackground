#################################################################################################################################################
#Create a rasterStack of rasters with d rows and columns. First raster is "true" occupancy (derived from psy), second raster is sampling ########
#effort (from a lognormal distribution of smean and ssdev), third true detections based on p, fourth are false detections in unoccupied sampling#
#units, and the fith are total detections. psi, p and q are constant across the study region and cells are treated as spatially independent######
#################################################################################################################################################



###Input variables###
# d = numbers of row and columns of the simulated raster
# psy - proportion of occupied cells
# smean and ssdev - mean and stardard deviation of the log of sampling effort
# p - detectavility to simmulate
# q - proportion of false dectections in unoccupied cells


simulateConstantPsyPR=function(d,psy,smean,ssdev,p,q){
  

baseRaster = raster(nrows=d,ncols=d, xmn=0,xmx=100,ymn=0,ymx=100)

##Create raster of true occurrences
trueOcc = baseRaster
trueOcc[] = sample(c(0,1),size = d^2,replace = T, prob = c(1-psy,psy)) #Occurrence is simulated 

##Create raster of sampling effort

samplingEffort = baseRaster
samplingEffort[] = round(exp(rnorm(d^2, mean = smean, sd = ssdev)))

##Create raster of true detections (with p detection probablility in occupied cells)

trueDetections = baseRaster
for(i in 1:d^2){
  
trueDetections[i] = sum(runif(samplingEffort[i])<=p) #Number of detections if all cells sampled are occupied

}

trueDetections = trueDetections * trueOcc #True detections only in cells that are occupied

##Create raster of false detections (with q detection probablility in occupied cells that are not occupied)

falseDetections = baseRaster

for(i in 1:d^2){
  
  falseDetections[i] = sum(runif(samplingEffort[i])<=q) #Number of false detections if all cells sampled are not occupied
  
  }

falseDetections = falseDetections * (1-trueOcc) #False detections only in cells that are not occupied

totalDetections = trueDetections + falseDetections

finalStack=stack(trueOcc,samplingEffort,trueDetections,falseDetections,totalDetections)
names(finalStack)=c("trueOcc","samplingEffort","trueDetections","falseDetections","totalDetections")

return(finalStack)

}