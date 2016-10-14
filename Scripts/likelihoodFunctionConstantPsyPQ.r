#####################################################################################################################################
#Estimate the loglikelihood of ################
#in the region specified by a basic raster, with unique sampling events then create a .csv file with the cleaned up localities ######
#####################################################################################################################################

###Objects from master###

likelihoodFunctionConstantPsyPQ=function(N,y,psy,p,q,nMin,nMax){

  addLHFactor=0

if(psy>0.9999){
  addLHFactor=addLHFactor+psy-0.9999
  psy=0.9999
  message("psy out of bounds")
}

 if(p>0.9999){
    addLHFactor=addLHFactor+p-0.9999
    p=0.9999
    message("p out of bounds")
  }

  if(q>0.9999){
    addLHFactor=addLHFactor+q-0.9999
    q=0.9999
    message("q out of bounds")
  }
  
  if(psy<0.0001){
    addLHFactor=addLHFactor-psy+0.0001
    psy=0.0001
    message("psy out of bounds")
  }
    
  if(p<0.0001){
    addLHFactor=addLHFactor-p+0.0001
    p=0.0001
    message("p out of bounds")
  }
  
  if(q<0.0001){
    addLHFactor=addLHFactor-q+0.0001
    q=0.0001
    message("q out of bounds")
  }
    
whichCellsSampled=which(is.na(N[])==F&N[]!=0&N[]>=nMin&N[]<=nMax) #List the cell numbers that should be used for likelihood stimate (Has been sampled at least nMin but no more than nMax times)


likelihoodV=vector()

for (i in whichCellsSampled){

ni=N[i]
yi=y[i]

if(is.infinite(choose(n=ni,k=yi))){next} #Too large numbers are ignored

likelihoodi=log((choose(n=ni,k=yi)*((psy)*(p^yi)*((1-p)^(ni-yi))))+((1-psy)*(q^yi)))

likelihoodV=c(likelihoodV,likelihoodi)

}

likelihoodT=sum(likelihoodV, na.rm=T)-addLHFactor

return(likelihoodT)

}