##########################################################################################################################################
#Create a raster of posterior probabilities given a psy, p and q. Numer of sampling events N and records y ###############################
#psi, p and q are constant across the study region and can only range between 0.0001 and 0.009 and cell are treated as independent  ######
##########################################################################################################################################

posteriorOccProbabilityConstantPsyPQRaster=function(N,y,psy,p,q,nMin,nMax){


whichCellsSampled=which(is.na(N[])==F&N[]!=0&N[]>=nMin&N[]<=nMax) #List the cell numbers that should be used

psyPp=N #Raster in which the posterior proababilities will be gathered
psyPp[]=NA

for (i in whichCellsSampled){
  
ni=N[i]  #Sampling events in cell i
yi=y[i]  #detection in cell i

#if(is.infinite(choose(n=ni,k=yi))){next} #Too large numbers are ignored

numerator=log((((psy)*(p^yi)*((1-p)^(ni-yi))))) #Likelihood function in cell i
denominator=log(((psy)*(p^yi)*((1-p)^(ni-yi)))+(((1-psy)*(q^yi)*((1-q)^(ni-yi))))) #Likelihood function in cell i

spyPpi=exp(numerator-denominator)
psyPp[i]=spyPpi

}


return(psyPp)

}