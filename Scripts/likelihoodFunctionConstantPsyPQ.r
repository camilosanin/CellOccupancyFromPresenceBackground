########################################################################################################################################
#Estimate the -log likelihood of the detection across a grid based on rasters of N sampling events and y presence records ##############
#psi, p and q are constant across the study region and can only range between 0.00001 and 0.99999 and cells are treated as independent##
#Approximation for the log of the binomail coefficient is used for more than 25 sampling events and likelihoods are ####################
#calculated in logs as much as possible to be able to compute very small numbers #######################################################
########################################################################################################################################

###Objects from master###

source("./Scripts/binaryEntropyFunctionH.r")
source("./Scripts/logxpy.r")


likelihoodFunctionConstantPsyPQ=function(N,y,psy,p,q,nMin,nMax){

####These first lines correct and penilize when the paramenters are our of bound. 
####This is needed to avoid NA and Inf results in optimizing algorithms. 
####Values por Psy, p and q outside 0-1 have no meaning 
  
  addLHFactor=0

if(psy>0.99999){
  addLHFactor=addLHFactor+psy-0.99999
  psy=0.99999
  message("psy out of bounds")
}

 if(p>0.99999){
    addLHFactor=addLHFactor+p-0.99999
    p=0.99999
    message("p out of bounds")
  }

  if(q>0.99999){
    addLHFactor=addLHFactor+q-0.99999
    q=0.99999
    message("q out of bounds")
  }
  
  if(psy<0.00001){
    addLHFactor=addLHFactor-psy+0.00001
    psy=0.00001
    message("psy out of bounds")
  }
    
  if(p<0.00001){
    addLHFactor=addLHFactor-p+0.00001
    p=0.00001
    message("p out of bounds")
  }
  
  if(q<0.00001){
    addLHFactor=addLHFactor-q+0.00001
    q=0.00001
    message("q out of bounds")
  }
    
whichCellsSampled=which(is.na(N[])==F&N[]!=0&N[]>=nMin&N[]<=nMax) #List the cell numbers that should be used for likelihood estimate (Has been sampled at least nMin but no more than nMax times)


likelihoodV=vector() #vector in which log likelihoods for all cell will be accumulated

for (i in whichCellsSampled){
  
ni=N[i]  #Sampling events in cell i
yi=y[i]  #detection in cell i

if(ni>25&ni!=yi&yi!=0){

  #Approximation of the lof of the binomial coefficient - more info in http://math.stackexchange.com/questions/235962/asymptotics-of-binomial-coefficients-and-the-entropy-function 
   
logBinomialCoeff=ni*H(yi/ni)  
  
}else{
  
  logBinomialCoeff=log(choose(n=ni,k=yi))
} 

logSecondA=log(psy)+(log(p)*yi)+(log(1-p)*(ni-yi)) #Log of the probability of dectection if the species is present (psy) and a portion of false negatives(1-p)


logSecondB=log(1-psy)+(log(q)*yi)+(log(1-q)*(ni-yi)) #Log of the probability of dectection if the species is not present (1-psy) and a portion of observations are false negatives (q) 


#Log second part of the binomial (i.e. ordered combinations)
#logxpy from http://stackoverflow.com/questions/5802592/dealing-with-very-small-numbers-in-r
logSecond=logxpy(logSecondA,logSecondB)


likelihoodi=logBinomialCoeff+logSecond
likelihoodV=c(likelihoodV,likelihoodi)

}

likelihoodT=sum(likelihoodV)-addLHFactor #Log likelihood for all the study area (excuding cells with sampling effort outside nMin and nMax). addLHFactor = 0 when psy, p and q take valid values
likelihoodT=likelihoodT*-1 #-log likelihood

return(likelihoodT)

}