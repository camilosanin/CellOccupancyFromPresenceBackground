######################################################################
#Estimate the Binary Entropy Functuion H() that will be used for the##
#aproximation of the log binomail coefficient#########################
######################################################################

#To be used in the approximation of the Log of the binomial coefficient - more info in http://math.stackexchange.com/questions/235962/asymptotics-of-binomial-coefficients-and-the-entropy-function 


H=function(x){
  y=(-x*log(x))-((1-x)*log(1-x))
  return(y)
}