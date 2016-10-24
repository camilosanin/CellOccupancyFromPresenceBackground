##################################################################################
#Estimate the log of the sum of x and y when the input is logx and logy###########
#http://stackoverflow.com/questions/5802592/dealing-with-very-small-numbers-in-r##
##################################################################################

#Approximation of the lof of the binomial coefficient - more info in http://math.stackexchange.com/questions/235962/asymptotics-of-binomial-coefficients-and-the-entropy-function 


logxpy = function(lx,ly){
  
  max(lx,ly) + log1p(exp(-abs(lx-ly)))
  
} 
  