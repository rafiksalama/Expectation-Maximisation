#OK, let us start the EM of determining 2 guassian distribution parameter sets.

#Generate Data
a = rnorm(100,2,3)
b = rnorm(100,10,1)
data = c(a,b)
#data=pi
#E Step
m = s = c(.1,.1)
weightsA = runif(1)
weightsB = 1-weightsA
#M Step, 
#calculate the likelihood of the data and assign membership
zA = zB = l = vector()
for(j in 1:1000)
{
  #E Step, essentially calculate the values of the alpha for the two components
  #P(zij = 1) = P(C=i|data,parameters)
  for(i in 1:length(data))
  {
    zA[i] = log(dgamma(data[i],shape=s[1],scale=m[1])+1e-300)*weightsA
    zB[i] = log(dgamma(data[i],shape=s[2],scale=m[2])+1e-300)*weightsB
    denom = (exp(zA[i])+exp(zB[i]))
    zA[i] = exp(zA[i])/denom
    zB[i] = exp(zB[i])/denom
  }
  
  #M Step, first we calculate the new weights
  
  weightsA = (1/length(data)) * sum(zA)
  weightsB = (1/length(data)) * sum(zB)
  
  #Now we assume that part of the data given is coming from component 1 and the other part from component 2. This is a bit
  #more complicated than assuming like in Kmeans that it definitely came from the class 1 or class 2, but the same concept apply
  
  # If you think that the weight is binary, this will be exactly the mean for the ones with weight =1
  meanA = sum(zA*data)/sum(zA)
  meanB = sum(zB*data)/sum(zB)
  
  #Ok now for the SD, we use the calculated meanA and meanB for our case
  #remember that the variance equation is sqrt(sum(x-mean(x))^2/N)
  
  varA = sum(zA*((data-meanA)*t(data-meanA)))/sum(zA)
  varB = sum(zB*((data-meanB)*t(data-meanB)))/sum(zB)
  m[1] = varA/meanA
  m[2] = varB/meanB
  s[1] = meanA/m[1]
  s[2] = meanB/m[2]
  
  #M Step, We need to calculate the mean of the data according to the new weights 
  
  print(c(weightsA,weightsB))
  print(m)
  print(s)
}


