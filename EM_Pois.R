#OK, let us start the EM of determining 2 guassian distribution parameter sets.

#Generate Data
a = rpois(1000,lambda =100)
b = rpois(100,lambda =1)
data = c(a,b)
#E Step

m = c(1,5)
s = c(1,2)
weightsA = 0.5
weightsB = 0.5
#M Step, 
#calculate the likelihood of the data and assign membership
zA = zB = l = vector()
for(j in 1:10)
{
  #E Step, essentially calculate the values of the alpha for the two components
  #P(zij = 1) = P(C=i|data,parameters)
  for(i in 1:length(data))
  {
    zA[i] = log(dpois(data[i],lambda  = m[1]))*weightsA
    zB[i] = log(dpois(data[i],lambda  = m[2]))*weightsB
    denom = ((exp(zA[i]))+(exp(zB[i])))
    zA[i] = exp(zA[i])/denom
    zB[i] = exp(zB[i])/denom
  }
  
  #M Step, first we calculate the new weights
  
  weightsA = (1/length(data)) * sum(zA)
  weightsB = (1/length(data)) * sum(zB)
  
  print(c(weightsA,weightsB))
  #Now we assume that part of the data given is coming from component 1 and the other part from component 2. This is a bit
  #more complicated than assuming like in Kmeans that it definitely came from the class 1 or class 2, but the same concept apply
  
  meanA = sum(zA*data)/sum(zA) # If you think that the weight is binary, this will be exactly the mean for the ones with weight =1
  meanB = sum(zB*data)/sum(zB)
  
  #Ok now for the SD, we use the calculated meanA and meanB for our case
  #remember that the variance equation is sqrt(sum(x-mean(x))^2/N)
  
  m = c(meanA,meanB)

  #M Step, We need to calculate the mean of the data according to the new weights 
  #m = c(meanA,meanB)
  #s = c(sdA,sdB)
  print(m)
}


