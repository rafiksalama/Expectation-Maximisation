#OK, let us start the EM of determining 2 guassian distribution parameter sets.

#Generate Data
#a = rnbinom(100,size=1,mu=100)
#b = rnbinom(1000,size=5,mu=10)
#data = c(a,b)
data = rbeta(10000,0.5,0.5)
#E Step

m = c(10,1)
s = c(1,10)
weightsA = runif(1)
weightsB = 1-weightsA
#M Step, 
#calculate the likelihood of the data and assign membership
zA = zB = l = vector()
for(j in 1:500)
{
  #E Step, essentially calculate the values of the alpha for the two components
  #P(zij = 1) = P(C=i|data,parameters)
  for(i in 1:length(data))
  {
    zA[i] = dbeta(data[i],s[1],m[1])*weightsA
    zB[i] = dbeta(data[i],s[2],m[2])*weightsB
    denom = zA[i]+zB[i]
    zA[i] = zA[i]/denom
    zB[i] = zB[i]/denom
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
  
  varA = sum(zA*((data-meanA)*t(data-meanA)))/sum(zA)
  varB = sum(zB*((data-meanB)*t(data-meanB)))/sum(zB)
  
  fA = varA+meanA^2-meanA
  fB = varB+meanB^2-meanB
  
  s[1] = -fA*meanA/varA
  s[2] = -fB*meanB/varB
  
  m[1] = fA*(meanA-1)/varA
  m[2] = fB*(meanB-1)/varB

  #M Step, We need to calculate the mean of the data according to the new weights 
  #m = c(meanA,meanB)
  #s = c(sdA,sdB)
  print(m)
  print(s)
}

plot(density(data))
lines(density(rbeta(100000,s[1],m[1])), col="blue")
lines(density(rbeta(100000,s[2],m[2])), col="red")

