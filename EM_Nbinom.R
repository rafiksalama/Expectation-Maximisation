#OK, let us start the EM of determining 2 guassian distribution parameter sets.

#Generate Data
a = rnbinom(100,size=1,mu=100)
b = rnbinom(1000,size=5,mu=10)
data = c(a,b)

traffic = fread("~/Downloads/TEST_TRAFFIC.csv")
traffic$date = anytime(traffic$date)
traffic = traffic %>% mutate(day=day(date), year=year(date),month=month(date), hour=hour(date), minute=minute(date))
daily = traffic %>% group_by(year,month,day,hour) %>% summarise(value=sum(value))
data = round(daily$value)
#E Step

m = c(1,5)
s = c(1,2)
weightsA = 0.5
weightsB = 0.5
#M Step, 
#calculate the likelihood of the data and assign membership
zA = zB = l = vector()
for(j in 1:500)
{
  #E Step, essentially calculate the values of the alpha for the two components
  #P(zij = 1) = P(C=i|data,parameters)
  for(i in 1:length(data))
  {
    zA[i] = dnbinom(data[i],size = s[1],mu = m[1])*weightsA
    zB[i] = dnbinom(data[i],size = s[2],mu = m[2])*weightsB
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
  
  beta = c(varA/meanA,varB/meanB)
  for(x in 1:length(beta)) if(beta[x] < 1) beta[x]=2 
  print(beta)
  
  s = c(meanA/(beta[1]-1),meanB/(beta[2]-1))
  m = c(meanA,meanB)

  #M Step, We need to calculate the mean of the data according to the new weights 
  #m = c(meanA,meanB)
  #s = c(sdA,sdB)
  print(m)
  print(s)
}
plot(density(data))
lines(density(rnbinom(10000,size=s[1],mu=m[1])), col="red")
lines(density(rnbinom(10000,size=s[2],mu=m[2])), col="blue")

