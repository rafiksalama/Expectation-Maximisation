#OK, let us start the EM of determining 2 guassian distribution parameter sets.

#Generate Data
#I have the dependent and independent variables, but not the coefficients and we assume two mixtures of two coeficients
x = cbind(rep(1,100),rnorm(100,2,3),rnorm(100))
B = rbind(c(1,2,3),c(0.5,4,5))
y = c(x[1:20,]%*%(B[1,]),x[21:100,]%*%(B[2,]))
#Y = Bx + B0 and hence , B0 = Y when X is near zero
#We will always assume that the mean of Y is simply calculated from the mean of the glm given the x
#We will then work out the maximum likelihood parameters of the distributin
#E Step
B = rbind(c(2,2,2),c(1,1,1))
weightsA = runif(1)
weightsB = 1-weightsA
#M Step, 
#calculate the likelihood of the data and assign membership
zA = zB = l = vector()
for(j in 1:100)
{
  #E Step, essentially calculate the values of the alpha for the two components
  #P(zij = 1) = P(C=i|data,parameters)
  for(i in 1:length(y))
  {
    zA[i] = log(dnorm((y[i]-(x[i,]%*%(B[1,]))),mean = 0,1))*weightsA
    zB[i] = log(dnorm((y[i]-(x[i,]%*%(B[2,]))),mean = 0,1))*weightsB
    denom = (exp(zA[i])+exp(zB[i]))
    zA[i] = exp(zA[i])/denom
    zB[i] = exp(zB[i])/denom
  }
  
  #M Step, first we calculate the new weights
  
 weightsA = (1/length(y)) * sum(zA)
 weightsB = (1/length(y)) * sum(zB)
  
  #Now we assume that part of the data given is coming from component 1 and the other part from component 2. This is a bit
  #more complicated than assuming like in Kmeans that it definitely came from the class 1 or class 2, but the same concept apply
  
   # If you think that the weight is binary, this will be exactly the mean for the ones with weight =1
  
  #my = c(sum(zA*y)/sum(zA),sum(zB*y)/sum(zB))
  #mx = c(sum(zA*x)/sum(zA),sum(zB*x)/sum(zB))
  #B = c(sum(((zA*x)-mx[1])*((zA*y)-my[1]))/sum(((zA*x)-mx[1])^2),sum(((zB*x)-mx[2])*((zB*y)-my[2]))/sum(((zB*x)-mx[2])^2))
  #You could otherwise just use the normal glm fit functions available
  xA = x[,-1]*zA
  yA = y*zA
  xB = x[,-1]*zB
  yB = y*zB
  B = rbind(coef(lm(yA ~ xA)),coef(lm(yB ~ xB)))
  
  #mBo = c(sum,sum(zB*(x-mx[2])*(y-my[2]))/sum(zB*(x-mx[2])^2))
    
  #Ok now for the SD, we use the calculated meanA and meanB for our case
  #remember that the variance equation is sqrt(sum(x-mean(x))^2/N)
  
  #s = c(sqrt(0.5*sum(zA*(x^2))/sum(zA)),sqrt(0.5*sum(zB*(x^2))/sum(zB)))
  
  #M Step, We need to calculate the mean of the data according to the new weights 
  #print(c(weightsA,weightsB))
  print(B)
  #print(my)
  #print(mx)
  #print(s)
}


