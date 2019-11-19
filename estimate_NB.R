d = rnbinom(10000,size=20,prob = .001)
mu = mean(d) #size*(1-p)/p, beta = (1-p/p), 
v = var(d) #size*(1-p)/p^2, then 
p = mu/v
size = mu * (p/(1-p)) 
#then size-size*p = mu*p, then 
p = size/(size+mu)
##if p=1-p, then size= mu * (1-p)/p, p*size = mu-mu*p, then p=mu/(mu+size)
plot(density(d))
lines(density(rnbinom(10000,size = size, mu=mu)), col="red")
#ok let us assume that we have a model of the mean that we are fitting, the variance from this model
#identifies 