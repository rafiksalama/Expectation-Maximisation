library(VGAM)
p = rbeta(100,10,10)
D = rbinom(100,1000,p)
fit = vglm(cbind(D, 1000-D) ~ 1, betabinomial, trace = TRUE)
param = Coef(fit)
mu = param[1]
rho = param[2]
alpha= mu/rho - mu
beta = alpha/mu - alpha
print(paste(alpha,beta))

logit <- function(x){1/(1+exp(-x))}
X1 = rnorm(100)
X2 = rnorm(100)
d = cbind(rep(1,length(X1)),X1,X2)
fit = vglm(cbind(D, 1000-D) ~ X1+X2, betabinomial, trace = TRUE)
param = Coef(fit)
mu = logit(d%*%as.matrix(c(param[1],param[3:length(param)])))
rho = logit(d%*%as.matrix(c(param[2],param[3:length(param)])))
alpha= mu/rho - mu
beta = alpha/mu - alpha
print(paste(alpha,beta))

#Real Data
setwd("/gpfs2/well/ratcliff/data/rafik/Analysis/temp/")
f = list.files(".")
m = matrix(ncol=length(f),nrow=30817,0)
for(i in 1:length(f))
{
  m[,i] = read.table(f[i])[,1]
}
epi = as.data.frame(matrix(ncol=4,nrow=30817,0))
epi[,1] = read.table("/gpfs2/well/ratcliff/data/rafik/Epigenome/FoldChange/BI.Adult_Kidney.H3K27ac.153.fc.signal_100k.bed")[,4]
epi[,2] = read.table("/gpfs2/well/ratcliff/data/rafik/Epigenome/FoldChange/E086-DNase.fc.signal_100k.bed")[,4]
epi[,3] = read.table("/gpfs2/well/ratcliff/data/rafik/Epigenome/FoldChange/E086-H3K4me1.fc.signal_100k.bed")[,4]
epi[,4] = read.table("/gpfs2/well/ratcliff/data/rafik/Epigenome/FoldChange/E086-H3K4me3.fc.signal_100k.bed")[,4]
epi = replace(epi,epi==".",0)
epi = t(apply(epi,1,as.numeric))
colnames(epi) <- c("H3K27Ac","DNASE","H3K4Me1","H3K4Me3")
D = rowSums(m)
Y = D/1e05
gg = glm(Y ~ H3K27Ac+DNASE+H3K4Me3+H3K4Me1, family="binomial", data=as.data.frame(epi))

d = cbind(rep(1,nrow(epi)),epi)
fit = vglm(cbind(D[n], 1e05-D[n]) ~ H3K27Ac+DNASE+H3K4Me3+H3K4Me1, data = as.data.frame(epi[n,]),betabinomial, trace = TRUE)
param = Coef(fit)
mu = logit(d%*%as.matrix(c(param[1],param[c(3,4,6,5)])))
rho = logit(d%*%as.matrix(c(param[2],param[c(3,4,6,5)])))
alpha= mu/rho - mu
beta = alpha/mu - alpha
print(paste(alpha,beta))

