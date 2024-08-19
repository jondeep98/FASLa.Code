 
library("stats")
 
library("msm") 
 
library("GenSA")
 
library("pracma")

g=100     
aa=array(0,dim=c(g,4))
for( j in 1:g) 
{
n=100    
x1=array(0,dim=c(n))    
mu=0      
sigma=1 
lambda=-1   
alpha=0.5    
     

target=function(x,mu,sigma,lambda,alpha)
{
((2*dlogis(x,  mu, sigma, log = FALSE)/(1+exp(-lambda*(x-mu)/sigma))*(1/((2+(alpha*(45-10*pi^2+7*pi^4))/30)/2))*(1+alpha*(((((x-mu)/sigma)^2-1)^2+2)/4))))
}


proposal=function(x)
{
dlogis(x)
}

metrop=function(N,y0,mu,sigma,lambda,alpha)
{
sample=0
for(i in 1:N)
{
x.star=rlogis(1)
r=((target(x.star,mu,sigma,lambda,alpha))/(target(y0,mu,sigma,lambda,alpha)))*((proposal(y0))/(proposal(x.star)))
u=runif(1)
if(u<min(1,r))
{
y1=x.star
}
else
{
y1=y0
}
sample[i]=y1
y0=y1
}
return(sample)
}


entire.sample=metrop(1000,1.5,0,1,-1,0.5)   
filtered.sample=entire.sample[901:1000]     
filtered.sample

mean(filtered.sample)
plot(density(filtered.sample))
plot


Exp <- function(a) {
-sum(log(2*dlogis(filtered.sample, location = a[1], scale = a[2], log = FALSE)/(1+exp(-a[3]*(filtered.sample-a[1])/a[2]))*(1/((2+(a[4]*(45-10*pi^2+7*pi^4))/30)/2))*(1+a[4]*(((((filtered.sample-a[1])/a[2])^2-1)^2+2)/4))
))
}
lower <- c(-2,0,-2,0)
upper <- c(1,3,1,3)
tol <- 1e-10
global.min <- -Inf
out <- GenSA(lower = lower, upper = upper, fn = Exp,
control=list(threshold.stop=global.min+tol,verbose=TRUE))
out[c("value","par","counts")]


print(out$par)
bb=out$par
cc <- array(out$par, dim=c(1,4))
for( k in 1:4)
{
aa[j,k]=cc[1,k]
}
print(j)
}
aa




mean(aa[,1])
mean(aa[,2])
mean(aa[,3])
mean(aa[,4])
sd(aa[,1])
sd(aa[,2])
sd(aa[,3])
sd(aa[,4])
cov(aa)
cor(aa)
var(aa)
bias1<-mean(aa[,1])-0
bias2<-mean(aa[,2])-1  
bias3<-mean(aa[,3])-(-1)
bias4<-mean(aa[,4])-(0.5)
bias1
bias2
bias3
bias4
a1<-(bias1)^2
a1
a2<-(bias2)^2
a2
a3<-(bias3)^2
a3
a4<-(bias4)^2
a4
var1<-(sd(aa[,1]))^2
var1
var2<-(sd(aa[,2]))^2
var2
var3<-(sd(aa[,3]))^2
var3
var4<-(sd(aa[,4]))^2
var4

MSE1<-var1+a1
MSE1
MSE2<-var2+a2
MSE2
MSE3<-var3+a3
MSE3
MSE4<-var4+a4
MSE4




