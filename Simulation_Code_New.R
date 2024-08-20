n=100
x1=array(0,dim=c(n))
mu=0
beta=1
lambda=1.5
alpha=1.5
sl<-function(y){

2*dlogis(y, mu, beta, log = FALSE)/(1+exp(-lambda*(y-mu)/beta))
}

for (i in 1:n)
{
M=2
U1=rlogis(1,mu,beta)
U2=runif(1,0,1)
r=(sl(U1)/dlogis(U1,mu,beta)*M)

while (U2>r)

{
U1=rlogis(1,mu,beta)
U2=runif(1,0,1)
r=(sl(U1)/dlogis(U1,mu,beta)*M)
}

x1[i]=U1
}
x1

hist(x1)







x2=rlogis(100,mu,beta)


x3=(2/(2+alpha))*(x2+(alpha/2)*x1)
x3
hist(x3)




library(GenSA)
x<-x3
skewl <- function(a) {
-sum(log((2/(2+a[4]))*(dlogis(x, location = a[1], scale = a[2], log = FALSE))*(1+a[4]*(1/((1+exp(-a[3]*(x-a[1])/a[2])))))))
}
lower <- c(-10,0.0001,0,0)
upper <- c(10,10,10,10)
tol <- 1e-8
global.min <- -Inf
out <- GenSA(lower = lower, upper = upper, fn = skewl,
control=list(threshold.stop=global.min+tol,verbose=TRUE))
out[c("value","par","counts")]
 




