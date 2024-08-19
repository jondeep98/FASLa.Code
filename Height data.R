# Height data (In this subsection we will analyze Professor Hoben Thomas’s data, these data
consist of observations of his students’ heights in inches presented in Table 4.8.)
(Ref: Cruz-Medina, I. Almost Nonparametric and Nonparametric Estimation in Mixture Models. Ph.D. Thesis, The Pennsylvania State
University: State College, PA, USA, 2001.)this is a phd thesis


#	Flexible Alpha Skew Logistic
set.seed(223)
library(GenSA)
x<-c(55.00, 60.00, 60.25, 61.00, 61.75, 62.25, 62.25, 62.63, 62.75, 63.00
,63.25, 63.25, 63.25, 63.25, 63.38, 64.00, 64.25, 64.25, 64.50, 64.75,
64.75, 65.00, 65.00, 65.13, 65.13, 65.17, 65.25, 65.25, 65.25, 65.25,
65.25, 65.50, 65.75, 66.00, 66.00, 66.25, 66.25, 66.25, 66.50, 66.75,
66.75, 66.75, 66.75, 67.00, 67.13, 67.25, 67.38, 67.50, 67.50, 67.75,
67.75, 67.75, 68.13, 68.75 ,69.00, 69.00, 69.25, 69.50, 69.88, 70.00,
70.00, 70.25, 70.38, 71.00, 71.00, 71.25, 71.75)
skewl <- function(a) {
-sum(log(2*dlogis(x, location = a[1], scale = a[2], log = FALSE)/(1+exp(-a[3]*(x-a[1])/a[2]))*(1/((2+(a[4]*(45-10*pi^2+7*pi^4))/30)/2))*(1+a[4]*(((((x-a[1])/a[2])^2-1)^2+2)/4))
))
}
lower <- c(-10,0,-40,0)
upper <- c(190,30,40,30)
tol <- 1e-10
global.min <- -Inf
out <- GenSA(lower = lower, upper = upper, fn = skewl,
control=list(threshold.stop=global.min+tol,verbose=TRUE))
out[c("value","par","counts")]




#	Alpha Beta Skew Logistic
library(GenSA)
x<-c(55.00, 60.00, 60.25, 61.00, 61.75, 62.25, 62.25, 62.63, 62.75, 63.00
,63.25, 63.25, 63.25, 63.25, 63.38, 64.00, 64.25, 64.25, 64.50, 64.75,
64.75, 65.00, 65.00, 65.13, 65.13, 65.17, 65.25, 65.25, 65.25, 65.25,
65.25, 65.50, 65.75, 66.00, 66.00, 66.25, 66.25, 66.25, 66.50, 66.75,
66.75, 66.75, 66.75, 67.00, 67.13, 67.25, 67.38, 67.50, 67.50, 67.75,
67.75, 67.75, 68.13, 68.75 ,69.00, 69.00, 69.25, 69.50, 69.88, 70.00,
70.00, 70.25, 70.38, 71.00, 71.00, 71.25, 71.75)
skewl <- function(a) {
-sum(log(dlogis(x, a[1],  a[2], log = FALSE)*(((1-(a[3]*(x-a[1])/a[2])-(a[4]*((x-a[1])/a[2])^3))^2+1)/(2+((pi^2*a[3]^2)/3)+((14*pi^4*a[3]*a[4])/15)+((31*pi^6*a[4]^2)/21)))))
}
lower <- c(-10,0,-30,-30)
upper <- c(190,30,30,30)
tol <- 1e-10
global.min <- -Inf
out <- GenSA(lower = lower, upper = upper, fn = skewl,
control=list(threshold.stop=global.min+tol,verbose=TRUE))
out[c("value","par","counts")]


#  Alpha Skew Logistic
library(GenSA)
x<-c(55.00, 60.00, 60.25, 61.00, 61.75, 62.25, 62.25, 62.63, 62.75, 63.00
,63.25, 63.25, 63.25, 63.25, 63.38, 64.00, 64.25, 64.25, 64.50, 64.75,
64.75, 65.00, 65.00, 65.13, 65.13, 65.17, 65.25, 65.25, 65.25, 65.25,
65.25, 65.50, 65.75, 66.00, 66.00, 66.25, 66.25, 66.25, 66.50, 66.75,
66.75, 66.75, 66.75, 67.00, 67.13, 67.25, 67.38, 67.50, 67.50, 67.75,
67.75, 67.75, 68.13, 68.75 ,69.00, 69.00, 69.25, 69.50, 69.88, 70.00,
70.00, 70.25, 70.38, 71.00, 71.00, 71.25, 71.75)
skewl <- function(a) {
-sum(log(dlogis(x, location = a[1], scale = a[2], log = FALSE)*(((1-(a[3]*(x-a[1])/a[2])))^2+1)/((2+((a[3]*pi)^2)/3))))       
}
lower <- c(-10,0.0000001,-30)
upper <- c(190,15,30)
tol <- 1e-10
global.min <- -Inf
out <- GenSA(lower = lower, upper = upper, fn = skewl,
control=list(threshold.stop=global.min+tol,verbose=TRUE))
out[c("value","par","counts")]
###################################

# Skew Logistic
library(GenSA)
x<-c(55.00, 60.00, 60.25, 61.00, 61.75, 62.25, 62.25, 62.63, 62.75, 63.00
,63.25, 63.25, 63.25, 63.25, 63.38, 64.00, 64.25, 64.25, 64.50, 64.75,
64.75, 65.00, 65.00, 65.13, 65.13, 65.17, 65.25, 65.25, 65.25, 65.25,
65.25, 65.50, 65.75, 66.00, 66.00, 66.25, 66.25, 66.25, 66.50, 66.75,
66.75, 66.75, 66.75, 67.00, 67.13, 67.25, 67.38, 67.50, 67.50, 67.75,
67.75, 67.75, 68.13, 68.75 ,69.00, 69.00, 69.25, 69.50, 69.88, 70.00,
70.00, 70.25, 70.38, 71.00, 71.00, 71.25, 71.75)
skewl <- function(a) {
-sum(log(2*dlogis(x, location = a[1], scale = a[2], log = FALSE)/(1+exp(-a[3]*(x-a[1])/a[2]))))
}
lower <- c(-10,0.000000000001,-40)
upper <- c(190,15,40)
tol <- 1e-8
global.min <- -Inf
out <- GenSA(lower = lower, upper = upper, fn = skewl,
control=list(threshold.stop=global.min+tol,verbose=TRUE))
out[c("value","par","counts")]
#######################################

length(x)




#  Logistic
library(GenSA)
x<-c(55.00, 60.00, 60.25, 61.00, 61.75, 62.25, 62.25, 62.63, 62.75, 63.00
,63.25, 63.25, 63.25, 63.25, 63.38, 64.00, 64.25, 64.25, 64.50, 64.75,
64.75, 65.00, 65.00, 65.13, 65.13, 65.17, 65.25, 65.25, 65.25, 65.25,
65.25, 65.50, 65.75, 66.00, 66.00, 66.25, 66.25, 66.25, 66.50, 66.75,
66.75, 66.75, 66.75, 67.00, 67.13, 67.25, 67.38, 67.50, 67.50, 67.75,
67.75, 67.75, 68.13, 68.75 ,69.00, 69.00, 69.25, 69.50, 69.88, 70.00,
70.00, 70.25, 70.38, 71.00, 71.00, 71.25, 71.75)
skewl <- function(a) {
-sum(log(dlogis(x, location = a[1], scale = a[2], log = FALSE)))
}
lower <- c(-10,0.000000000001)
upper <- c(190,15)
tol <- 1e-8
global.min <- -Inf
out <- GenSA(lower = lower, upper = upper, fn = skewl,
control=list(threshold.stop=global.min+tol,verbose=TRUE))
out[c("value","par","counts")]




