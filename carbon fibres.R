#  (Power Hamza Distribution)The data set is on the breaking strength of carbon fibres of 50 mm length (GPa). 
The data has been previously used by [20] and [21]. The data is as follows:#


#  Alpha Skew Logistic
library(GenSA)
x<-c(0.39, 0.85, 1.08, 1.25, 1.47, 1.57, 1.61, 1.61, 1.69, 1.80, 1.84, 
1.87, 1.89, 2.03, 2.03, 2.05, 2.12, 2.35, 2.41, 2.43, 2.48, 2.50, 
2.53, 2.55, 2.55, 2.56, 2.59, 2.67, 2.73, 2.74, 2.79, 2.81, 2.82, 
2.85, 2.87, 2.88, 2.93, 2.95, 2.96, 2.97, 3.09, 3.11, 3.11, 3.15, 
3.15, 3.19, 3.22, 3.22, 3.27, 3.28, 3.31, 3.31, 3.33, 3.39, 3.39, 
3.56, 3.60, 3.65, 3.68, 3.70, 3.75, 4.20, 4.38, 4.42, 4.70, 4.90)
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
x<-c(0.39, 0.85, 1.08, 1.25, 1.47, 1.57, 1.61, 1.61, 1.69, 1.80, 1.84, 
1.87, 1.89, 2.03, 2.03, 2.05, 2.12, 2.35, 2.41, 2.43, 2.48, 2.50, 
2.53, 2.55, 2.55, 2.56, 2.59, 2.67, 2.73, 2.74, 2.79, 2.81, 2.82, 
2.85, 2.87, 2.88, 2.93, 2.95, 2.96, 2.97, 3.09, 3.11, 3.11, 3.15, 
3.15, 3.19, 3.22, 3.22, 3.27, 3.28, 3.31, 3.31, 3.33, 3.39, 3.39, 
3.56, 3.60, 3.65, 3.68, 3.70, 3.75, 4.20, 4.38, 4.42, 4.70, 4.90)
skewl <- function(a) {
-sum(log(2*dlogis(x, location = a[1], scale = a[2], log = FALSE)/(1+exp(-a[3]*(x-a[1])/a[2]))))
}
lower <- c(-10,0.000000000001,-30)
upper <- c(190,15,30)
tol <- 1e-8
global.min <- -Inf
out <- GenSA(lower = lower, upper = upper, fn = skewl,
control=list(threshold.stop=global.min+tol,verbose=TRUE))
out[c("value","par","counts")]
#######################################






#  Logistic
library(GenSA)
x<-c(0.39, 0.85, 1.08, 1.25, 1.47, 1.57, 1.61, 1.61, 1.69, 1.80, 1.84, 
1.87, 1.89, 2.03, 2.03, 2.05, 2.12, 2.35, 2.41, 2.43, 2.48, 2.50, 
2.53, 2.55, 2.55, 2.56, 2.59, 2.67, 2.73, 2.74, 2.79, 2.81, 2.82, 
2.85, 2.87, 2.88, 2.93, 2.95, 2.96, 2.97, 3.09, 3.11, 3.11, 3.15, 
3.15, 3.19, 3.22, 3.22, 3.27, 3.28, 3.31, 3.31, 3.33, 3.39, 3.39, 
3.56, 3.60, 3.65, 3.68, 3.70, 3.75, 4.20, 4.38, 4.42, 4.70, 4.90
)
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


#	Beta Skew Logistic
library(GenSA)
x<-c(0.39, 0.85, 1.08, 1.25, 1.47, 1.57, 1.61, 1.61, 1.69, 1.80, 1.84, 
1.87, 1.89, 2.03, 2.03, 2.05, 2.12, 2.35, 2.41, 2.43, 2.48, 2.50, 
2.53, 2.55, 2.55, 2.56, 2.59, 2.67, 2.73, 2.74, 2.79, 2.81, 2.82, 
2.85, 2.87, 2.88, 2.93, 2.95, 2.96, 2.97, 3.09, 3.11, 3.11, 3.15, 
3.15, 3.19, 3.22, 3.22, 3.27, 3.28, 3.31, 3.31, 3.33, 3.39, 3.39, 
3.56, 3.60, 3.65, 3.68, 3.70, 3.75, 4.20, 4.38, 4.42, 4.70, 4.90)
skewl <- function(a) {
-sum(log(dlogis(x, a[1],  a[2], log = FALSE)*(((1-(a[3]*((x-a[1])/a[2])^3))^2+1)/(2+((31*pi^6*a[3]^2)/21)))))
}
lower <- c(-10,0,-30)
upper <- c(190,30,30)
tol <- 1e-10
global.min <- -Inf
out <- GenSA(lower = lower, upper = upper, fn = skewl,
control=list(threshold.stop=global.min+tol,verbose=TRUE))
out[c("value","par","counts")]


#	Alpha Beta Skew Logistic
library(GenSA)
x<-c(0.39, 0.85, 1.08, 1.25, 1.47, 1.57, 1.61, 1.61, 1.69, 1.80, 1.84, 
1.87, 1.89, 2.03, 2.03, 2.05, 2.12, 2.35, 2.41, 2.43, 2.48, 2.50, 
2.53, 2.55, 2.55, 2.56, 2.59, 2.67, 2.73, 2.74, 2.79, 2.81, 2.82, 
2.85, 2.87, 2.88, 2.93, 2.95, 2.96, 2.97, 3.09, 3.11, 3.11, 3.15, 
3.15, 3.19, 3.22, 3.22, 3.27, 3.28, 3.31, 3.31, 3.33, 3.39, 3.39, 
3.56, 3.60, 3.65, 3.68, 3.70, 3.75, 4.20, 4.38, 4.42, 4.70, 4.90)
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



#Flexible Alpha Skew Logistic
library(GenSA)
x<-c(0.39, 0.85, 1.08, 1.25, 1.47, 1.57, 1.61, 1.61, 1.69, 1.80, 1.84, 
1.87, 1.89, 2.03, 2.03, 2.05, 2.12, 2.35, 2.41, 2.43, 2.48, 2.50, 
2.53, 2.55, 2.55, 2.56, 2.59, 2.67, 2.73, 2.74, 2.79, 2.81, 2.82, 
2.85, 2.87, 2.88, 2.93, 2.95, 2.96, 2.97, 3.09, 3.11, 3.11, 3.15, 
3.15, 3.19, 3.22, 3.22, 3.27, 3.28, 3.31, 3.31, 3.33, 3.39, 3.39, 
3.56, 3.60, 3.65, 3.68, 3.70, 3.75, 4.20, 4.38, 4.42, 4.70, 4.90)
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










2*dlogis(x, location = a[1], scale = a[2], log = FALSE)/(1+exp(-a[3]*(x-a[1])/a[2]))*(1/((2+(a[4]*(45-10*pi^2+7*pi^4))/30)/2))*(1+a[4]*(((((x-a[1])/a[2])^2-1)^2+2)/4))




