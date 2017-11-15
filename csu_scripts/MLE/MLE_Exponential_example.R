#############################################
######### SIMPLE 2 PARAMETER MLE ############
###### BASED ON EXPONENTIAL FUNCTION ########
#############################################
rm(list=ls())
#############################################
### Get the right Exponential function
#############################################
par(mfrow=c(2,2))
a= 0.6; b= 5.5
curve(b*(exp(x*a)), from=0, to=10)    ### I'll just try this one for now
curve((b*exp(-x*a)), from=0, to=10)
curve((1-(b*exp(-x*a))), from=0, to=10)
curve((1-(b*exp(x*a))), from=0, to=10)

#############################################
### Create data to fit based on Exp Fn
#############################################
x = round(runif(25, 0, 5),2); x
y1 = b*exp(x*a); y           ### The function used to make the data

y1 = b*exp(-x*a)
#################################################
### Add some randomness to simulate 'real' data
#################################################
random = runif(1000, 0.75, 1.25)
mix = sample(random, length(y1), replace=T)
y = y1*mix; y

#############################################
### Take a look at the 'data' we want to fit
#############################################
x = c(3.28,2.58,3.80,1.22,3.94,2.91,4.35,1.16,3.10,1.63,4.01,1.84,2.00,2.85,2.40,
      3.55,4.53,1.13,4.49,1.68,3.67,1.26,1.41,2.18,2.96)
y = c(38.91,25.60,66.03,11.19,50.13,30.13,90.77,9.34,37.57,16.19,52.06,14.76,15.19,
      23.77,20.22,39.66,70.73,10.16,62.21,16.69,40.09,10.35,11.18,17.70,39.08)
      
plot(x, y)

######################################
### One parameter example: Exp Fn 
######################################
rvec = seq(0, 1, by=0.01)
par(mfrow=c(1,3))

plot(rvec, dexp(1.5, rate=rvec, log=F))
max=rvec[which.max(dexp(1.5, rate=rvec, log=F))]
abline(v=max)

plot(rvec, -dexp(1.5, rate=rvec, log=F))
min=rvec[which.min(-dexp(1.5, rate=rvec, log=F))]
abline(v=min)

plot(rvec, -dexp(1.5, rate=rvec, log=T))
min=rvec[which.min(-dexp(1.5, rate=rvec, log=T))]
abline(v=min)

######################################
### Now to let mle2 do the work 
######################################
library(bbmle)

n.LogLik = function(a, x){
    -dexp(x, rate=a, log=T)
}

Est = mle2(n.LogLik, start=list(a= 0.5), data=list(x= 1.5))
summary(Est)
coef(Est)
confint(Est)


####################################################
### Two parameter example: Exp Fn:  y=b*exp(x*a)
####################################################
mydata= data.frame(x = c(3.28,2.58,3.80,1.22,3.94,2.91,4.35,1.16,3.10,1.63,4.01,1.84,2.00,2.85,2.40,
      3.55,4.53,1.13,4.49,1.68,3.67,1.26,1.41,2.18,2.96),
      y = c(38.91,25.60,66.03,11.19,50.13,30.13,90.77,9.34,37.57,16.19,52.06,14.76,15.19,
      23.77,20.22,39.66,70.73,10.16,62.21,16.69,40.09,10.35,11.18,17.70,39.08))

plot(y~x, data=mydata)

##############################################
### Use mle2 to calculate parameters a & b
############# a = 0.6; b = 5.5 ###############
##############################################
n.LogLik = function(a, b, x, y){
    -sum(dexp(y, rate= b*exp(x*a)), log=T)
}
### Here's where the trouble starts

Est = mle2(n.LogLik, start=list(a= 0.3, b= 7), data=list(mydata))
summary(Est)
confint(Est)
coef(Est)
