##################################
### Calculating DBH >- LA
####################################
rm(list=ls())
############################################
### Regresssion from Sala
### Power method (aka log-log regression) for y=a*x^b
############################################
Data <- read.csv("Sala DBH.csv")

######  Linear fit
fit <- glm(pLA ~ DBH, data= Data)
summary(fit)
par(mfrow=c(1,2))
plot(Data$DBH, Data$pLA, main = "Linear"); abline(fit, col=2, lty=4, lwd=2) # Not great, try transformation
plot(Data$DBH, residuals(fit)); abline(h=0)


####  Log 10 transform
logfit1 <- glm(log10(pLA) ~ DBH, data= Data)
summary(logfit1)

m1 = coef(logfit1)[2] # slope of fit1
b1 = coef(logfit1)[1] # y-int of fit1

#####################
### Plot the graphs
#####################
par(mfrow=c(1,3))
plot(Data$DBH, Data$pLA, main = "Log 10"); curve(10^(b1 + m1*x), from = 0, to= 55, , lty= 4, col=2, lwd=2, add=T)
plot(Data$DBH, log10(Data$pLA)); abline(logfit1)
plot(Data$DBH, residuals(logfit1)); abline(h=0)

####  Natural log transformation
logfit2 <- glm(log(pLA) ~ DBH, data= Data)
summary(logfit2)

m2 = coef(logfit2)[2] # slope of fit2
b2 = coef(logfit2)[1] # y-int of fit2

#####################
### Plot the graphs
#####################
par(mfrow=c(1,3))
plot(Data$DBH, Data$pLA, main = "Nat. Log"); curve(exp(b2 + m2*x), from = 0, to= 55, , lty= 4, col=2, lwd=2, add=T)
plot(Data$DBH, log(Data$pLA)); abline(logfit2)
plot(Data$DBH, residuals(logfit2)); abline(h=0)


###############################
# Nonlinear fit using
# nls (least squares) function
################################
nls.fit <- nls(pLA ~ a*DBH^b, start= list(a=1,b=1), data= Data)
summary(nls.fit)
a3 <- coef(nls.fit)[1]
b3 <- coef(nls.fit)[2]

par(mfrow=c(1,2))
plot(Data$DBH, Data$pLA, main = "NLS Power Func"); curve(a3*x^b3, from=0.1, to=55, lty= 4, col=2, lwd=2, add=TRUE)
plot(Data$DBH, residuals(nls.fit)); abline(h=0, col= 2, lwd= 2)


###################################################################
# LIKELIHOOD FUNCTION FOR PARAMETERS TO POWER FUNCTION 
###################################################################
##### sort by values of DBH, just for fun
Data = Data[order(Data$DBH),]

#########################
### Fits Normal dist ####
### Kolmogorov-Smirnov ##
#########################
ks.test(Data$DBH, pnorm)
ks.test(Data$pLA, pnorm)
ks.test(Data$DBH, Data$pLA)

#############################################
# LIKELIHOOD FUNCTION THE OLD FASHINED WAY
#### Using nlm ##############################
###############################
loglik = function(x) {
  LA <- x[1]*Data$DBH^x[2]
  -sum(dnorm(Data$pLA, mean= mean(LA), sd= sd(LA), log= TRUE))
  
}

#################################
### Estimate Parameters using ML
#################################
MLfit <- nlm(loglik,  c(0.5, 1.0), hessian= TRUE, print.level= 1, steptol= 0.0000001); MLfit

# Calculate values of LA from DBH and parameter estimates
a4 <- MLfit$estimate[1]; a4
b4 <- MLfit$estimate[2]; b4
LA.MLfit <- (a4*Data$DBH^b4); LA.MLfit


###############################################
### SE estimates from Bolker (2009), pg. 199.
###############################################
se.a4 <- sqrt(solve(MLfit$hessian[1,1]))
se.b4 <- sqrt(solve(MLfit$hessian[2,2]))

par(mfrow=c(1,3))
plot(Data$DBH, Data$pLA, main= "Max Like Est. 1", ylab="Projected LA", xlab="DBH (cm)") 
curve(a4*x^b4, from= 0.1, to= 55, lty=  4, col= 2, lwd= 2, add=TRUE)
curve((a4+1.96*se.a4) * x^(b4+1.96*se.b4), from= 0.1, to= 55, lty= 3, col= 2, lwd= 1, add=TRUE)
curve((a4-1.96*se.a4) * x^(b4-1.96*se.b4), from= 0.1, to= 55, lty= 3, col= 2, lwd= 1, add=TRUE)
text(15, 175, paste("pLA = a * DBH^b"), cex= 1, col= 1, font= 1)
text(15, 200, paste("a =", format(a4,digits=2), "(", format(1.96*se.a4, digits=2), "+/- CI95)"), cex= 1, col= 1, font= 1)
text(15, 225, paste("b =", format(b4,digits=2), "(", format(1.96*se.b4, digits=2), "+/- CI95)"), cex= 1, col= 1, font= 1)

#######################
##############################
# try with mle2 (Bolker 2008)
##############################
#######################
library(bbmle)
nloglik2 = function(a, b) {
  LA <- a*Data$DBH^b
  -sum(dnorm(Data$pLA, mean= mean(LA), sd= sd(LA), log= TRUE))
}

#nloglik2 = function(a, b) {
#  ymean <- a * x ^ b
#  n <- length(x)
#  -sum(dnorm(y, mean= ymean, sd= sd(y-ymean)*(n-1/n), log= TRUE))
#}

MleFit <- mle2(nloglik2, start= list(a= 0.1,b= 1), data= Data); MleFit
#MleFit <- mle2(nloglik2, start= list(a= 0.1,b= 1), data= list(x=Data$DBH,y=Data$pLA)); MleFit
CI <- confint(MleFit)
a5 <- coef(MleFit)["a"]
b5 <- coef(MleFit)["b"]
LA.MleFit = (a5 * Data$DBH^b5); LA.MleFit

plot(pLA ~ DBH, main= "Max Like Est. 2", data=Data); 
curve(a5*x^b5, from= 0.1, to= 55, lty= 4, col= 2, lwd= 2, add= TRUE)
curve(CI["a","2.5 %"]* x^CI["b","97.5 %"], from= 0.1, to= 55, lty= 2, col= 4, add= TRUE)
curve(CI["a","97.5 %"]* x^CI["b","2.5 %"], from= 0.1, to= 55, lty= 2, col= 4, add= TRUE)
text(25, 200, paste("a =",format(a5,digits=4), ", b =", format(b5, digits= 3)), cex= 1, col= 1, font= 4)


residuals.A2 = Data$pLA - a5 * Data$DBH^b5
plot(Data$DBH, residuals.A2); abline(h= 0, col= 2, lwd= 2)


################################################
### Plot all functions together for comparison
################################################
par(mfrow=c(1,1))
plot(Data$DBH, Data$pLA, main="All Estimates and Functions Compared", ylab="Leaf Area", xlab="Diameter Breast Height"); abline(fit, col=6, lty=4, lwd=2) 
curve(a3*x^b3, from=0.1, to=55, lty= 4, col=2, lwd=2, add=TRUE)
curve(-1+exp(b2 + m2*x), from = 0, to= 55, , lty= 4, col=3, lwd=2, add=TRUE)
curve(1+ 10^(b1 + m1*x), from = 0, to= 55, , lty= 4, col=1, lwd=2, add=TRUE)
curve(a5*x^b5, from=0.1, to=55, lty= 4, col=5, lwd=2, add=TRUE)
text(20,210,paste("Linear:  m =",format(coef(fit)[2],digits=3),", b =",format(coef(fit)[1],digits=3)),cex=1,col=6,font=4)
text(20,190,paste("Log 10:  m =",format(10^m1,digits=3),", b =",format(10^b1,digits=3)),cex=1,col=1,font=4)
text(20,170,paste("Nat Log: m =",format(exp(m2),digits=3),", b =",format(exp(b2),digits=3)),cex=1,col=3,font=4)
text(20,150,paste("Power:   a =",format(a3,digits=3),", b =",format(b3,digits=3)),cex=1,col=2,font=4)
text(20,130,paste("Max Like a =",format(a5,digits=3),", b =",format(b5,digits=3)),cex=1,col=5,font=4)
