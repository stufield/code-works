############################
# Nonlinear Fitting with
# the nls() function
#############################
### Create fake Data (jittered logistic growth)
########################
x <- seq(1, 26, 2)
K <- 0.4
m <- 0.4
c <- 15
set.seed(125)
y.det <- K / (1 + exp(m * (c - x)))
#y <- jitter(y.det, amount=0.033)
y <- rnorm(length(y.det), mean=y.det, sd=0.03)
#logy = log(y)
plot(x, y, main="Nonlinear Least Squares", xlab= "time", ylab="Proportion Infected")

#######################
### check for normality
#######################
ks.test(y, pnorm)

##########################
# fit using nls()
# Non-linear Least Squares
##########################
fit <- nls(y ~ K/(1+exp(m*(c-x))), start= list(K=1, m=1, c=5))
summary(fit)
K1 <- coef(fit)["K"]
m1 <- coef(fit)["m"]
c1 <- coef(fit)["c"]
CI95 <- confint(fit)

plot(x, y, main="Nonlinear Least Squares", pch=19, xlab= "time", ylab="Proportion Infected")
curve(K1/(1+exp(m1*(c1-x))), from=0, to=max(x), lty= 1, col=4, lwd=2, add=TRUE)
curve(CI95[1,1]/(1+exp(CI95[2,2]*(CI95[3,2]-x))), from=0, to=max(x), lty= 4, col=2, lwd=2, add=TRUE)
curve(CI95[1,2]/(1+exp(CI95[2,1]*(CI95[3,1]-x))), from=0, to=max(x), lty= 4, col=2, lwd=2, add=TRUE)


########################
### Doesn't work: why?
########################
xvec <- seq(1, 26, by=0.5)
matlines(xvec, predict(fit, newdata=data.frame(x=xvec), interval="prediction"),
	      col=c(1,2,2), lty=c(1,2,2))

matlines(xvec, predict(fit, newdata=data.frame(x=xvec), interval="confidence"),
         col=c(1, 4, 4), lty=c(1, 2, 2))

############################
# Nonlinear Fitting with
# Maximum Likelihood (MLE)
#############################
#### Create function to maximize
nloglik = function(pars) {
  Y <- pars[1]/(1+exp(pars[2]*(pars[3]-x)))
  nloglik <- -sum(dnorm(y, mean= mean(Y), sd= sd(Y), log= TRUE))
}

MaxLik <- nlm(nloglik, p= c(1, 1, 5), hessian= TRUE, print.level= 1, steptol= 0.0000001)
MaxLik

# Calculate values of y from x and parameter estimates
K2 <- MaxLik$estimate[1]; K2
m2 <- MaxLik$estimate[2]; m2
c2 <- MaxLik$estimate[3]; c2

plot(x, y, main="MLE #1", pch=19, xlab= "time", ylab="Proportion Infected")
curve(K2/(1+exp(m2*(c2-x))), from=0.1, to=max(x), lty= 4, col=3, lwd=2, add=TRUE)

###############################################
### SE estimates from Bolker (2009), pg. 199.
###############################################
#se.a4 <- sqrt(solve(MLfit$hessian[1,1]))
#se.b4 <- sqrt(solve(MLfit$hessian[2,2]))

#par(mfrow=c(1,3))
#plot(Data$DBH, Data$pLA, main= "Max Like Est. 1", ylab="Projected LA", xlab="DBH (cm)") 
#curve(a4*x^b4, from= 0.1, to= 55, lty=  4, col= 2, lwd= 2, add=TRUE)
#curve((a4+1.96*se.a4) * x^(b4+1.96*se.b4), from= 0.1, to= 55, lty= 3, col= 2, lwd= 1, add=TRUE)
#curve((a4-1.96*se.a4) * x^(b4-1.96*se.b4), from= 0.1, to= 55, lty= 3, col= 2, lwd= 1, add=TRUE)
#text(15, 175, paste("pLA = a * DBH^b"), cex= 1, col= 1, font= 1)
#text(15, 200, paste("a =", format(a4,digits=2), "(", format(1.96*se.a4, digits=2), "+/- CI95)"), cex= 1, col= 1, font= 1)
#text(15, 225, paste("b =", format(b4,digits=2), "(", format(1.96*se.b4, digits=2), "+/- CI95)"), cex= 1, col= 1, font= 1)

#######################
##############################
# try with mle2 (Bolker 2008)
##############################
#######################
library(bbmle)
loglik2 = function(K, m, c) {
  Ymean <- K / (1 + exp(m * (c-x)))
  n <- length(x)
  #-sum(dgamma(y, shape=shape, scale= Ymean/shape, log= TRUE))
  -sum(dnorm(y, mean= mean(Ymean), sd= sd(y-Ymean)*(n-1)/n, log= TRUE))
  #-sum(dunif(y, min= min(Ymean), max= max(Ymean), log= TRUE))
}

MaxLik2 <- mle2(loglik2, start= list(K=1, m=1, c= 10), method="Nelder-Mead", data=list(x=x, y=y))
coef(MaxLik2)

K3 <- coef(MaxLik2)["K"]
m3 <- coef(MaxLik2)["m"]
c3 <- coef(MaxLik2)["c"]

plot(x, y, main="MLE #2", pch=19, xlab= "time", ylab="Proportion Infected")
curve(K3/(1+exp(m3*(c3-x))), from=0.1, to=max(x), lty= 4, col=4, lwd=2, add=TRUE)

#######################
##############################
### Comparison of the three
##############################
#######################
plot(x, y, main="Comparison of Various Fitting Procedures", pch=19, xlab= "time", ylab="Proportion Infected")
curve(K1/(1+exp(m1*(c1-x))), from=0.1, to=max(x), lty= 4, col= 2, lwd= 2, add=TRUE)
curve(K2/(1+exp(m2*(c2-x))), from=0.1, to=max(x), lty= 4, col= 3, lwd= 2, add=TRUE)
curve(K3/(1+exp(m3*(c3-x))), from=0.1, to=max(x), lty= 4, col= 4, lwd= 2, add=TRUE)
legend("topleft", bg="gray95", legend=c("Nonlinear L-S (nls)", "MLE #1 (nlm)", "MLE #2 (mle2)"), col=c(2,3,4), lty=4)

