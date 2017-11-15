############################################################
# 
#  Example non-linear model fitting using optim and mle
# 
#  Consider trying it with bbmle::mle2 instead of mle
#
############################################################
library(stats4)
library(MASS)   # for confint.nls ... which doesn't seem to work well here.

### Define Model Parameters
a <- 1
b <- 2
r <- -0.7
ptrue <- c(a=a, b=b, r=r)
t <-seq(0, 10, length=500)  # our independent variable values (time)

### Define the form of the model
model <- function(p, x) {
   p["a"] + (p["b"] - p["a"]) * exp(p["r"] * x)
}

###  add some noise with mean zero and stdev s
s <- abs(rnorm(1, 0.3, 0.1))

###  Generate fake data from our model
y <- model(ptrue, t) + rnorm(length(t), 0, s)

###  Plot in two frames - a smoothed curve and the fitted curve...
par(mfrow=c(1, 2))
plot(t, y, type="p", pch=19) # the data
fit <- smooth.spline(t, y) # smoothed data...
lines(fit, lwd=2)
legend("topright", legend=c("smooth.spline of the data"), col=c("black"), lty=1, lwd=2)

# Next, our function and the optimization...
FitFunc <- function(p) {
  mean((model(p, t) - y)^2) # regular ol' mean squared error ... a forgiveable first approach! ;)
}

# guess initial params
parms0 <- c(a=0.5, b=1.5, r=-1)    # give the optimizer an initial guess
pfit   <- optim(parms0, FitFunc)   # then let it run...

# Plot the fitted function (solid) and the true function (dashed)
plot(t, y, type="p", pch=21)
lines(t, model(pfit$par, t), type="l", lty=1, col="red", lwd=2)
lines(t, model(ptrue, t), type="l", lty=2, col="black", lwd=2)


##########################################################################
###########################  Next, fit using MLE  ########################
# This will allow things like confidence intervals, etc.  Always good!
# To do this we'll need to optimize the negative log liklihood...
# Assume errors are normal. If they aren't normal, use the best 
# looking distribution?????? (In help, used dpois instead of normal. ???)
minuslogL <- function(a, b, r, sigma) {  
   p    <- c(a=a, b=b, r=r)
   errs <- model(p,t)-y
   -sum(dnorm(errs, mean=0, sd=sigma, log=TRUE))  # log=T gives log(f) not f
}

# send to mle, which uses optim to maximize the likelihood...
pmle <- mle(minuslogL, start=list(parms0["a"], parms0["b"], parms0["r"],
            sigma=sqrt(var(y)*mean(1/y))))

parmsMLE <- coef(pmle)  # note sigma is in here as well :)

lines(t,model(parmsMLE,t)  , type="l", lty=2, col="green", lwd=2)
legend("topright",legend=c("True Params", "Least Squares", "MLE"),
       col=c("black", "red", "green"), lty=c(2, 1, 2), lwd=c(2, 2, 2))

summary(pmle)  # Summary statistics
logLik(pmle)   # Log(liklihood) value
vcov(pmle)     # variance covariance matrix
#confint(pmle)  # Confidence Intervals... sometimes.
#confint.nls(pmle)  # Confidence Intervals... sometimes.

