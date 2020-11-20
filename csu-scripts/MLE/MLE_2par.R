##################################
### Applied Bayesian Statistics
### by Scott Lynch
########################
require(bbmle)
require(emdbook)

set.seed(200)
data <- rnorm(n=30, mean=20, sd=5)

N.LogLik <- function(x, mu, sigma) {
	-sum(dnorm(x, mean=mu, sd=sigma, log=TRUE))
}

MLE.fit <- mle2(N.LogLik, start=list(mu=10, sigma=3), data=list(x=data), method="L-BFGS-B")

summary(MLE.fit)
pars <- coef(MLE.fit)
pars
c(mean(data),sd(data))
CI95 <- confint(MLE.fit)
CI95
LL <- logLik(MLE.fit)
LL
exp(LL)


##################
#### Plotting ####
##################
curve3d(-N.LogLik(x=data, mu=x, sigma=y), from=c(18,3), to=c(25,7), xlab="mu", ylab="sigma", main="3D Likelihood Surface", col="navy", zlab= "log-Likelihood", sys3d="persp", theta= 45, phi= 30, expand= 0.7, ltheta= 10, shade=0.4, box=TRUE, ticktype="detailed", nticks=5, cex.axis=0.5)

plot <- curve3d(N.LogLik(x=data, mu=x, sigma=y), from=c(18,3), to=c(25,7), xlab="mu", ylab="sigma", main="2D Likelihood Surface", zlab= "log-Likelihood", sys3d="image")

contour(plot$x, plot$y, plot$z, add = TRUE, col=1)
contour(plot$x, plot$y, plot$z, levels= -LL + qchisq(0.95,2)/2, label = "95%", add=TRUE, col=4)
points(coef(MLE.fit)["mu"], coef(MLE.fit)["sigma"], pch=19, col="blue") 
text(coef(MLE.fit)["mu"], coef(MLE.fit)["sigma"], labels="mle", pos=1, cex=0.7) 
points(20, 5, pch=19, col="green")
text(20, 5, labels="True", pos=3, cex=0.7)
points(mean(data), sd(data), pch=19, col="yellow") 
text(mean(data), sd(data), labels="Classic", pos=3, cex=0.7)
abline(v=CI95["mu", ], lty=2, lwd=0.5); abline(h=CI95["sigma", ], lty=2, lwd=0.5)
