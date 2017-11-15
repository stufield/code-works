########################################
###	Stu Field ##########################
###	Department of Biology ##############
###	Colorado State University ##########
###	Fort Collins, CO 80523-1878 ########
###	sgf@colostate.edu ##################
########################################
### Lease Squares fit
########################
#set.seed(101)
x <- seq(1, 10, by=0.1)
a <- 1
b <- 1
y.det <- a-b/x
#y <- jitter((a-b/x) + c, amount=0.1)
y <- rnorm(length(x), mean=y.det, sd=0.05)
mydata <- data.frame(x,y)
plot(mydata, col=2, main="Data of y = a - b/x")
lines(x, y.det)
segments(x, y, x, y.det, col="gray60")

sse.fun <- function(pars, data) {
	x <- data[, "x"]
	y <- data[, "y"]
	a <- pars[1]
	b <- pars[2]
	y.det <- a - b / x
	sum((y-y.det)^2) # sse
}

a.vec <- seq(0, 3, 0.01)
sse.a <- NA
for (i in 1:length(a.vec)) {
	sse.a[i] <- sse.fun(pars=c(a.vec[i], b), data= mydata)
}

b.vec <- seq(0, 2, 0.01)
sse.b <- NA
for ( i in 1:length(b.vec) ) {
   sse.b[i] <- sse.fun(pars=c(a, b.vec[i]), data= mydata)
}
b.vec[which.min(sse.b)]

par(mfrow=c(1, 2))
plot(a.vec, sse.a, type= "l", ylab= "SSE")
plot(b.vec, sse.b, type= "l", ylab= "SSE")

ipars <- c(a=0.5, b=2)
fit   <- optim(par=ipars, fn=sse.fun, data=mydata)
fit$par

plot(mydata)
lines(x, (fit$par[1] - fit$par[2]/x), col=2)
lines(x, y.det, col=1)

##########################
# fit using nls()
# singular gradient matrix at initial parameter estimates
# means your model of OVER-parameterized, use fewer ones
#?nls
############################################################
fit2 <- nls(y ~ a-b/x, start= list(a=1, b=2))
fit2
summary(fit2)
coef(fit2)
CI95.2 <- confint(fit2)
CI95.2
plot(mydata)
lines(x, (coef(fit2)["a"] - coef(fit2)["b"]/x), col=3)
lines(x, y.det, col=1)

##########################
# fit using mle()
##########################
library(bbmle)
### Assume normal error
nlogl <- function(a, b) {
	y.det <- a-b/x 
	n <- length(x) 
	-sum(dnorm(y, mean= y.det, sd= sd(y - y.det) * (n-1)/n, log=TRUE))
}

fit3 <- mle2(nlogl, start= list(a=1, b=2), data= list(x=mydata[,"x"], y=mydata[,"y"]))
fit3
summary(fit3)
coef(fit3)
CI95.3 <- confint(fit3)
CI95.3
plot(mydata)
lines(x, (coef(fit3)["a"] - coef(fit3)["b"]/x), col=4) # MLE
lines(x, y.det, col=1)

rbind(fit$par, coef(fit2), coef(fit3))
par(mfrow=c(1,2))
plot(mydata, col=2, main="Data of y = a - b/x")
lines(x, y.det)
segments(x, y, x, y.det, col="gray60")

plot(mydata)
lines(x, (coef(fit3)["a"] - coef(fit3)["b"]/x), col="navy", lty=1) # MLE
lines(x, (CI95.3["a","97.5 %"] - CI95.3["b","2.5 %"]/x), col="red", lty=2) # upper
lines(x, (CI95.3["a","2.5 %"] - CI95.3["b","97.5 %"]/x), col="red", lty=2) # lower

