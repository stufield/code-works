##########################################
### Power method (aka log-log regression)
##########################################
### Create fake Data
########################
set.seed(101)
x   <- seq(1, 5, 0.1)
ran <- runif(length(x), 0.5, 1.5)
y   <- exp(x) * (ran/x)
plot(x, y, pch=19, cex=0.75, main="X vs. Y", xlab="X", ylab="Y")

#####################
### Regresssion
#####################
fit <- lm(log(y) ~ log(x))
summary(fit)
m <- coef(fit)[2] # slope
b <- coef(fit)[1] # y-int
Res <- residuals(fit)

#####################
### Plot the graphs
#####################
par(mfrow=c(1, 3))
plot(x, y, pch=19, cex=0.75, main="X vs. Y", xlab="X", ylab="Y")
curve(exp(b)*(x^m), from = 0.1, to= 5, col=2, lwd= 2, lty=4, add=T)
plot(log(x), log(y), pch=19, cex=0.75); abline(fit)
plot(x, Res, pch=19, cex=0.75); abline(h=0)

#################
# try with nls
# for a nonlinear fit
################
fit2 <- nls(y ~ b*x^m, start=list(b=1, m=1))
b2   <- coef(fit2)[1]
m2   <- coef(fit2)[2]
plot(x, y, pch=19, cex=0.75, main="X vs. Y", xlab="X", ylab="Y")
curve(b2*x^m2, from=0.1, to=5, lty=4, col=2, lwd=2, add=TRUE)

#################
# try with mle2 ??????
################

fun <- function(x, b=1, m=1) { b * x^m }
mle2(minuslogl=fun, parameters=list(b, m))
mle2(formula=y ~ b*x^m) # with defaults
calc_mle2_function(formula=y ~ b*x^m, parameters=list(b, m), start=list(b=1, m=1))

