##################################
### Calculating DBH > LA
############################################
### Regresssion from Sala
### Power method (aka log-log regression) for y=a*x^b
############################################
Data <- read.csv("Sala DBH.csv")
fit  <- lm(P.LA ~ DBH, data=Data)   # regular regression
plot(DBH, P.LA)
abline(fit, col=2, lty=4, lwd=2)    # Not great, try transformation
logfit <- lm(log(P.LA) ~ log(DBH), data= Data)   # log-log regression
summary(fit1)
summary(fit2)
m <- coef(logfit)[2] # slope of fit2
b <- coef(logfit)[1] # y-int of fit2
LA.Res <- residuals(logfit)


#####################
### Plot the graphs
#####################
par(mfrow=c(1, 3))
plot(DBH, P.LA)
curve(exp(b)*(x^m), from = 0, to= 55, , lty= 4, col=2, lwd=2, add=TRUE)
plot(log(DBH), log(P.LA))
abline(fit2)
plot(DBH, LA.Res)
abline(h=0)

###############################
# Nonlinear fit using
# nls (least squares) function
################################
nls.fit <- nls(P.LA ~ a*DBH^b, start=list(a=1, b=1))
summary(nls.fit)
a <- coef(nls.fit)[1]
b <- coef(nls.fit)[2]
plot(DBH, P.LA)
curve(a*x^b, from=0.1, to=55, lty= 4, col=2, lwd=2, add=TRUE)


#################
# try with mle2 ?????? Didn't get this far...
################
fun <- function(b=1, m=1, x) { y = b * x^m }

mle2(minuslogl=fun, parameters= list(b,m))
mle2(formula = y ~ b*x^m) # with defaults
calc_mle2_function(formula= y ~ b*x^m, parameters= list(b,m), start= list(b=1,m=1))

