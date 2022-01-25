###########################
### Function for r.cache
###########################
x = c(0.00001, seq(120, 600, 60))
y = c(0.24, 0.25, 0.255, 0.27, 0.29, 0.33, 0.4, 0.48, 0.6, 0.99)
plot(x,y, ylim=c(0,1))

#########################
### Polynomial fitting
#########################
fit <- lm(y ~ poly(x, degree = 4))
##### OR ###########
fit <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4))
curve(fit$coef[1] + fit$coef[2]*x + fit$coef[3]*x^2 + fit$coef[4]*x^3 + fit$coef[5]*x^4, from=0, to= 600, ylim=c(0,1), add=T)
