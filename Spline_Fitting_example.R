###################################################
### Spline Plotting Examples
###################################################
x <- seq(0,10, by=0.1)
r <- 5
h <- 6
k <- 5
mix <- runif(101, 0.8, 1.2)
M   <- matrix(0, length(x), r)

for ( n in 1:r ) {
	M[,n] <- (1 - exp(-x * n)) * sample(mix, 101, replace=TRUE)
}

#####################################
# Other functions to use
#####################################
#y= 1-exp(-x*n)
#y= 1+exp(-x*n)
#y= (-(x-h)^2+(k))
#y= (-(x-h)^3+(k))

y <- M[,1]
par(mfrow=c(1, 2))
scatter.smooth(x, y)

#####################################
# Calculate the spline data
#####################################
spl.data <- smooth.spline(x, y, nknots=15)
plot(x, y, "b", col="navy")
points(spl.data, pch=19, bg="dark red", col="dark red")
legend("bottomright", legend=c("Data", "Predicted Spline"), lty=c(1,0), pch=c(1,1), col=c("blue3", "dark red"))

#####################################
# Extrapolate data from 0:5 using spline function
#####################################
#plot(predict(spl.data, 0:5), type='l')




##############
	# newer example
########################
plotLOESS <- function(x, y, add=FALSE, ...) {
	FUN <- if ( add ) points else plot 
	try(FUN(loess.smooth(x, y), type="l",...))
}

y <- sapply(c(5,10,15,12,3), function(x) rnorm(100, x, 4))
boxplot(y, col=8)
plotLOESS(rep(1:ncol(y),each=nrow(y)), y, add=TRUE, col=2, lty=2, lwd=2)






