###################################
### William Black IV! #############
### Solving indefinite integrals ##
### with Monte-Carlo simulation ###
### modified by Stu Field #########
### 22-OCT-2010 ###################
###################################
# Define function of interest
# Normal PDF #
normal <- function(x) {
   (1/sqrt(2*pi) * exp((-x^2)/2))
}

# Parabola #
parabola <- function(x, a=1, b=2) {
   a * x^2 + b
}

# sin function #
sin <- function(x, a=1/3, b=3) {
   a * sin(x) + b
}

##########################
# mystery fun
###########################
mystery.fun <- function (x) {
  20*dnorm(x, mean=-1, sd=5) +
     ifelse(x > -1.1, 
            6*dgamma(x=x+1,shape=2,scale=0.5), 
            1.5*dgamma(x=-x,shape=5,scale=0.2)) + 
            2*dgamma(x=2.75-x, shape=3, scale=0.25)
}



##############################################
### MC.integral() call #######################
##############################################
MC.integral(n=10000, interval=c(-2.98, 2.98), FUN=mystery.fun)


##########################################################################
### MC.integral.fast() is much much FASTER!!!!!!!!!! #####################
##########################################################################
# A faster version of MC.integral() used vectorized random No. generation
# >100x faster than above when plotting turned off
##########################################################################
MC.integral.fast(n=100000, interval=c(-2.98, 2.98), FUN=mystery.fun, plot=FALSE)





####################################
####################################
####################################
### Now Bootstrap 
### The estimates to get
### a better estimate of area
####################################
####################################
####################################
nboot <- 1000
mc    <- 100000
MCrep.v <- rep(NA, nboot)
for ( j in 1:nboot ) {
  MCrep.v[j] <- MC.integral.fast(mc, interval= c(-2.98, 2.98), FUN= mystery.fun, plot=FALSE)
}


#data(MonteCarloIntegral)   # if previously run
hist(MCrep.v, col="gray75", prob=TRUE, xlab="Area", main="", breaks=15); box()
lines(density(MCrep.v))
lines(density(MCrep.v, adjust=1.75), lty="dotted", col=2)
Est <- density(MCrep.v)$x[which.max(density(MCrep.v)$y)]
abline(v=Est, col=4, lty="dotted")
legend("topleft", legend=format(paste("Area Est ~",round(Est,6))), bg="gray75", cex= 0.75)

# check that histogram sums to 1 # 
H <- hist(MCrep.v, plot=FALSE)
print(sum((H$breaks[2]-H$breaks[1])*H$density), digits=10)

