###########################
#     Multiplot Title     #
###########################
z1 <- rexp(100)
z2 <- rnorm(100)
z3 <- rpois(100,lambda=2)
z4 <- rbinom(100,prob=0.4, size=100)
###################################
par(mfrow=c(2,2), oma= c(0, 0, 3, 0), bg= "mediumslateblue")   # set margins
###############################################
#     oma goes c(bottom, left, top, right)
###############################################
curve(dexp, from=0, to=5, main="rexp")   # plot1
hist(z2, main="rnorm")     # plot2
hist(z3, main="rpois")    # plot3
hist(z4, main="rbinom")     # plot4
mtext("Densities", outer= TRUE, cex= 1.5)
#?mtext
