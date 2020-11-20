########################################
###	Stu Field ##########################
###	Department of Biology ##############
###	Colorado State University ##########
###	Fort Collins, CO 80523-1878 ########
###	sgf@colostate.edu ##################
########################################
##################
### 3D plot using 
### pine12()
### ?curve3d
### ?persp
##################
rm(list=ls())
##################
require(pine)
require(emdbook)
require(lattice)
library(latticeExtra)
require(scatterplot3d)
################################
#        Base Model Eqm Fig    #
################################
iipop <- c(0,1000,0,0,0,0,0,0,0,0,0,0)
iipop2 <- c(62580,38,79,65,91,353,0,0,0,0,0,0)
Eqm <- pine12(Gen=800, Beta=0, x1=iipop)$Pop.Projection[,2:6]
Eqm <- pine12(Gen=800, x1=iipop2)$Pop.Projection[,8:12]
lines <- 1
colors <- 1:5
##########################
par(bg="thistle4")
matplot(Eqm, typ="n", 
	ylab="No. Individuals", 
	main="", 
	xlab="time", 
	ylim=c(0,max(Eqm)+100))
grid(NA,NULL, lty=1, lwd=1, col="gray95")
matplot(Eqm, typ="l", lwd=1.5, add=TRUE, col=colors, lty=lines)
legend("topright", legend=c(expression(SD[1]),expression(SD[2]),"SA","YA","MA"), lty=lines, col=colors, cex=1.1, lwd=1.5, bg="gray90")
title("Equilibrium Structure")
