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
require(emdbook)
require(lattice)
library(latticeExtra)
require(scatterplot3d)
source("~/Dropbox/sgf_Rscripts/Pine Project/pine12/pine12.R")
################################################################
###################### Simple 3D plot ##########################
################################################################
#####################
### delta vs time ###
#####################
#curve3d(pine12(Gen=y, delta=x)$FinalSum, from=c(0,2), to=c(0.3,200), zlab="PopTotal", ylab="time", xlab="delta", col="darkorchid4", sys3d="rgl")
#rgl.snapshot(filename= "Delta-v-Time-Image.png", fmt= "png")

#curve3d(pine12(Gen=y, delta=x)$Adults, from=c(0,2), to=c(0.3,200), zlab="TotalAdults", ylab="time", xlab="delta", col="darkorchid4", sys3d="rgl")
#rgl.snapshot(filename= "Delta-v-Time-Image2.png", fmt= "png")




#par(mfrow=c(1,2))
curve3d(pine12(Gen=x, delta=y)$FinalSum, from=c(2,0), to=c(200,0.3), zlab="Total Trees", xlab="time", ylab="delta", col="gray99", sys3d="persp", theta= 45, phi= 30, expand= 0.7, ltheta= 10, shade=0.4, box=TRUE, ticktype="detailed", nticks=5, cex.axis=0.5)

curve3d(pine12(Gen=x, delta=y)$Adults, from=c(2,0), to=c(200,0.3), zlab="Mature Adults", xlab="time", ylab="delta", col="gray99", sys3d="persp", theta= 45, phi= 30, expand= 0.7, ltheta= 10, shade=0.4, box=TRUE, ticktype="detailed", nticks=5, cex.axis=0.5)





#####################
### Save 3D image ###
#####################
rgl.snapshot(filename= "rgl-Image.png", fmt= "png")
