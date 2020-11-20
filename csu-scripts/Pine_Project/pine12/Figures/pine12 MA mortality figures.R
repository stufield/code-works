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
source("~/Dropbox/CSU/R-scripts/5NP R code/pine12/pine12.R")
################################################################
###################### Simple 3D plot ##########################
################################################################


#######################################
### Effect of Adult mortality on SR ###
### LAIb=0; Beta=0 ####################
#######################################
curve3d(tail(pine12(Gen=x, Beta=0, m6=y)$Germination_rate,1), from=c(2,0.001), to=c(200,0.02), zlab="Germination Rate", ylab="Adult Mortality", xlab="time", col="darkorchid4", sys3d="rgl")
rgl.snapshot(filename= "m6-v-SR-Image.png", fmt= "png")

curve3d(pine12(Gen=x, Beta=0, m6=y)$FinalSum, from=c(2,0.001), to=c(200,0.02), zlab="Total Pop", ylab="Adult Mortality", xlab="time", col="darkorchid4", sys3d="rgl")
rgl.snapshot(filename= "m6-v-Total-Image.png", fmt= "png")

#par(mfrow=c(1,2))
curve3d(tail(pine12(Gen=x, Beta=0, m6=y)$Germination_rate,1), from=c(2,0.001), to=c(200,0.02), zlab="Germination Rate", ylab="Adult mortality", xlab="time", col="darkorchid4", sys3d="persp", theta= 45, phi= 30, expand= 0.7, ltheta= 10, shade=0.4, box=TRUE, ticktype="detailed", nticks=5, cex.axis=0.5)

curve3d(pine12(Gen=x, Beta=0, m6=y)$FinalSum, from=c(2,0.001), to=c(200,0.02), zlab="Total Pop", ylab="Adult mortality", xlab="time", col="darkorchid4", sys3d="persp", theta= 45, phi= 30, expand= 0.7, ltheta= 10, shade=0.4, box=TRUE, ticktype="detailed", nticks=5, cex.axis=0.5)


#################################################
### Effect of Adult mortality on PopStructure ###
### Beta = 0 ####################################
#################################################
xtime = 100; m = 0.02
Str <- pine12(Gen=xtime, Beta=0, m6=m)$Pop_Projection[,2:6]
colnames(Str)=c("SD1","SD2","SP","YA","MA")
z <- cbind(expand.grid(time=1:xtime, stage=colnames(Str)), x= as.vector(Str))
#z$stage <- ordered(z$stage, levels= rev(levels(z$stage))) ## re-order stages on yaxis
cloud(x ~ time * stage, data=z, screen=list(z=-40, x=-55), col.facet="darkslateblue", xbase=1.2, ybase=0.8, scales=list(arrows=FALSE, distance=c(1.2,1.5,1.0), cex=0.55, col="darkgreen", font=2), par.settings= list(box.3d=list(col="gray75")), aspect= c(1.125, 0.6), panel.aspect=0.9, panel.3d.cloud= panel.3dbars, main="Adult Mortality on Population Structure", col="white", drape=FALSE, colorkey=FALSE, light.source=c(0,10,10), zlab=list(label="No.", font=4, col="darkred", cex=1.3), xlab=list(label="time", font=4, col="darkred", cex=1.3), ylab=list(label="stage", font=4, col="darkred", cex=1.3), zlim=c(0,610))
ltext(360, 100, substitute(italic(m[6])==x, list(x=m)), cex=1.2, col="darkorchid", font=2)
#####################
### Save 3D image ###
#####################
rgl.snapshot(filename= "rgl-Image.png", fmt= "png")
