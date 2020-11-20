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
########################
### Effect of alpha_2 ##
########################
xtime = 100; alpha2 = 0.001
Str <- pine12(Gen=xtime, alpha2=alpha2)$Pop_Projection[,c(-1,-7)]
colnames(Str)=c("SD1","SD2","SP","YA","MA","SD1i","SD2i","SPi","YAi","MAi")
z <- cbind(expand.grid(time=1:xtime, stage=colnames(Str)), x= as.vector(Str))
#z$stage <- ordered(z$stage, levels= rev(levels(z$stage))) ## re-order stages on yaxis
#?cloud
cloud(x ~ time * stage, data=z, screen=list(z=-40, x=-55), col.facet="darkslateblue", xbase=1.2, ybase=0.8, scales=list(arrows=FALSE, distance=c(1.2,1.5,1.0), cex=0.55, col="darkgreen", font=2), par.settings= list(box.3d=list(col="gray75")), aspect= c(1.125, 0.6), panel.aspect=0.9, panel.3d.cloud= panel.3dbars, main="Alpha on Population Structure", col="white", drape=FALSE, colorkey=FALSE, light.source=c(0,10,10), zlab=list(label="No.", font=4, col="darkred", cex=1.3), xlab=list(label="time", font=4, col="darkred", cex=1.3), ylab=list(label="stage", font=4, col="darkred", cex=1.3))
ltext(360, 100, substitute(alpha[2]==x, list(x=alpha2)), cex=1.2, col="darkorchid", font=2)

########################
### Effect of alpha_3 ##
########################
xtime = 100; alpha3 = 0
Str <- pine12(Gen=xtime, alpha3=alpha3)$Pop_Projection[,c(-1,-7)]
colnames(Str)=c("SD1","SD2","SP","YA","MA","SD1i","SD2i","SPi","YAi","MAi")
z <- cbind(expand.grid(time=1:xtime, stage=colnames(Str)), x= as.vector(Str))
#z$stage <- ordered(z$stage, levels= rev(levels(z$stage))) ## re-order stages on yaxis
cloud(x ~ time * stage, data=z, screen=list(z=-40, x=-55), col.facet="darkslateblue", xbase=1.2, ybase=0.8, scales=list(arrows=FALSE, distance=c(1.2,1.5,1.0), cex=0.55, col="darkgreen", font=2), par.settings= list(box.3d=list(col="gray75")), aspect= c(1.125, 0.6), panel.aspect=0.9, panel.3d.cloud= panel.3dbars, main="Alpha on Population Structure", col="white", drape=FALSE, colorkey=FALSE, light.source=c(0,10,10), zlab=list(label="No.", font=4, col="darkred", cex=1.3), xlab=list(label="time", font=4, col="darkred", cex=1.3), ylab=list(label="stage", font=4, col="darkred", cex=1.3))
ltext(360, 100, substitute(alpha[3]==x, list(x=alpha3)), cex=1.2, col="darkorchid", font=2)

#####################
### Save 3D image ###
#####################
rgl.snapshot(filename= "rgl-Image.png", fmt= "png")
