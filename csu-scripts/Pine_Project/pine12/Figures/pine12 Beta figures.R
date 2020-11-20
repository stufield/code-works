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
require(latticeExtra)
require(scatterplot3d)
################################################################
###################### Simple 3D plot ##########################
################################################################
#curve3d(pine12(Gen=10, Beta=x, LAIb=y)$FinalSum, from=c(0.01,1), to=c(0.2,5), zlab="Final Pop Total", ylab="LAI.b", xlab="Beta", col="darkred", sys3d="persp")

#curve3d(pine12(Gen=x, Beta=y)$FinalSum, from=c(2,0), to=c(10,0.2), zlab="Final Pop Total", ylab="Beta", xlab="time", col="darkorchid4", sys3d="persp", theta=335)


####################
### Beta vs time ###
####################
#curve3d(pine12(Gen=y, Beta=x)$FinalSum, from=c(0,2), to=c(0.2,200), zlab="PopTotal", ylab="time", xlab="Beta", col="darkorchid4", sys3d="rgl")
#rgl.snapshot(filename= "Beta-v-Time-Image.png", fmt= "png")

#curve3d(pine12(Gen=y, Beta=x)$Adults, from=c(0,2), to=c(0.2,200), zlab="TotalAdults", ylab="time", xlab="Beta", col="darkorchid4", sys3d="rgl")
#rgl.snapshot(filename= "Beta-v-Time-Image2.png", fmt= "png")



#par(mfrow=c(1,2))
curve3d(pine12(Gen=y, Beta=x)$FinalSum, from=c(0,2), to=c(0.2,200), zlab="Total Trees", ylab="time", xlab="Beta", col="gray99", sys3d="persp", theta= 135, phi= 30, expand= 0.7, ltheta= 10, shade=0.4, box=TRUE, ticktype="detailed", nticks=5, cex.axis=0.5, zlim=c(0,800))

curve3d(pine12(Gen=y, Beta=x)$Adults, from=c(0,2), to=c(0.2,200), zlab="Mature Adults", ylab="time", xlab="Beta", col="gray99", sys3d="persp", theta= 135, phi= 30, expand= 0.7, ltheta= 10, shade=0.4, box=TRUE, ticktype="detailed", nticks=5, cex.axis=0.5)






#########################
### Beta vs LAIb @ 100 yrs
#########################
#curve3d(pine12(Gen=100, Beta=x, LAIb=y)$FinalSum, from=c(0,0), to=c(0.2,3), zlab="PopTotal @ 100 yrs", ylab="LAIb", xlab="Beta", col="darkorchid4", sys3d="rgl")
#rgl.snapshot(filename= "Beta-v-LAIb-Image.png", fmt= "png")
#
#curve3d(pine12(Gen=100, Beta=x, LAIb=y)$FinalSum, from=c(0,0), to=c(0.2,3), zlab="PopTotal @ 100 yrs", ylab="LAIb", xlab="beta", col="darkorchid4", sys3d="persp", theta= 45, phi= 30, expand= 0.7, ltheta= 10, shade=0.4, box=TRUE, ticktype="detailed", nticks=5, cex.axis=0.5)








######################################
### Effect of Beta on PopStructure ###
####### No Competition: LAIb = 0 #####
######################################
par(bg="thistle4")
b = c(0, 0.016, 0.044, 0.2)
yaxis <- c(0,350)
lines <- 1:5
colors <- 1
###################
for (i in 1:4) {
matplot(pine12(Gen=200, Beta=b[i], LAIb=0)$Pop.Proj.GroupClass[,-1], type="n",
	xlab="time", main="", ylim=yaxis, ylab="No. Individuals")
	text(175, 315, substitute(beta==x, list(x=b[i])), cex=0.9)
	grid(NA,NULL, lty=1, lwd=1, col="gray95")
matplot(pine12(Gen=200, Beta=b[i], LAIb=0)$Pop.Proj.GroupClass[,-1], type="l", 
    add=TRUE, col=colors, lty=lines, lwd=1.5)
	if (i==1) legend("right", legend=c(expression(SD[1]),expression(SD[2]),"SA","YA","MA"), lty=lines, col=colors, lwd=1.5, cex=0.9, bg="gray80")}


######################################
### Effect of Beta on PopStructure ###
####### Competition: LAIb = 2 ########
######################################
#par(mfrow=c(4,1))
par(bg="thistle4")
b = c(0, 0.016, 0.044, 0.2)
yaxis <- c(0,350)
lines <- 1:5
colors <- 1
I.pop2 <- pine12(Gen=2000, Beta=0, LAIb=2)$FinalPopVec   # disease-free for LAIb=2
#####################
for (i in 1:4) {
matplot(pine12(Gen=200, x1=I.pop2, Beta=b[i], LAIb=2)$Pop.Proj.GroupClass[,-1],
	type="n", xlab="time", main="", ylim=yaxis, ylab="No. Individuals")
	text(175, 275, substitute(beta==x, list(x=b[i])), cex=0.9)
	grid(NA,NULL, lty=1, lwd=1, col="gray95")
matplot(pine12(Gen=200, x1=I.pop2, Beta=b[i], LAIb=2)$Pop.Proj.GroupClass[,-1],
	type="l", add=TRUE, col=colors, lty=lines, lwd=1.5)
	if (i==1) legend("topleft", legend=c(expression(SD[1]),expression(SD[2]),"SA","YA","MA"), lty=lines, col=colors, lwd=1.5, cex=0.9, bg="gray80")}








####################
### beta vs delta ##
### @ XXX yrs ######
####################
#curve3d(pine12(Gen=100, Beta=x, delta=y)$FinalSum, from=c(0,0), to=c(0.2,0.3), zlab="Total Trees @ 100 yrs", ylab="delta", xlab="Beta", col="darkorchid4", sys3d="rgl")
#rgl.snapshot(filename= "Delta-v-Beta-Image.png", fmt= "png")

#curve3d(pine12(Gen=100, Beta=x, delta=y)$Adults, from=c(0,0), to=c(0.2,0.3), zlab="Mature Adults @ 100 yrs", ylab="delta", xlab="Beta", col="darkorchid4", sys3d="rgl")
#rgl.snapshot(filename= "Delta-v-Beta-Image2.png", fmt= "png")


XXX = 100
#par(mfrow=c(1,2))
curve3d(pine12(Gen=XXX, Beta=x, delta=y)$FinalSum, from=c(0,0), to=c(0.2,0.3), zlab="Total Trees - 100 yrs", ylab="delta", xlab="Beta", col="gray99", sys3d="persp", theta= 45, phi= 30, expand= 0.7, ltheta= 10, shade=0.4, box=TRUE, ticktype="detailed", nticks=5, cex.axis=0.5)

curve3d(pine12(Gen=XXX, Beta=x, delta=y)$Adults, from=c(0,0), to=c(0.2,0.3), zlab="Mature Adults - 100 yrs", ylab="delta", xlab="Beta", col="gray99", sys3d="persp", theta= 45, phi= 30, expand= 0.7, ltheta= 10, shade=0.4, box=TRUE, ticktype="detailed", nticks=5, cex.axis=0.5)








###########################
### 3D plots of Structure
### Without Competition
##########################
### Effect of Beta #######
##########################
#xtime = 100; b = 0.2
#Str <- pine12(Gen=xtime, Beta=b)$Pop.Projection[,c(-1,-7)]
#colnames(Str)=c("SD1","SD2","SP","YA","MA","SD1i","SD2i","SPi","YAi","MAi")
#z <- cbind(expand.grid(time=1:xtime, stage=colnames(Str)), x= as.vector(Str))
##z$stage <- ordered(z$stage, levels= rev(levels(z$stage))) ## re-order stages on y-axis
##z$time <- ordered(z$time, levels= rev(levels(z$time)))
#cloud(x ~ time * stage, data=z, groups = stage, col.facet="darkslateblue", screen=list(z=-40, x=-55), xbase=1.2, ybase=0.8, scales=list(arrows=FALSE, distance=c(1.2,1.5,1.0), cex=0.55, col="darkgreen", font=2), par.settings= list(box.3d=list(col="gray75")), aspect= c(1.125, 0.6), panel.aspect=0.9, panel.3d.cloud= panel.3dbars, main="Beta on Population Structure", col="white",
# drape=FALSE, colorkey=FALSE, light.source=c(0,10,10), zlab=list(label="No.", font=4, col="darkred", cex=1.3), xlab=list(label="time", font=4, col="darkred", cex=1.3), ylab=list(label="stage", font=4, col="darkred", cex=1.3))
#ltext(360, 100, substitute(beta==x, list(x=b)), cex=1.2, col="darkorchid", font=2)

#####################
### Save 3D image ###
#####################
rgl.snapshot(filename= "rgl-Image.png", fmt= "png")
