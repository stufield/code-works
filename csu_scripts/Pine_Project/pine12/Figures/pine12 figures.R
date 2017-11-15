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
curve3d(pine12(Gen=10, Beta=x, LAIb=y)$FinalSum, from=c(0.01,1), to=c(0.2,5), zlab="Final Pop Total", ylab="LAI.b", xlab="Beta", col="darkred", sys3d="persp")

curve3d(pine12(Gen=x, Beta=y)$FinalSum, from=c(2,0), to=c(10,0.2), zlab="Final Pop Total", ylab="Beta", xlab="time", col="darkorchid4", sys3d="persp", theta=335)

####################
### Fig. 4 #########
####################
iipop <- c(0,1000,0,0,0,0,0,0,0,0,0,0)
Eqm <- pine12(Gen=800, Beta=0, x1=iipop)$Pop.Projection[,2:6]
matplot(Eqm, typ="n", 
	ylab="No. Individuals", 
	main="", 
	xlab="time", 
	ylim=c(0,max(Eqm)+100))
grid(NA,NULL, lty=1, lwd=1, col="gray95")
matplot(Eqm, typ="l", lwd=1.5, add=TRUE)
legend("topright", legend=c(expression(SD[1]),expression(SD[2]),"SA","YA","MA"), lty=1:5, col=1:5, cex=1.1, bg="gray95")


####################
### Beta vs time ###
####################
curve3d(pine12(Gen=y, Beta=x)$FinalSum, from=c(0,2), to=c(0.2,200), zlab="PopTotal", ylab="time", xlab="Beta", col="darkorchid4", sys3d="rgl")
rgl.snapshot(filename= "Beta-v-Time-Image.png", fmt= "png")

curve3d(pine12(Gen=y, Beta=x)$Adults, from=c(0,2), to=c(0.2,200), zlab="TotalAdults", ylab="time", xlab="Beta", col="darkorchid4", sys3d="rgl")
rgl.snapshot(filename= "Beta-v-Time-Image2.png", fmt= "png")

#par(mfrow=c(1,2))
curve3d(pine12(Gen=y, Beta=x)$FinalSum, from=c(0,2), to=c(0.2,200), zlab="PopTotal", ylab="time", xlab="beta", col="gray50", sys3d="persp", theta= 135, phi= 30, expand= 0.7, ltheta= 10, shade=0.4, box=TRUE, ticktype="detailed", nticks=5, cex.axis=0.5, zlim=c(0,800))

curve3d(pine12(Gen=y, Beta=x)$Adults, from=c(0,2), to=c(0.2,200), zlab="TotalAdults", ylab="time", xlab="beta", col="darkorchid4", sys3d="persp", theta= 135, phi= 30, expand= 0.7, ltheta= 10, shade=0.4, box=TRUE, ticktype="detailed", nticks=5, cex.axis=0.5)


#########################
### Beta vs LAIb @ 100 yrs
#########################
curve3d(pine12(Gen=100, Beta=x, LAIb=y)$FinalSum, from=c(0,0), to=c(0.2,3), zlab="PopTotal @ 100 yrs", ylab="LAIb", xlab="Beta", col="darkorchid4", sys3d="rgl")
rgl.snapshot(filename= "Beta-v-LAIb-Image.png", fmt= "png")

curve3d(pine12(Gen=100, Beta=x, LAIb=y)$FinalSum, from=c(0,0), to=c(0.2,3), zlab="PopTotal @ 100 yrs", ylab="LAIb", xlab="beta", col="darkorchid4", sys3d="persp", theta= 45, phi= 30, expand= 0.7, ltheta= 10, shade=0.4, box=TRUE, ticktype="detailed", nticks=5, cex.axis=0.5)

#####################
### delta vs time ###
#####################
curve3d(pine12(Gen=y, delta=x)$FinalSum, from=c(0,2), to=c(0.3,200), zlab="PopTotal", ylab="time", xlab="delta", col="darkorchid4", sys3d="rgl")
rgl.snapshot(filename= "Delta-v-Time-Image.png", fmt= "png")

curve3d(pine12(Gen=y, delta=x)$Adults, from=c(0,2), to=c(0.3,200), zlab="TotalAdults", ylab="time", xlab="delta", col="darkorchid4", sys3d="rgl")
rgl.snapshot(filename= "Delta-v-Time-Image2.png", fmt= "png")

#par(mfrow=c(1,2))
curve3d(pine12(Gen=x, delta=y)$FinalSum, from=c(2,0), to=c(200,0.3), zlab="TotalPop", xlab="time", ylab="delta", col="darkorchid4", sys3d="persp", theta= 45, phi= 30, expand= 0.7, ltheta= 10, shade=0.4, box=TRUE, ticktype="detailed", nticks=5, cex.axis=0.5)

curve3d(pine12(Gen=x, delta=y)$Adults, from=c(2,0), to=c(200,0.3), zlab="TotalAdults", xlab="time", ylab="delta", col="darkorchid4", sys3d="persp", theta= 45, phi= 30, expand= 0.7, ltheta= 10, shade=0.4, box=TRUE, ticktype="detailed", nticks=5, cex.axis=0.70)


####################
### beta vs delta ##
### @ 100 yrs ######
####################
curve3d(pine12(Gen=100, Beta=x, delta=y)$FinalSum, from=c(0,0), to=c(0.2,0.3), zlab="PopTotal @ 100 yrs", ylab="delta", xlab="beta", col="darkorchid4", sys3d="rgl")
rgl.snapshot(filename= "Delta-v-Beta-Image.png", fmt= "png")

curve3d(pine12(Gen=100, Beta=x, delta=y)$Adults, from=c(0,0), to=c(0.2,0.3), zlab="TotalAdults @ 100 yrs", ylab="delta", xlab="beta", col="darkorchid4", sys3d="rgl")
rgl.snapshot(filename= "Delta-v-Beta-Image2.png", fmt= "png")

#par(mfrow=c(1,2))
curve3d(pine12(Gen=100, Beta=x, delta=y)$FinalSum, from=c(0,0), to=c(0.2,0.3), zlab="PopTotal @ 100yrs", ylab="delta", xlab="beta", col="darkorchid4", sys3d="persp", theta= 45, phi= 30, expand= 0.7, ltheta= 10, shade=0.4, box=TRUE, ticktype="detailed", nticks=5, cex.axis=0.5)

curve3d(pine12(Gen=100, Beta=x, delta=y)$Adults, from=c(0,0), to=c(0.2,0.3), zlab="TotalAdults @ 100yrs", ylab="delta", xlab="beta", col="darkorchid4", sys3d="persp", theta= 45, phi= 30, expand= 0.7, ltheta= 10, shade=0.4, box=TRUE, ticktype="detailed", nticks=5, cex.axis=0.5)


######################################
### Effect of Beta on PopStructure ###
####### No Competition: LAIb = 0 #####
######################################
b = c(0, 0.016, 0.044, 0.2)
yaxis <- c(0,350)
#par(mfrow=c(4,1))
for (i in 1:4){
matplot(pine12(Gen=200, Beta=b[i], LAIb=0)$Pop.Proj.GroupClass[,-1], type="n", 
	sub= substitute(beta==x, list(x=b[i])), xlab="time", main="Population Structure", ylim=yaxis)
	grid(NA,NULL, lty=1, lwd=1, col="gray95")
	matplot(pine12(Gen=200, Beta=b[i], LAIb=0)$Pop.Proj.GroupClass[,-1], type="l", add=TRUE) 
	if (i==1) legend("right", legend=c("Sd1","Sd2","Sp","Ya","Ma"), lty=1:5, col=1:5, cex=0.5, bg="gray95")}


######################################
### Effect of Beta on PopStructure ###
####### Competition: LAIb = 2 ########
######################################
#par(mfrow=c(4,1))
for (i in 1:4){
matplot(pine12(Gen=200, x1=I.pop2, Beta=b[i], LAIb=2)$Pop.Proj.GroupClass[,-1], type="n", sub= substitute(beta==x, list(x=b[i])), xlab="time", main="Population Structure", ylim=yaxis)
grid(NA,NULL, lty=1, lwd=1, col="gray95")
matplot(pine12(Gen=200, x1=I.pop2, Beta=b[i], LAIb=2)$Pop.Proj.GroupClass[,-1], type="l", add=TRUE)
	if (i==1) legend("topright", legend=c("Sd1","Sd2","Sp","Ya","Ma"), lty=1:5, col=1:5, cex=0.5, bg="gray95")}


######################################
### Effect of Beta on Prevalence #####
####### No Competition: LAIb = 0 #####
####### Competition: LAIb = 2 ########
######################################
b = c(0.016, 0.044, 0.2)
#par(mfcol=c(3,2))

for (i in 1:3){
matplot(pine12(Gen=200, x1=I.pop, Beta=b[i])$Prevalence, type="n", ylim=c(0,1), sub= substitute(beta==x, list(x=b[i])), xlab="time", main="Rust Prevalence")
grid(NA, NULL, lty=1, lwd=1, col="gray95")
matplot(pine12(Gen=200, x1=I.pop, Beta=b[i])$Prevalence, type="l", add=TRUE)
if (i==1) legend("topleft", legend=c("Total","Sd1","Sd2","Sp","Ya","Ma"), lty=c(1:5), col=1:6, cex=0.5, bg="gray95")}

for (i in 1:3){
matplot(pine12(Gen=200, x1=I.pop2, Beta=b[i],LAIb=2)$Prevalence, type="n", ylim=c(0,1), sub= substitute(beta==x, list(x=b[i])), xlab="time", main="Rust Prevalence")
grid(NA, NULL, lty=1, lwd=1, col="gray95")
matplot(pine12(Gen=200, x1=I.pop2, Beta=b[i],LAIb=2)$Prevalence, type="l", add=TRUE)}



###########################
### 3D plots of Structure
### Without Competition
###########################
### Effect of Beta
##########################
xtime = 100; b = 0.2
Str <- pine12(Gen=xtime, Beta=b)$Pop.Projection[,c(-1,-7)]
colnames(Str)=c("SD1","SD2","SP","YA","MA","SD1i","SD2i","SPi","YAi","MAi")
z <- cbind(expand.grid(time=1:xtime, stage=colnames(Str)), x= as.vector(Str))
#z$stage <- ordered(z$stage, levels= rev(levels(z$stage))) ## re-order stages on y-axis
#z$time <- ordered(z$time, levels= rev(levels(z$time)))
cloud(x ~ time * stage, data=z, groups = stage, col.facet="darkslateblue", screen=list(z=-40, x=-55), xbase=1.2, ybase=0.8, scales=list(arrows=FALSE, distance=c(1.2,1.5,1.0), cex=0.55, col="darkgreen", font=2), par.settings= list(box.3d=list(col="gray75")), aspect= c(1.125, 0.6), panel.aspect=0.9, panel.3d.cloud= panel.3dbars, main="Beta on Population Structure", col="white",
 drape=FALSE, colorkey=FALSE, light.source=c(0,10,10), zlab=list(label="No.", font=4, col="darkred", cex=1.3), xlab=list(label="time", font=4, col="darkred", cex=1.3), ylab=list(label="stage", font=4, col="darkred", cex=1.3))
ltext(360, 100, substitute(beta==x, list(x=b)), cex=1.2, col="darkorchid", font=2)


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

###############################
### Effect of LAI on Eqm SR ###
###############################
#plot(pine12(Gen=250, Beta=0, LAIb=2)$LAI.y, pine12(Gen=250, Beta=0, LAIb=2)$Germination_rate, main="LAI vs. Germination rate")

#curve3d(tail(pine12(Gen=100, Beta=0, alpha2=x, alpha3=y, LAIb=0)$Germination_rate,1), from=c(0.001,0), to=c(0.1,3), zlab="SR @ 100yrs ", ylab="alpha3", xlab="alpha2", col="darkorchid4", sys3d="rgl")
#rgl.snapshot(filename= "LAI-v-SR-Image.png", fmt= "png")


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

################################
### Primary seedlings figure ###
################################
par(mar=c(5,4,4,4)+0.1) # default is mar=c(5,4,4,2)+0.1 (right margin)
t = 50; bx = c(0,0.016,0.044,0.2); fall = 50
### when fall = t; cumsum2() is just cumsum()
#par(mfrow=c(2,2))
for (i in 1:4){
v1 <- pine12(Gen=t, Beta=bx[i])$Pop_Projection[,2] #sd1
v2 <- pine12(Gen=t, Beta=bx[i])$Pop_Projection[,8] #sd1i
v3 <- cumsum2(group.class(pine12(Gen=t, Beta=bx[i])$Dead)[,2],fall) #cumsum sd1+sd1i
#v3 <- cumsum2(pine12(Gen=t, Beta=bx[i])$Dead[,2],fall) #cumsum sd1
v4 <- cumsum2(pine12(Gen=t, Beta=bx[i])$Dead[,8],fall) #cumsum sd1i
v5 <- pine12(Gen=t, Beta=bx[i])$Prevalence[,2]
Plot <- cbind(v1,v2,v3,v4)
matplot(Plot, type="n", main="SD1 Pop Str & Rust Prevalence", 	sub=substitute(beta==x, list(x=bx[i])), xlab="time", ylab="Primary seedlings/ha", ylim=c(0,325), lwd=1.5); grid(NA,NULL, lty=1, lwd=1, col="gray95")
matplot(Plot, type="l", lwd=1.5, add=TRUE); box()
par(new=T)
plot(1:t, v5, type="l", lty=5, col=5, ylab="", xlab="", axes=FALSE, ylim=c(0,1))
axis(4, ylim=c(0,1))
mtext("Rust Prevalence", side=4, line=2.5)
if (i==1) legend("topleft", legend=c("Sd1","Sd1i","CumSum Dead Sd1+Sd1i","CumSum Dead Sd1i","Proportion sd1 infected"), lty=1:5, col=1:5, cex=0.5, bg="gray95")
}

#####################
### Save 3D image ###
#####################
rgl.snapshot(filename= "rgl-Image.png", fmt= "png")
