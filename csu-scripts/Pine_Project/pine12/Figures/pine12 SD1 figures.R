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
################################################################
###################### Simple 3D plot ##########################
################################################################
################################
#   Primary seedlings figure   #
################################
par(mar=c(5,4,4,4)+0.1) # default is mar=c(5,4,4,2)+0.1 (right margin)
t = 50; bx = c(0,0.016,0.044,0.2); fall = 50
### when fall = t; cumsum2() is just cumsum()

#par(mfrow=c(2,2))
for (i in 1:4) {
v1 <- pine12(Gen=t, Beta=bx[i])$Pop.Projection[,2] #sd1
v2 <- pine12(Gen=t, Beta=bx[i])$Pop.Projection[,8] #sd1i
v3 <- cumsum2(group.class(pine12(Gen=t, Beta=bx[i])$Dead)[,2],fall) #cumsum sd1+sd1i
#v3 <- cumsum2(pine12(Gen=t, Beta=bx[i])$Dead[,2],fall) #cumsum sd1
v4 <- cumsum2(pine12(Gen=t, Beta=bx[i])$Dead[,8],fall) #cumsum sd1i
v5 <- pine12(Gen=t, Beta=bx[i])$Prevalence[,2]
Plot <- cbind(v1,v2,v3,v4)

################
#   Plotting   #
################
matplot(Plot, type="n", main="", xlab="time", 
	    ylab="", ylim=c(0,325))
grid(NA,NULL, lty=1, lwd=1, col="gray95")
text(43, 313, substitute(beta==x, list(x=bx[i])))
matplot(Plot, type="l", lwd=1.5, lty=2:5, col=1, add=TRUE); box()
par(new=T)
plot(1:t, v5, type="l", lwd=2, lty=1, col=1, ylab="", xlab="", axes=FALSE, ylim=c(0,1))
axis(4, ylim=c(0,1))
if(i<4) title(ylab=expression("Primary seedlings "*(SD[1]) ))
if(i>1) mtext(expression("Rust Prevalence "*(Phi[2])), side=4, line=2.5)
if (i==1) {
	text(40,0.55,label=expression(paste("dead ",italic(x[2])+italic(x[8]))), cex=0.8)
	text(40,0.15,label=expression(italic(x[2])), cex=0.8)
	text(47,0.04,label=expression(Phi[2]), cex=0.8)
	legend("topleft", 
	    legend=c(expression(italic(x)[2]), expression(italic(x)[8]), 
		expression(paste("cumulative dead ",italic(x)[2] + italic(x)[8])),
		expression(paste("cumulative dead ", italic(x)[8])), 
		expression(Phi[2])),
		lty=c(2:5,1), col=1, cex=0.8, bg="gray80")
	}
if (i==4){
	text(37,0.7,label=expression(paste("dead ", italic(x[2])+italic(x[8]))), cex=0.8)
	text(40,0.45,label=expression(paste("dead ", italic(x[8]))), cex=0.8)
	text(46,0.24,label=expression(Phi[2]), cex=0.8)
	text(50,0.035,label=expression(italic(x[8])), cex=0.8)
	text(40,0.09,label=expression(italic(x[2])), cex=0.8)
	}
}









#####################
### Save 3D image ###
#####################
rgl.snapshot(filename= "rgl-Image.png", fmt= "png")
