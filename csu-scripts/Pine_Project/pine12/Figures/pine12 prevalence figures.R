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
######################################
### Effect of Beta on Prevalence #####
####### No Competition: LAIb = 0 #####
####### Competition: LAIb = 2 ########
######################################
b = c(0.016, 0.044, 0.2)
l.thick <- c(3,rep(1.5,5))
for (i in 1:3) {
	plot <- pine12(Gen=200, Beta=b[i])$Prevalence
	matplot(plot, type="n", 
			  ylim=c(0,1), xlab="time", ylab="Rust Prevalence")
	text(180, 0.95, substitute(beta==x, list(x=b[i])), cex=0.9)
	grid(NA, NULL, lty=1, lwd=1, col="gray95")
	matplot(plot, type="l", add=TRUE, col=1, lty=1:6, lwd=l.thick)
	#lines(1:200, plot[,1], type="l", lty=6, pch=19, lwd=3)
	if (i==1) legend("topleft", 
	legend=c("Total",expression(SD[1]),expression(SD[2]),"SA","YA","MA"), 
	col=1, lty=1:6, cex=0.9, bg="gray80", lwd=l.thick)
}




################
#   Not Used   #
################
for (i in 1:3){
	plot2 <- pine12(Gen=200, x1=I.pop2, Beta=b[i],LAIb=2)$Prevalence
	matplot(plot2, type="n", ylim=c(0,1), xlab="time", ylab="Rust Prevalence")
	text(180, 0.85, substitute(beta==x, list(x=b[i])), cex=0.9)
	grid(NA, NULL, lty=1, lwd=1, col="gray95")
	matplot(plot2, type="l", col=1, lty=1:6, lwd=l.thick, add=TRUE)
}

