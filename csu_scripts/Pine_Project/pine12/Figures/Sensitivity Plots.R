########################################
###	Stu Field ##########################
###	Department of Biology ##############
###	Colorado State University ##########
###	Fort Collins, CO 80523-1878 ########
###	sgf@colostate.edu ##################
########################################
### 2D Sensitivity/Elasticity barplots
### for pine12()
##################
rm(list=ls())
######################################################
dir <- "~/Dropbox/CSU/R-scripts/5NP R Code/pine12"   # <- change to your path here!
if (getwd() != dir) setwd(dir)
######################################################
y <- read.csv("Elasticities.csv")
#y <- rnorm(32)
time <- c("@ 100 yrs","@ 200 yrs","@ 1000 yrs")

par(mfrow=c(length(time),1))
for (i in 2:ncol(y)){
	barplot(y[,i], ylab= "Elasticity to parameter wrt QOI", xlab= "Parameter", 
	ylim=c(min(y[,i])*1.1, max(y[,i])*1.1), col= rainbow(length(y[,i])))

	title("Elasticity Plots for pine12", sub= time[i-1], cex.sub= 0.75); box()
	xaxis <- as.vector(barplot(y[,i], plot= FALSE))
	axis(1, at= xaxis, labels= 1:length(y[,i]), tick= TRUE, cex.axis= 0.75, col.ticks= 1,
	lwd.ticks= 0.75, las= 1, tcl= -0.5, padj= -1)
}


