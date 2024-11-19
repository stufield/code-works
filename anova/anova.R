########################
### One-Way ANOVA ######
########################
### Example out of The R Book
Sand <- c(6,10,8,6,14,17,9,11,7,11)
Clay <- c(17,15,3,11,14,12,12,8,10,13)
Loam <- c(13,16,9,12,15,16,17,13,18,14)

# Setup dependent variable
y <- c(Sand, Clay, Loam)

# Make sure it is a factor
soil <- as.factor((c(rep("Sand", 10), rep("Clay", 10), rep("Loam", 10))))

# Take a look at variances
sapply(list(Sand, Clay, Loam), var)

# Test for homogeniety of variances
# Fligner-Killeen test; make sure soil = 'factor'
fligner.test(y ~ soil)
# Bartlett test
bartlett.test(y ~ soil)

boxplot(y ~ soil, ylab="Yield", notch=FALSE, col="darkslateblue")

aov.model <- aov(y ~ soil)
summary(aov.model)


#########################
### Plotting error bars
#########################
#################################
## create function for error.bars
###################################
error.bars <- function(yv, z, nn) {
	xv <- barplot(yv, ylim=c(0, (max(yv) + max(z))), names=nn, ylab=deparse(substitute(yv)))
	g  <- (max(xv) - min(xv)) / 50
	for (i in 1:length(xv)) {
		lines(c(xv[i], xv[i]), c(yv[i]+z[i], yv[i]-z[i]))
		lines(c(xv[i]-g, xv[i]+g), c(yv[i]+z[i], yv[i]+z[i]))
		lines(c(xv[i]-g, xv[i]+g), c(yv[i]-z[i], yv[i]-z[i]))
	}
}

### Get means by treatment
Crop.Yield <- as.vector(tapply(y, soil, mean))
### Make barplot
barplot(Crop.Yield, names=levels(soil))
### Get standard error of mean from anova table
SEM <- rep(sqrt(summary(aov.model)[[1]][2,3] / as.numeric(table(soil)[1])), length(levels(soil)))
### This calculation of sem assumes equal sample sizes and thus pooled s^2!!!!!!!!!!!
labels <- as.character(levels(soil))

### Plot the Barplot
error.bars(bar.y, SEM, labels)

##################################
### Apparently someone invented
### a function to do error.bars
#################################
?barplot2 #### requires gplots (which requires gdata & gtools)

barplot2(y ~ soil)


