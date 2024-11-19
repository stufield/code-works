########################
### Simple 2-way ANOVA
### Checking for interactions
### of 2 factors with multiple levels
########################
##############################
###   Stu Field
###	  Department of Biology
###	  Colorado State University
###	  Fort Collins, CO 80523-1878
###	  sgf@colostate.edu
##################################
ex1 <- function() {
	mydata <- data.frame(Deterg = factor(c(rep("Super",12),rep("Best",12))),
								Temp = factor(rep(gl(3,4,labels=c("Cold","Warm","Hot")),2)),
								Wash = c(4,5,6,5,7,9,8,12,10,12,11,9,6,6,4,4,13,15,12,12,12,13,10,13))

	### Explore 'data'
	mydata
	summary(mydata)
	tapply(mydata$Wash, mydata$Temp, mean)
	tapply(mydata$Wash, mydata$Deterg, mean)
	tapply(mydata$Wash, mydata$Deterg:mydata$Temp, mean)

	### Create Linear Model
	model <- aov(Wash ~ Deterg*Temp, data= mydata)
	summary(model)

	### Optional output
	anova(model)
	residuals(model)
	residuals(model)^2
	fitted(model)
	coefficients(model)

	### Plot Interaction
	interaction.plot(mydata$Temp, mydata$Deterg, mydata$Wash)
	interaction.plot(mydata$Deterg, mydata$Temp, mydata$Wash)
}



###########
# Ex 2
###########
ex2 <- function() {
	mydata <- data.frame(quality = factor(rep(c("Fresh","Rancid"),6)),
								sex = factor(gl(2,6), labels=c("male","female")),
								food = c(709,592,679,538,699,476,657,508,594,505,677,539))

	summary(mydata)
	tapply(mydata$food, mydata$sex, mean)
	tapply(mydata$food, mydata$quality, mean)
	tapply(mydata$food, mydata$quality:mydata$sex, mean)

	### Create Linear Model
	model <- aov(food ~ quality*sex, data= mydata)
	summary(model)

	### Optional output
	anova(model)
	residuals(model)
	residuals(model)^2
	fitted(model)
	coefficients(model)

	### Plot Interaction
	interaction.plot(mydata$sex, mydata$quality, mydata$food)
	interaction.plot(mydata$quality, mydata$sex, mydata$food)
}

###########
# Ex. 3
###########
ex3 <- function() {
	data(ToothGrowth)
	attach(ToothGrowth)
	#detach(ToothGrowth)
	head(ToothGrowth)
	anova(aov(len~factor(dose)*supp, data=ToothGrowth))
	interaction.plot(factor(dose),supp,len)
}
