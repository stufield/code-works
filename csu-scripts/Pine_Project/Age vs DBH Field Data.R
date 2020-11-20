#########################################
### simple linear regression ############
### 5 needle pine Age vs dbh ############
#########################################
rm(list=ls())
######################################
### Import data into R and call it
######################################
pine.data <- read.csv("Age vs DBH.csv")
pine.data

##############################
### PIAR = Bristlecone pine
### PIFL = Limber pine
######################################
### Get a rough plot of them
######################################
plot(pine.data$age, pine.data$PIFL_dbh)
points(pine.data$age, pine.data$PIAR_dbh)
curve((mean(c(20.19726,12.67591)))*log(x) + mean(c(-69.6496,-40.27601)), from = 0, to= 425, add=TRUE, col=2)

##################################################
### Do linear regression analysis for each species
##################################################
fitPIFL <- lm(PIFL_dbh ~ age, data= pine.data)
summary(fitPIFL)     ### Summarized the model you just created; gives intercept, R-squared value, and P-value
#anova.lm(fitPIFL)   ### This gives you the anova table of the values you just calculated

fitPIAR <- lm(pine.data$PIAR_dbh ~ pine.data$age, data=pine.data)
summary(fitPIAR)
#anova.lm(fitPIAR)

par(mfrow=c(1,2))
plot(pine.data$PIFL_dbh ~ pine.data$age, data= pine.data); abline(fitPIFL, lwd=1.5)
plot(pine.data$PIAR_dbh ~ pine.data$age, data= pine.data); abline(fitPIAR, lwd=1.5)
#### NOT TOO GOOD A FIT - TRANSFORM

############################################################
### Do log-linear regression analysis for each species
### y = m * log(x) + b; take log-x-axis
############################################################
fitPIFL2 <- lm(PIFL_dbh ~ log(age), data= pine.data)
fitPIAR2 <- lm(PIAR_dbh ~ log(age), data= pine.data)
summary(fitPIFL2); summary(fitPIAR2)

PIFLm = coef(fitPIFL2)[2] # slope of fit2 PIFL
PIFLb = coef(fitPIFL2)[1] # y-int of fit2 PIFL
PIARm = coef(fitPIAR2)[2] # slope of fit2 PIAR
PIARb = coef(fitPIAR2)[1] # y-int of fit2 PIAR
PIFL.Res = residuals(fitPIFL2)
PIAR.Res = residuals(fitPIAR2)
PIFLm; PIFLb
PIARm; PIARb

###################
### Plots
###################
par(mfrow=c(1,2))
#####################
### Plot Data & Fits
#####################
plot(pine.data$age[1:300], pine.data$PIFL_dbh[1:300], cex=0.75, xlab="Age", ylab="dbh PIFL")
#scatter.smooth(pine.data$age, pine.data$PIFL.dbh, col=1)
curve((PIFLm*log(x) + PIFLb), from = 0, to= 375, lty=4, add=T, col=2, lwd= 2)

# Therefore PIFL dbh = 20.19726 * log(age) - 69.6496

plot(pine.data$age, pine.data$PIAR_dbh, cex=0.75, xlab="Age", ylab="dbh PIAR")
#scatter.smooth(pine.data$age, pine.data$PIFL.dbh, col=1)
curve((PIARm*log(x) + PIARb), from = 0, to= 421, lty=4, add=T, col=2, lwd= 2)

# Therefore PIAR dbh = 12.67591 * log(age) - 40.27601

#####################
### Plot residuals
#####################
plot(pine.data$age[1:300], PIFL.Res, cex=0.75, xlab="log(Age)", ylab="Residuals"); abline(h=0)
plot(pine.data$age, PIAR.Res, cex=0.75, xlab="log(Age)", ylab="Residuals"); abline(h=0)


########################################
### Log-linear transformation for 
### linear regression example
#########################################
x = c(0.1,0.15,0.22,0.34,0.5,0.75,1.1,1.7,2.6,3.8,5.8,8.9)
y = c(-2.5,-2.2,-1.5,-1,-0.33,0,0.45,1.02,1.53,1.99,2.45,3.07)

mydata <- data.frame(x,y)
plot(mydata)

fit <- lm(y ~ log(x))
summary(fit)
Res <- summary(fit)$residuals
m <- coef(fit)[2] # slope
b <- coef(fit)[1] # y-int
curve((m*log(x) + b), from = 0, to= 9, add=TRUE, col=2)

### y = 1.24257 * log(x) + 0.34288


########################################
### Non-linear regression fit for 
### linear regression example
#########################################
# can't seem to get good starting values because overparameterized
nonlfit <- nls(y ~ 1-(exp(a-x)), start= list(a=1), data=mydata)
summary(nonlfit)

a <- coef(nonlfit)
plot(y~x, pch=19)
curve(1 - (m*exp(b-x)), from=0, to= 9, lty= 4, col= 2, lwd= 2, add=TRUE)
curve(1 - (exp(a-x)), from=0, to= 9, lty= 4, col= 2, lwd= 2, add=TRUE)

