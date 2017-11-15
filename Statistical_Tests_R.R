##################################
##################################
######## Basic Statistical #######
########## Tests in R###### ######
########## By Stu Field ##########
########## April 22, 2009 ########
##################################
### if you attached your data using attach(mydata)
### you do need to refer to	objects from the data.frame
### using the mydata$variableX notation, 
### you may simply use the variable name as it appears
####
###########
########################
### Basic Statistics ###
########################
###########
mydata = read.csv('Tree data.csv', sep=',', header=T) 
summary(mydata)

plot(mydata$dbh, mydata$Heartwood, pch= 19, col='black', main='Heartwood Area vs. DBH', ylab='Heartwood area (cm^2)', xlab='Diameter @ breast height')

#######################
### The Linear Model
#######################
fit <- lm(mydata$Heartwood ~ mydata$dbh, data = mydata)
summary(fit)
abline(fit, col='red', lty = 4, lwd = 2)
legend('topleft', legend=c('lm(fit)'), col= 'red', lty= 4, bg= 'gray95')
attributes(fit)
coef(fit)
resids = fit$residuals

#####################
# Correlation tests
#####################
cor.test(~ Heartwood + dbh, data = mydata)
cor.test(~ Heartwood + dbh, data = mydata, method = 'spearman') # Non-parametric alternative

########################
# Parametric comparison
# of 2 treatment means
# t-test on SapDepth
########################

##############################
### First we must check ######
### that variances are equal #
### Levene's test ############
##############################
var.test(SapDepth ~ Temp, data = mydata)
### In this case the answer is YES!

##############################
### Next we must check #######
### that fits Normal dist ####
### Kolmogorov-Smirnov test ##
##############################
ks.test(mydata$SapDepth, pnorm); hist(mydata$SapDepth)
### In this case the answer is NO! They come from different

norm <- function(n=1000){
set.seed(666)
nD = rnorm(n, 400, 25)
ks.test(nD, pnorm); hist(nD, col='gray88', main='', xlab=''); par(new=T)
plot(min(nD):max(nD), dnorm(min(nD):max(nD), mean(nD), sd(nD)), 'l', col='navy', lwd= 2, axes= F, xlab='', ylab='', bty= 'n')}

################################
### Aside: Do two samples come
### from the SAME distribution?
################################
normal = rnorm(50)
binomal = rbinom(50,100,0.33)
hist(normal); hist(binomal)
ks.test(normal, binomal)
### NOPE! ###

#################################
### For One Sample t-test #######
### Compare to a specific mean ##
#################################
### To test whether 'cool' is > 3.0
cool = mydata[mydata$season == 'Fall' | mydata$season == 'Winter', 5] # SapDepth = col5
mean(cool)
t.test(cool, alternative= 'greater', mu= 3.0)

#########################
# or create a new factor 
# for comparisons
#########################
Temp = rep(0,dim(mydata)[1])          # create new factor to compare in t-test
Temp[which(mydata$season == 'Fall' | mydata$season == 'Winter')] = 'COOL'
Temp[which(mydata$season == 'Spring' | mydata$season == 'Summer')] = 'WARM'
mydata = cbind(mydata, Temp)              # add factor to mydata

t.test(SapDepth ~ Temp, data = mydata, var.equal = F, paired = F)

###############################
# Non-parametric comparisons
# of 2 treatment means
# Mann-Whitney U & Wilcoxon U
###############################
# package for computing exact p-values in case of rank ties
### Be careful, exact tests do not calculate conf.int correctly
require(exactRankTests)
### Mann-Whitney test is Wilcoxon test with paired = F
wilcox.test(SapDepth ~ Temp, data = mydata, paired = F)
wilcox.exact(SapDepth ~ Temp, data = mydata, paired = F)

### Wilcoson U test (for paired data; paired = T)
wilcox.test(SapDepth ~ Temp, data = mydata, paired = T)
wilcox.exact(SapDepth ~ Temp, data = mydata, paired = T)

#############################
### Parametric comparisons
### of > 2 treatment means
######### ANOVA #############
#############################
# Conduct ANOVA on SapArea by spp. (4 treatments)
##############################
### First we must check ######
### that variances are equal #
### just like in t-test ######
###### Bartlett's test #######
##############################
bartlett.test(SapArea ~ spp, data = mydata)
fligner.test(SapArea ~ spp, data = mydata)
### bartlett = NOT equal; Fligner-Killeen test = equal (go with F-K) 

#####################
### Now the model ###
#####################
aov.model = aov(SapArea ~ spp, data = mydata)
anova(aov.model)

#####################
# Non-parameteric rank
# Kruskal-Wallis
# for > 2 means
#####################
# K-W test of Sapwood Area by spp.
kruskal.test(SapArea ~ spp, data = mydata)


#################
#####################################
### Making Boxplot with Error Bars
### I'll do it for Sapwood Depth
### because it had a nice boxplot
#####################################
#################
boxplot(SapDepth ~ spp, data= mydata, ylab= 'Sapwood Depth', col='gray50') #### We'll do SapDepth and look for differences among spp cause it looks promising

### First the ANOVA as above but with SapDepth
fligner.test(SapDepth ~ spp, data= mydata) ### Good, they're equal
aov.spp = aov(SapDepth ~ spp, data= mydata)
summary(aov.spp)

