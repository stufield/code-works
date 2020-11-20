########################################################################
##################### Parameter Calculation ############################
################ For 5-needle Pine matrix model ########################
########################################################################
rm(list=ls())
##########################################################
### Calculation of Looping Survivorship ##################
### Probabilities from Residence time ####################
##########################################################
### Residence times (R) & Mortalities (M) of each class
##########################################################
Rn = 31; Rj = 12; Mn = 0.02; Mj = 0.01; Ma = 0.01
#################################################### 
Rn = seq(1, 201, by = 5)     ### create a vector for Rn to see how SLn & Sn vary as a function of residence time

################################
### METHOD 1: Akcakaya via Ettl  (don' think this can be right b/c if Rn = 1, SLn is negative, should be zero)
################################ (This method assumes dying trees do not also transition/grow to next class; reasonable assumption)
SLn = (1 - (1/Rn)) - Mn
Sn = (1/Rn)
SLj = (1 - (1/Rj)) - Mj
Sj = (1/Rj) 
SLa = 1 - Ma

Pars <- matrix(c(SLn, Sn, SLj, Sj, SLa), 1, 5); colnames(Pars) = c('SLn','Sn','SLj','Sj','SLa'); Pars
plot(Rn, SLn); abline(h = c(0,(1-Mn)), col=c('dark red','navy'))


###########################
### METHOD 2: Book Chapter   (don't think this can be right b/c if Rn > 46, Sn becomes negative)
###########################
SLn = (1 - (1/Rn))
Sn = (1 - Mn) - (1 - (1/Rn))
SLj = (1 - (1/Rj))
Sj = (1 - Mj) - (1 - (1/Rj)) 
SLa = 1 - Ma

Pars <- matrix(c(SLn, Sn, SLj, Sj, SLa), 1, 5); colnames(Pars) = c('SLn','Sn','SLj','Sj','SLa'); Pars
plot(Rn, Sn); abline(h = c(0,(1-Mn)), col=c('dark red','navy'))


##################################
### METHOD 3: Stu's method       (Simple multiplication rule of independent probabilities)
### For 3 classes ################
##################################
Rn = 41; Rj = 60; Mn = 0.02; Mj = 0.01; Ma = 0.01
###################################################
SLn = (1 - (1/Rn))*(1 - Mn)
Sn = (1/Rn)*(1 - Mn)
SLj = (1 - (1/Rj))*(1 - Mj)
Sj = (1/Rj)*(1 - Mj) 
SLa = 1 - Ma

Pars <- matrix(c(SLn, Sn, SLj, Sj, SLa), 1, 5); colnames(Pars) = c('SLn','Sn','SLj','Sj','SLa'); Pars
par(mfrow=c(1,2))
plot(Rn, SLn); abline(h = c(0,(1-Mn)), col=c('dark red','navy')); plot(Rn, Sn); abline(h = c(0,(1-Mn)), col=c('dark red','navy'))

##################################
### METHOD 3: Stu's method       (Simple multiplication rule of independent probabilities)
### For 6 classes ################
##################################
#################################################################################################
Rsd = 1; Rsd1 = 4; Rsd2 = 16; Rsp = 20; Rjv = 50               ### Residence times for stages ###
Msd1 = 0.29; Msd2 = 0.03; Msp = 0.02; Mjv = 0.01; Mad = 0.01   ### Mortality rates for stages ###
#################################################################################################
Ssd1 = round((1 - (1/Rsd1))*(1 - Msd1), 4)	### Primary Seedling Survivirship
Tsd1 = round((1/Rsd1)*(1 - Msd1), 4)		### T = transitioning to next stage class

Ssd2 = round((1 - (1/Rsd2))*(1 - Msd2), 4)	### Secondary Seedling Survivirship
Tsd2 = round((1/Rsd2)*(1 - Msd2), 4)		### T = transitioning to next stage class

Ssp = round((1 - (1/Rsp))*(1 - Msp), 4)		### Sapling Survivirship
Tsp = round((1/Rsp)*(1 - Msp), 4)			### T = transitioning to next stage class

Sjv = round((1 - (1/Rjv))*(1 - Mjv), 4)		### Young Adult Survivirship
Tjv = round((1/Rjv)*(1 - Mjv), 4)			### T = transitioning to next stage class
 
Sad = round((1 - Mad), 4)					### Mature Adult Survivirship

Vitals <- matrix(c(0, 1, Ssd1, Tsd1, Ssd2, Tsd2, Ssp, Tsp, Sjv, Tjv, Sad, NA), 6, 2, byrow=T)
colnames(Vitals) = c('Survivorship','Transition'); rownames(Vitals) = c('Seeds','Seedling1','Seedling2','Sapling','Juvenile','Adult'); Vitals
