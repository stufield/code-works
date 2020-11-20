############################################################
############# 6 CLASS INFECTION MODEL ######################
############# without Genetics 12x12 #######################
############   ANIMAL DISPERSED SP.  #######################
############################################################
rm(list=ls(all=T))
plotlevel = 4   ### Number of plots to display
###########################################################
######## Create initial stage vector stage class ##########
###########################################################
SD= 10000;	SD1= 100; SD2= 87; SP= 82; JV= 123; AD= 244
SDi= 0;		SD1i= 0; SD2i= 0;  SPi= 0;  JVi= 0;  ADi= 0
I.pop <- c(SD,	SD1, SD2, SP, JV, AD, SDi, SD1i, SD2i, SPi, JVi, ADi); stages = length(I.pop)
Gen = 1000                            ### How long will this run
dbh.v = c(0, 0, 0, 2.05, 12.5, 37)    ### Vector of mean dbh for stage classes (animal); from Coop's data

###################################################
### Matrices to store population results ##########
###################################################
popn.Mat <- matrix(0, Gen, stages)          ### overall storage of population projection
popn.Mat[1,]= I.pop                         ### first row is the initial population
Pop.Total = c(rep(0, Gen)); Pop.Total[1]= sum(I.pop) - I.pop[1]  ### Remove seeds from total population
LAI.v = c(rep(0, Gen-1))
Prev.v = c(rep(0, Gen)); Prev.v[1]= sum(I.pop[9:12])/(sum(I.pop) - I.pop[1])
SumCones.v = c(rep(0, Gen-1))			    ### Total cones of population storage vector
r.cache.v = c(rep(0, Gen-1))
TreeMort.Mat <- matrix(0, Gen-1, stages)

###################################################
#### Set linear Parameters ########################
###################################################
### Leaf Area Calculation ##############
########################################
LA.v = 0.117 * (dbh.v^1.925); LA.v[3]= 0.456         ### Don't forget the seedlings!!!!!!!!

############################
### Survivorships ##########
############################################################
Rsd = 1; Rsd1 = 4; Rsd2 = 16; Rsp = 20; Rjv = 50               ### Residence times for stages
Msd1 = 0.29; Msd2 = 0.03; Msp = 0.02; Mjv = 0.01; Mad = 0.01    ### Mortality rates for stages
############################################################
Ssd1 = round((1 - (1/Rsd1))*(1 - Msd1), 4)	### Primary Seedling Survivirship
Tsd1 = round((1/Rsd1)*(1 - Msd1), 4)		### T = transitioning to next stage class

Ssd2 = round((1 - (1/Rsd2))*(1 - Msd2), 4)	### Secondary Seedling Survivirship
Tsd2 = round((1/Rsd2)*(1 - Msd2), 4)			### T = transitioning to next stage class

Ssp = round((1 - (1/Rsp))*(1 - Msp), 4)		### Sapling Survivirship
Tsp = round((1/Rsp)*(1 - Msp), 4)				### T = transitioning to next stage class

Sjv = round((1 - (1/Rjv))*(1 - Mjv), 4)		### Young Adult Survivirship
Tjv = round((1/Rjv)*(1 - Mjv), 4)			### T = transitioning to next stage class
 
Sad = round((1 - Mad), 4)						### Mature Adult Survivirship

Vitals <- matrix(c(0, 1, Ssd1, Tsd1, Ssd2, Tsd2, Ssp, Tsp, Sjv, Tjv, Sad, NA), 6, 2, byrow=T)
colnames(Vitals) = c('Survivorship','Transition'); rownames(Vitals) = c('Seeds','Seedling1','Seedling2','Sapling','Juvenile','Adult')        

############################
#### For infected stages ###
############################
fit.cost.S = 1-exp(-0.15*(dbh.v))
fit.cost.S[2]= 0.01
fit.cost.S[3]= 0.13
fit.cost.F = 0.125
Ssd2i = Ssd2 * fit.cost.S[3]               ### Infected Seedling 2 Survivirship
Sspi = Ssp * fit.cost.S[4]                 ### Infected Sapling Survivirship
Sjvi = Sjv * fit.cost.S[5]                 ### Infected young adult Survivirship
Sadi = Sad * fit.cost.S[6]                 ### Infected adult Survivirship
mort.v = c(0, Msd1, Msd2, Msp, Mjv, Mad, 0, 0, (1-Ssd2i), (1-Sspi), (1-Sjvi), (1-Sadi)) # special vector for calculating mortalities
	
##########################
#### Beta parameters
##########################
Beta = 0.016                               ### Beta for the Adult stage (Range = 0.016 - 0.14)
Beta.v = (LA.v/LA.v) * Beta                ### Constant Beta for all classes; could change relative to certain class
B.sd2 = Beta.v[3]
B.sp = Beta.v[4]
B.jv = Beta.v[5]
B.ad = Beta.v[6]

##########################
#### Fecundity parameters    (Mean values from Cottone & Ettl; Keane et al. 1990/1996)
##########################
P.find = 0.8                  # Proportion of seeds reclaimed by Nutcrackers following season 
SPC = 3.7                     # Seeds per cache
P.cons = 0.3                  # Proportion of seeds eaten
r.cache = 0.37                # Cacheability via Nutcrackers
Cones.max = 7.5               # Cones per tree maximum (not scaled for density effects)
Seed.cone = 46                # Number of viable seeds per cone; from Owens paper
nBirds = 3					  # Number of Clark's nutcrackers caching on site
Smax = 600	* 60	          # Maximum number of seeds per nutcracker
jv.ad = 0.1					  # Relative fecundity difference between juveniles & adults (jv/ad)

##############################
#### Set up transition matrix
#### Density-independent pars
##############################
M <- matrix(c(0, 0, 0, 0, 0, 0,                          0, 0, 0, 0, 0, 0, 
              0, Ssd1, 0, 0, 0, 0,                       0, 0, 0, 0, 0, 0, 
              0, Tsd1, Ssd2*(1-B.sd2), 0, 0, 0,          0, 0, 0, 0, 0, 0,
              0, 0, Tsd2*(1-B.sd2), Ssp*(1-B.sp), 0, 0,  0, 0, 0, 0, 0, 0, 
              0, 0, 0, Tsp*(1-B.sp), Sjv*(1-B.jv), 0,    0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, Tjv*(1-B.jv), Sad*(1-B.ad),    0, 0, 0, 0, 0, 0, 
              
              0, 0, 0, 0, 0, 0,                          0, 0, 0, 0, 0, 0, 
              0, 0, 0, 0, 0, 0,                          0, 0, 0, 0, 0, 0, 
              0, 0, Ssd2*(B.sd2), 0, 0, 0,               0, 0, Ssd2i, 0, 0, 0,
              0, 0, Tsd2*(B.sd2), Ssp*(B.sp), 0, 0,      0, 0, Tsd2, Sspi, 0, 0, 
              0, 0, 0, Tsp*(B.sp), Sjv*(B.jv), 0,        0, 0, 0, Tsp, Sjvi, 0,
              0, 0, 0, 0, Tjv*(B.jv), Sad*(B.ad),        0, 0, 0, 0, Tjv, Sadi), 12, 12, byrow=T)


#############################################################
############ Loop over generations! #########################
#############################################################
for (n in 2:Gen) {
##############################
### Set non-linear parameters
##############################
### LAI Calculation
##########################
  Sum.LA = sum(LA.v * popn.Mat[n-1,])          ### NO. INDIVIDUALS IN EACH CLASS FROM popn.Mat VECTOR
  LAI = Sum.LA/10000; LAI                      ### Projected LA per area of ground (1 ha = 10000 m^2)
  LAI.v[n-1] = LAI								### Keep track of LAI over time

############################################
### Density-dependent Seedling Recruitment
############################################
  r.ALs <- exp((-0.8*LAI) + 2*(0.8))            ### available light shade factor, depends on LAI
  if (LAI <= 2) r.ALs = 1                       ### Flat top to the curve for low LAI values
  SpB = popn.Mat[n-1,1] / nBirds      			### Seeds per bird
  r.cache <- exp((SpB / Smax)^(10)) - 0.3 * (1 - (SpB / Smax)) - 0.5
  #r.cache <- exp((popn.Mat[n-1,1] /(C.nutt * Smax))^10) - 0.3 * (1 - (popn.Mat[n-1,1] / (C.nutt * Smax))) - 0.5
  if (r.cache > 1) r.cache = 1.00
  r.cache.v[n-1]= r.cache
  SR = (((1-P.find) * (1-P.cons))/SPC) * r.cache * r.ALs     ### Seedling Recruitment (proportion seeds becoming seedlings)
  M[2,1]= SR                                                 ### Set Seedling Recruitment into M

#######################################
### Density-dependent cone production
#######################################
  N.seed = Seed.cone * Cones.max * (0.5/(1+exp((LAI-2.25)/0.2)) + 0.5)   ### Total number of seeds produced by reproductive adults per year
  M[1,5]= N.seed * jv.ad; M[1,6]= N.seed      ### Set parameters into M
  M[1,11]= N.seed * fit.cost.F * jv.ad        ### Set parameters into M (multiplied by fitness cost to fecundity)
  M[1,12]= N.seed * fit.cost.F                ### Set parameters into M - reduction factor for infected class fecundity
  SumCones = sum((popn.Mat[n-1,5:6] * c((N.seed/Seed.cone) * jv.ad, (N.seed/Seed.cone))) + (popn.Mat[n-1,11:12] * c((N.seed/Seed.cone) * jv.ad, (N.seed/Seed.cone))))
  SumCones.v[n-1] = SumCones
  if (SumCones < 130) M[1,]= 0				  ### Nutcrackers won't disperse cones if < 130 cones/ha (McKinney et al. in press)
  
###############################
### Matrix Multiplication
###############################
	popn.Mat[n,]= M %*% popn.Mat[n-1,]	                       ### Matrix multiplication of population trajectory
	TreeMort.Mat[n-1,]= popn.Mat[n-1,] * mort.v	               ### Calculate # trees dying in each time step
	TreeMort.Mat[n-1,1]= popn.Mat[n-1,1] * (1-SR)              ### Reset mortality of seeds to reflect non-germinating seeds (=dead)
	#popn.Mat[n,]= ifelse(popn.Mat[n,] < 1, 0, popn.Mat[n,])   ### remove all < 1 individuals
	Pop.Total[n]= sum(popn.Mat[n,]) - popn.Mat[n,1]            ### Remove seedlings from population count
	Prev.v[n]= sum(popn.Mat[n,9:12])/(sum(popn.Mat[n,9:12]) + sum(popn.Mat[n,3:6]))   ### Calculate prevalence of rust in popn
}  # END OF LOOP

#################################################
####### Plot variables of interest ##############
#################################################
if (plotlevel >= 1) {
par(mfrow=c(1,3))
plot(1:Gen, Pop.Total, 'l', xlim=c(0,Gen), xlab="Years", main="Total Population", ylab="Total Whitebark Population: Class 2-12", col='navy', lwd=2); grid()
plot(1:(Gen-1), LAI.v, xlab="Years", main="Leaf Area Index", ylab="LAI"); grid()
plot(1:Gen, Prev.v, 'l', col='dark red', lwd=2, xlab="Years", main="Rust Prevalence", ylab="Proportion infected individuals"); grid()
}
if (plotlevel >= 2) {
par(mfrow=c(1,2))
for (p in 2:stages) {
  if (p==2) {plot(1:Gen, popn.Mat[,p], 'l', ylim=c(0,max(popn.Mat[,2:stages])), xlim=c(0,Gen), xlab="Years", main="Uninfected Population", ylab="Whitebark Population by Class", col=p, lwd=1.5); grid()
  legend('topright', legend=c(2:6), lty=c(rep(1,5)), col=c(2:6), lwd=1.5, bg='gray95')}
  if (p >= 3 && p <= 6) lines(1:Gen, popn.Mat[,p], 'l', xlim=c(0,Gen), col=p, lwd=1.5)
  
  if (p == 8) {plot(1:Gen, popn.Mat[,p], 'l', ylim=c(0,max(popn.Mat[,9:stages])), xlim=c(0,Gen), xlab="Years", main="Infected Population", ylab="Whitebark Population by Class", lty=2, col=p-6, lwd=1.5); grid()
  legend('topright', legend=c(8:12), lty=c(rep(2,5)), col=c(2:6), lwd=1.5, bg='gray95')}
  if (p >= 9) lines(1:Gen, popn.Mat[,p], 'l', xlim=c(0,Gen), lty =2, col=p-6, lwd=1.5)
  } }

if (plotlevel >= 3) {
plot(1:Gen, popn.Mat[,1], 'l', xlim=c(0,Gen), xlab="Years", main="Uninfected Seed Population", ylab="Total Seeds", col='black', lwd=2); grid()
plot(1:(Gen-1), r.cache.v, 'l', xlab='Years', ylab='r.cache', main="r.cache over time", lwd=2); grid()
  }

if (plotlevel >= 4) {
par(mfrow=c(1,2))
for (p in 2:stages){ 
  if (p==2) {plot(1:(Gen-1), TreeMort.Mat[,p], 'l', ylim=c(0,max(TreeMort.Mat[,2:stages])), xlim=c(0,Gen), xlab="Years", main="Uninfected Mortality", ylab="Tree Mortality by Class", col=p, lwd=1.5); grid()
  legend('topright', legend=c(2:6), lty=c(rep(1,5)), col=c(2:6), lwd=1.5, bg='gray95')}
  if (p >= 3 && p <= 6) lines(1:(Gen-1), TreeMort.Mat[,p], 'l', xlim=c(0,Gen), col=p, lwd=1.5)
  
  if (p == 8) {plot(1:(Gen-1), TreeMort.Mat[,p], 'l', ylim=c(0,max(TreeMort.Mat[,9:stages])), xlim=c(0,Gen), xlab="Years", main="Infected Mortality", ylab="Tree Mortality by Class", lty=2, col=p-6, lwd=1.5); grid()
  legend('topright', legend=c(8:12), lty=c(rep(2,5)), col=c(2:6), lwd=1.5, bg='gray95')}
  if (p >= 9) lines(1:(Gen-1), TreeMort.Mat[,p], 'l', xlim=c(0,Gen), lty =2, col=p-6, lwd=1.5)
  } }

popn.Mat[Gen,]
########################################################
########################################################
################### END PROGRAM ########################
########################################################
#write.csv(popn.Mat, "Popn Test.csv", row.names= F)	