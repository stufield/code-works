############################################################
############# 6 CLASS INFECTION MODEL ######################
############# without Genetics 12x12 #######################
#############  ANIMAL DISPERSED SP.  #######################
############################################################
#rm(list=ls())
######################################################
### Find external file where all parameters located
######################################################
source("/Users/sfield/Documents/CSU/R-scripts//5NP R Code/TreeFunctions.R")
# sDiag function for matrix M construction below
source("/Users/sfield/Documents/CSU/R-scripts/5NP R Code/Animal/SetParameters.R")

pine6 <- function(Gen=2, Beta=0.044, plot=1) {
#
### Gen = number of generations to run the model; default = 100
### Beta for the Adult stage (Range = 0.016 - 0.14); default = 0.04
### plotlevel = number of plots to display; default = 3
#
#####################################################
######## Create storage matrices & vectors ##########
#####################################################
popn.Mat <- matrix(0, Gen, stages)          ### overall storage of population projection
popn.Mat[1,]= I.pop                         ### first row is the initial population

Pop.Total = c(rep(0, Gen))
Pop.Total[1]= sum(I.pop) - I.pop[1]  		### Remove seeds from total population

LAI.v = c(rep(0, Gen)) 						### storage vector for LAI
LAI.v[1]= NA       

Prev.v = c(rep(0, Gen))						### storage vector for prevalence
Prev.v[1]= sum(I.pop[8:12])/(sum(I.pop[2:12]))    

SumCones.v = c(rep(0, Gen))					### storage vector for total cones in the population
SumCones.v[1]= NA    

r.cache.v = c(rep(0, Gen))					### storage vector for r.cache
r.cache.v[1]= NA	

SR.v = c(rep(0, Gen))						### storage vector for Seedling Recruitment
SR.v[1]= NA			

TreeMort.Mat <- matrix(0, Gen-1, stages)	### storage matrix for dying trees

###################################################
#### Set linear Parameters ########################
###################################################
### Leaf Area Calculation ##############
########################################
LA.v = L.par[1] * (dbh.v^L.par[2])
LA.v[3]= L.par[3]                     ### Don't forget the sedondary seedlings

######################################
#### Mortality of Classes for Anna ###
######################################
mort.v = c(0, MT[1], MT[2], MT[3], MT[4], MT[5], 0, 0, (1-S.par[3] * C.par[3]), (1-S.par[4] * C.par[4]), (1-S.par[5] * C.par[5]), (1-S.par[6] * C.par[6])) # special vector for calculating mortalities

##########################
#### Beta parameters #####
##########################
B.par = c(0, rep(Beta,5))	 ### Beta is constant for all classes; Used to make Beta Matrix (BM)
#B.par = (LA.v/LA.v) * Beta  ### This is if Beta depends on class

####################################
#### Set up projection matrix ######
######## Linear Map Matrix #########
####################################
# Survivorship Matrix
SM = diag(S.par) + sDiag(T.par[1:5],-1)
# Cost Matrix
CM = diag(C.par)  
### In this case, the lower right quadrant will be SM %*% CM
# Beta Matrix
BM = diag(B.par)
# Combine sub-matrices
M <- cbind(rbind(SM %*% (diag(6)-BM), SM %*% BM), rbind(diag(0,6), CM %*% SM))

#############################################################
############ Loop over generations! #########################
#############################################################
for (n in 2:Gen) {
##############################
### Set non-linear parameters
##############################
##### LAI Calculation ########
##############################
  Sum.LA = sum(LA.v * popn.Mat[n-1,])   ### NO. INDIVIDUALS IN EACH CLASS FROM popn.Mat VECTOR
  LAI = Sum.LA/10000	                ### Projected LA per area of ground (1 ha = 10000 m^2)
  LAI.v[n] = LAI						### Store and track of LAI over time

############################################
### Density-dependent Seedling Recruitment
############################################
  r.ALs <- 1/(1+exp(2*(LAI-3)))					### Continuous version of function for available light shading factor
  SpB = popn.Mat[n-1,1] / Bird.par[3]  			### Seeds per bird  
  r.cache <- 0.73/(1+exp((31000 - SpB)/3000)) + 0.27
  r.cache.v[n]= r.cache							### Store r.cahce for graphing
  SR = (((1-Bird.par[1]) * (1-Bird.par[2]))/F.par[4]) * r.cache * r.ALs     ### Seedling Recruitment (proportion seeds becoming seedlings)
  M[2,1]= SR; SR.v[n]= SR                 		### Set Seedling Recruitment into M & store into vector

###############################################
### Density-dependent cone production #########
###############################################
  N.seed = F.par[5] * F.par[3] * (0.5/(1+exp(5*(LAI-2.25))) + 0.5)   ### Total max number of seeds produced by reproductive adults per year
  M[1,5]= N.seed * F.par[2]; M[1,6]= N.seed      ### Set parameters into F
  M[1,11]= N.seed * F.par[1] * F.par[2]        ### Set parameters into F (multiplied by fitness cost to fecundity)
  M[1,12]= N.seed * F.par[1]                ### Set parameters into F - reduction factor for infected class fecundity
  #SumCones = sum((popn.Mat[n-1,5:6] * c((N.seed/F.par[5]) * F.par[2], (N.seed/F.par[5]))) + (popn.Mat[n-1,11:12] * c((N.seed/F.par[5]) * F.par[2], (N.seed/F.par[5]))))
  #SumCones.v[n] = SumCones				  ### store and track total cones in popn
  #if (SumCones < 130) M[1,]= 0			### Nutcrackers won't disperse cones if < 130 cones/ha (McKinney et al. 2009)
  
###############################
### Matrix Multiplication
###############################
	popn.Mat[n,]= M %*% popn.Mat[n-1,]                       ### Matrix multiplication of population trajectory
	TreeMort.Mat[n-1,]= popn.Mat[n-1,] * mort.v	               ### Calculate # trees dying in each time step
	TreeMort.Mat[n-1,1]= popn.Mat[n-1,1] * (1-SR)              ### Reset mortality of seeds to reflect non-germinating seeds (=dead)
	#popn.Mat[n,]= ifelse(popn.Mat[n,] < 1, 0, popn.Mat[n,])   ### remove all < 1 individuals
	Pop.Total[n]= sum(popn.Mat[n,]) - popn.Mat[n,1]            ### Remove seedlings from population count
	Prev.v[n]= sum(popn.Mat[n,8:12])/(sum(popn.Mat[n,2:12]))   ### Calculate prevalence of rust in popn excluding seeds

}  # END OF LOOP

#################################################
#################################################
####### Plot variables of interest ##############
#################################################
#################################################
if (plot >= 1) {
plot(1:Gen, Pop.Total, 'l', xlim=c(0,Gen), xlab="Years", main="Total Population", ylab="Total Whitebark Population: Class 2-12", col='navy', lwd=2); grid()
par(mfrow=c(1,2))
plot(1:(Gen-1), LAI.v[2:Gen], xlab="Years", main="Leaf Area Index", ylab="LAI"); grid()
plot(1:Gen, Prev.v, 'l', col='dark red', lwd=2, xlab="Years", main="Rust Prevalence", ylab="Proportion infected individuals"); grid()
}
if (plot >= 2) {
par(mfrow=c(1,2))
for (p in 2:stages) {
  if (p==2) {plot(1:Gen, popn.Mat[,p], 'l', ylim=c(0,max(popn.Mat[,2:stages])), xlim=c(0,Gen), xlab="Years", main="Uninfected Population", ylab="Whitebark Population by Class", col=p, lwd=1.5); grid()
  legend('topright', legend=c(2:6), lty=c(rep(1,5)), col=c(2:6), lwd=1.5, bg='gray95')}
  if (p >= 3 && p <= 6) lines(1:Gen, popn.Mat[,p], 'l', xlim=c(0,Gen), col=p, lwd=1.5)
  
  if (p == 8) {plot(1:Gen, popn.Mat[,p], 'l', ylim=c(0,max(popn.Mat[,9:stages])), xlim=c(0,Gen), xlab="Years", main="Infected Population", ylab="Whitebark Population by Class", lty=2, col=p-6, lwd=1.5); grid()
  legend('topright', legend=c(8:12), lty=c(rep(2,5)), col=c(2:6), lwd=1.5, bg='gray95')}
  if (p >= 9) lines(1:Gen, popn.Mat[,p], 'l', xlim=c(0,Gen), lty =2, col=p-6, lwd=1.5)
  } }

if (plot >= 3) {
plot(1:Gen, popn.Mat[,1], 'l', xlim=c(0,Gen), xlab="Years", main="Uninfected Seed Population", ylab="Total Seeds", col='black', lwd=2); grid()
plot(1:(Gen-1), r.cache.v[2:Gen], 'l', xlab='Years', ylab='r.cache', main="r.cache over time", lwd=2); grid()
  }

if (plot >= 4) {
par(mfrow=c(1,2))
for (p in 2:stages){ 
  if (p==2) {plot(1:(Gen-1), TreeMort.Mat[,p], 'l', ylim=c(0,max(TreeMort.Mat[,2:stages])), xlim=c(0,Gen), xlab="Years", main="Uninfected Mortality", ylab="Tree Mortality by Class", col=p, lwd=1.5); grid()
  legend('topright', legend=c(2:6), lty=c(rep(1,5)), col=c(2:6), lwd=1.5, bg='gray95')}
  if (p >= 3 && p <= 6) lines(1:(Gen-1), TreeMort.Mat[,p], 'l', xlim=c(0,Gen), col=p, lwd=1.5)
  
  if (p == 8) {plot(1:(Gen-1), TreeMort.Mat[,p], 'l', ylim=c(0,max(TreeMort.Mat[,9:stages])), xlim=c(0,Gen), xlab="Years", main="Infected Mortality", ylab="Tree Mortality by Class", lty=2, col=p-6, lwd=1.5); grid()
  legend('topright', legend=c(8:12), lty=c(rep(2,5)), col=c(2:6), lwd=1.5, bg='gray95')}
  if (p >= 9) lines(1:(Gen-1), TreeMort.Mat[,p], 'l', xlim=c(0,Gen), lty =2, col=p-6, lwd=1.5)
  } }

	Out <- list(popn.Mat, Pop.Total, Prev.v, LAI.v, r.cache.v, SR.v, M)    ### What pine6 returns
	names(Out) <- c('Pop_Projection', 'Pop_Totals', 'Prevalence', 'LAI', 'r_cache', 'SR', 'Projection_Matrix')
	Out
}	# END OF pine6
