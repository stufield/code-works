############################################################
############# 3 CLASS GENETICS MODEL #######################
############# without Infection 9x9 ########################
############################################################
require(popbio)
rm(list=ls())
pro <- function(v,x) {v[x]/sum(v)} ### Proportion of a cell x within a sector v

###########################################################
######## Create initial stage vector by genotype ##########
###########################################################

NRR=9;	JRR=0;	ARR=0
NRr=42;	JRr=0;	ARr=0
Nrr=49;	Jrr=0;	Arr=0 
par.data <- read.table('5NP Vitals.csv', sep=',', header=T)
I.pop <- c(NRR,JRR,ARR,NRr,JRr,ARr,Nrr,Jrr,Arr); stages = length(I.pop)
Gen = 25                        ### How long will this run
Lit.Val = F                     ### if true use literature values for survivorship; if false use dummy simplified values
S.vec = c(0.95, 0.05, 0.95, 0.05, 0.95)  ### Set simple/predictable survivorships & transitions
if (Lit.Val) S.vec = c(par.data$Value[1],par.data$Value[2],par.data$Value[3],par.data$Value[4],par.data$Value[5])  ### Ts & Ss from literature
FA = FJ = 1                     ### For now...
F.boost = F                     ### if true fecundity is doubled (check to see if Fecundity will bring the popn back)

###################################################
### Matrices to store population results ##########
###################################################
popn.Mat <- matrix(0, Gen, stages)          # overall storage of population projection
popn.Mat[1,]= I.pop                         # first row is the initial population
popn.Mat.S <- matrix(0, Gen-1, stages)      # survival matrix to track surviving individuals before reproduction

#####################################################
########### Outputs of interest #####################
#####################################################
freq.R = c(rep(0, Gen)); freq.R[1] = (2*(sum(I.pop[1:3]))+(sum(I.pop[4:6])))/(sum(I.pop)*2)
freq.r = c(rep(0, Gen)); freq.r[1] = (2*(sum(I.pop[7:9]))+(sum(I.pop[4:6])))/(sum(I.pop)*2)
Pop.Total = c(rep(0,Gen)); Pop.Total[1]= sum(I.pop)
lambda.vec = c(rep(0,Gen)); lambda.vec[1]= NA

##############################################################################
##### Create vectors to store parameters for population matrices ############
##############################################################################
#######################################################################################################
######## Survival ############### Outside of loop because survivorship is static/linear ###############
#######################################################################################################
  par.vec = c(rep(0, stages^2))
	par.vec[1]=  S.vec[1]
	par.vec[10]= S.vec[2]
	par.vec[11]= S.vec[3]
	par.vec[20]= S.vec[4]
	par.vec[21]= S.vec[5]
	par.vec[31]= S.vec[1]
	par.vec[40]= S.vec[2]
	par.vec[41]= S.vec[3]
	par.vec[50]= S.vec[4]
	par.vec[51]= S.vec[5]
	par.vec[61]= S.vec[1]
	par.vec[70]= S.vec[2]
	par.vec[71]= S.vec[3]
	par.vec[80]= S.vec[4]
	par.vec[81]= S.vec[5]
  SM <- matrix(par.vec, stages, stages, byrow=T); #SM

#############################################################
############ Loop over generations! #########################
#############################################################
for (n in 2:Gen) {

#######################################
#########  Survival of Classes  #######
#######################################
	S.popn.vec = SM%*%popn.Mat[n-1,]; S.popn.vec    # matrix multiplication for survivorship
	popn.Mat.S[n-1,]= S.popn.vec                    # create matrix to follow the survivorship part only

#################################################################
#############  Mating & Reproduction  ###########################
#################################################################
#### Get frequencies of each genotype in mating population ######
#################################################################
	Mate.pop <- S.popn.vec[c(2,3,5,6,8,9)]; Mate.pop      # pick out J & A individuals who mate (i.e. mating popn)
	Sum.Mpop = sum(Mate.pop)                              # sum of the mating population
	
	Pj = Mate.pop[1];		Hj = Mate.pop[3];		Qj = Mate.pop[5]            # assigne numbers of each class within the mating popn
	Pa = Mate.pop[2];		Ha = Mate.pop[4];		Qa = Mate.pop[6]            # P = RR; H = Rr; Q = rr; juvie or adult

###############################################################################
###### Calculate expected numbers of each genotype in the next generation #####
###############################################################################
#### Fecundity separate from Survivorship #####################################
  par.vec.F = c(rep(0, stages^2))             # create vector to store results of matings for input into Fecundity matrix (FM)

  ##### Row 1 of the Fecundity matrix ################################################
	par.vec.F[2]= FJ * (Pj + Pa + 0.5*(Hj + Ha))/Sum.Mpop                       # Contribution of JRR to NRR
	par.vec.F[3]= FA * (Pj + Pa + 0.5*(Hj + Ha))/Sum.Mpop                       # Contribution of ARR to NRR
	par.vec.F[5]= FJ * (0.5*(Pj + Pa) + 0.25*(Hj + Ha))/Sum.Mpop                # Contribution of JRr to NRR
	par.vec.F[6]= FA * (0.5*(Pj + Pa) + 0.25*(Hj + Ha))/Sum.Mpop                # Contribution of ARr to NRR
	
  ##### Row 4 of the Fecundity matrix ################################################
	par.vec.F[29]= FJ * (Qj + Qa + 0.5*(Hj + Ha))/Sum.Mpop                      # Contribution of JRR to NRr
	par.vec.F[30]= FA * (Qj + Qa + 0.5*(Hj + Ha))/Sum.Mpop                      # Contribution of ARR to NRr
	par.vec.F[32]= FJ * (0.5 * (Hj + Ha + Pj + Pa + Qj + Qa))/Sum.Mpop          # Contribution of JRr to NRr
	par.vec.F[33]= FA * (0.5 * (Hj + Ha + Pj + Pa + Qj + Qa))/Sum.Mpop          # Contribution of ARr to NRr
	par.vec.F[35]= FJ * (Pj + Pa + 0.5*(Hj + Ha))/Sum.Mpop                      # Contribution of Jrr to NRr
	par.vec.F[36]= FA * (Pj + Pa + 0.5*(Hj + Ha))/Sum.Mpop                      # Contribution of Arr to NRr
	
  ##### Row 7 of the Fecundity matrix ################################################	
  par.vec.F[59]= FJ * (0.5*(Qj + Qa) + 0.25*(Hj + Ha))/Sum.Mpop               # Contribution of JRr to Nrr
	par.vec.F[60]= FA * (0.5*(Qj + Qa) + 0.25*(Hj + Ha))/Sum.Mpop               # Contribution of ARr to Nrr
	par.vec.F[62]= FJ * (Qj + Qa + 0.5*(Hj + Ha))/Sum.Mpop                      # Contribution of Jrr to Nrr
	par.vec.F[63]= FA * (Qj + Qa + 0.5*(Hj + Ha))/Sum.Mpop                      # Contribution of Arr to Nrr
	
	if (F.boost) par.vec.F = par.vec.F*2                         # if True, give a Fecundity boost, all genotypes' Fecundity doubled
  ID <- diag(stages)

	###### Matrix multiplication for Fecundity ######################################
	FM <- matrix(par.vec.F, stages, stages, byrow=T)     # Fill in Fecundity matrix (FM) with parameters calculated above from mating popn
  FM <- FM + ID                                          # Add ID to get 1s along diagonal
  popn.Mat[n,] = FM%*%S.popn.vec                       # Matrix multiplication to get next generation population (both Survivor & Fecund)
  I.ID <- ID+1                                         # I.ID = inverse Identity Matrix (1s with 0s along the diagonal
  I.ID[which(I.ID != 1)]= 0
  FM <- FM*I.ID                                        # Multiply by Inverse Identity Matrix to remove diagonal 1s
  Mat <- SM + FM                                        # Add two matrices for eigen analysis below
  
###################################################
########### OUTPUTS OF INTEREST ###################
###################################################
	freq.R[n] = (2*(sum(popn.Mat[n,1:3]))+(sum(popn.Mat[n,4:6])))/(sum(popn.Mat[n,])*2)
	freq.r[n] = (2*(sum(popn.Mat[n,7:9]))+(sum(popn.Mat[n,4:6])))/(sum(popn.Mat[n,])*2)
	Pop.Total[n] = sum(popn.Mat[n,])
	lambda.vec[n]= eigen.analysis(Mat)$lambda         # calculate dom. eigenvalue for each new matrix (Mat) that's made (F differs each time)
	
}  # END OF LOOP

#####################################################
### Look at a table of the results ##################
#####################################################
Totals.Mat = cbind(1:Gen, Pop.Total, freq.R, lambda.vec, popn.Mat)
colnames(Totals.Mat) = c('Gen','Total','p(R)','Lambda','NRR','JRR','ARR','NRr','JRr','ARr','Nrr','Jrr','Arr')
round(Totals.Mat, 4)

#################################################
####### Plot variables of interest ##############
#################################################
par(mfrow=c(1,3))
plot(1:Gen, Pop.Total, 'l', xlim=c(0,Gen), xlab="Years", main="Total Population", ylab="Total Whitebark Population", col='navy', lwd=2)

plot(1:Gen, freq.R, 'l', ylim=range(0:1.0), ylab="Allele Frequency (R) in Population", main="Frequency R",	xlim=c(0,Gen), xlab="Years", col='dark blue', lwd=2)
  lines(1:Gen, freq.r, 'l', lwd=2, col='dark red', lty=2)
  text(Gen*0.5, 0.5, "R = Blue")

plot(1:(Gen), lambda.vec, 'l', lwd=2, ylab="Eqm Growth Rate", main="Lambda ",	xlim=c(0,Gen), xlab="Years", col='dark green')
########################################################
########################################################
################### END PROGRAM ########################
########################################################
########################################################
#fix(Totals.Mat)
#write.table(round(popn.Mat,4), file="Output Table.csv", sep=',', row.names=F, col.names=T)