###########################################
###### Basic Matrix Analyses ##############
###### Stu Field from Caswell #############
########## July 18th 2008 #################
###########################################
rm(list=ls(all=T)) 
require(popbio)

#############################################
### Create an initial transition matrix A ###
#############################################
s1 = 0.97; s2 = 0.08; s3 = 0.93; s4 = 0.06; s5 = 0.99
FA = 0.14; FJ = FA*0.66       
B = 0.015
F.sel = 0.12
S.sel = 0.9
I.pop = c(1,1,1,0,0,0); Gen = 50
A <- matrix(c(s1*(1-B), FJ, FA, 0, FJ*F.sel, FA*F.sel, 
              s2*(1-B), s3*(1-B), 0, 0, 0, 0, 
              0, s4*(1-B), s5*(1-B), 0, 0, 0,
              s1*B, 0, 0, s1*S.sel, 0, 0,
              s2*B, s3*B, 0, s2*S.sel, s3*S.sel, 0,
              0, s4*B, s5*B, 0, s4*S.sel, s5*S.sel), 6, 6, byrow=T); A
              
pop = pop.projection(A, I.pop, Gen)
eigen.analysis(A)

plot(1:Gen, pop$stage.vectors[1,],'l', ylim=range(0:5), col='black', main='Total Population Size', ylab='No.Individuals', xlab='Year', lwd=2)
  lines(1:Gen, pop$stage.vectors[2,],'l', col='dark red', lwd=2)
  lines(1:Gen, pop$stage.vectors[3,],'l', col='dark green', lwd=2)
  lines(1:Gen, pop$stage.vectors[4,],'l', col='dark blue', lwd=2)
  lines(1:Gen, pop$stage.vectors[5,],'l', col='gold', lwd=2)
  lines(1:Gen, pop$stage.vectors[6,],'l', col='darkorchid', lwd=2)
  legend("topleft", c("N","J","A","Ni","Ji","Ai"),
    cex=0.7, col=c('black','dark red','dark green','dark blue','gold','darkorchid'), lwd=2, lty=1, bg="gray95")

par(mfrow=c(1,2))
##################################################
### Graphing Stable Stage Distribution (SSD) #####
##################################################
prop.1 = c(rep(0,Gen)); prop.2 = c(rep(0,Gen))
prop.3 = c(rep(0,Gen)); prop.4 = c(rep(0,Gen))
prop.5 = c(rep(0,Gen)); prop.6 = c(rep(0,Gen))

##############
### Loop #####
##############
for (n in 1:Gen) {
	prop.1[n]= pop$stage.vectors[1,n]/sum(pop$stage.vectors[,n])
	prop.2[n]= pop$stage.vectors[2,n]/sum(pop$stage.vectors[,n])
	prop.3[n]= pop$stage.vectors[3,n]/sum(pop$stage.vectors[,n])
	prop.4[n]= pop$stage.vectors[4,n]/sum(pop$stage.vectors[,n])
	prop.5[n]= pop$stage.vectors[5,n]/sum(pop$stage.vectors[,n])
	prop.6[n]= pop$stage.vectors[6,n]/sum(pop$stage.vectors[,n])
}
prop.1; prop.2; prop.3; prop.4; prop.5; prop.6

plot(1:Gen, prop.1,'l', ylim=range(0:1), main='Stable Stage Distribution', ylab='Proportion of Population', xlab='Generations', lwd=2, col='black')
  lines(1:Gen, prop.2,'l', col='dark red', lwd=2)
  lines(1:Gen, prop.3,'l', col='dark green', lwd=2)
  lines(1:Gen, prop.4,'l', col='dark blue', lwd=2)
  lines(1:Gen, prop.5,'l', col='gold', lwd=2)
  lines(1:Gen, prop.6,'l', col='darkorchid', lwd=2)
  legend("topleft", c("N","J","A","Ni","Ji","Ai"), cex=0.8, lwd=2, lty=1, bg="gray95",
      col=c('black','dark red','dark green','dark blue','gold','darkorchid'))


################################################################################
############### Calculate RIGHT Eigenvectors ###################################
### The SSD is the Normalized Right Eigenvector of the Dominant Eigenvalue #####
################################################################################
eig.A = eigen(A); Mod(eig.A$vectors)
SSD = Mod(eig.A$vectors[,1])/sum(Mod(eig.A$vectors[,1])); round(SSD,5)

#### OR use popbio Package #####
SSD = eigen.analysis(A)$stable.stage; round(SSD,5)


################################################################################
#################### Calculate LEFT Eigenvectors ###############################
### The RV is the Normalized Left Eigenvector of the Dominant Eigenvalue #######
### The Left Eigenvalue is the Right Eigenvalue of the transposed matrix #######
################################################################################
a = t(A); a
eig.a = eigen(a); Mod(eig.a$vectors)
RV = Mod(1/eig.a$vectors[1,1])*Mod(eig.a$vectors[,1]); RV     ### RV relative to 1st Stage Class = 1

#### OR use popbio Package #####
RV = eigen.analysis(A)$repro.value; RV

###################################################
####### Time lag until SSD is reached #############
####### Calculated using damping ratio ############
### Damping Ratio is the contribution of ##########
### the dominant to the population relative #######
####### to the sub-dominant eigenvalue ############
###################################################
damp.ratio = Mod(eig.A$values[1])/Mod(eig.A$values[2]); damp.ratio

### OR use PopBio to calculate Damping Ratio
damp.ratio = eigen.analysis(A)$damping.ratio

################################################################################
#### Time to when lambda becomes 20x more important than the sub-dominant ######
#### eigenvalue, corresponds to the approx. time until SSD is reached ##########
################################################################################
t = log(20)/log(damp.ratio); t  


###################################################################
###### Now Perturb vital rates 10% each, hold others constant #####
###################################################################
#### Newbie survivorship uninfected
A1 <- matrix(c(s1*(1-B)*(0.9), FJ, FA, 0, FJ*F.sel, FA*F.sel, 
              s2*(1-B), s3*(1-B), 0, 0, 0, 0, 
              0, s4*(1-B), s5*(1-B), 0, 0, 0,
              s1*B, 0, 0, s1*S.sel, 0, 0,
              s2*B, s3*B, 0, s2*S.sel, s3*S.sel, 0,
              0, s4*B, s5*B, 0, s4*S.sel, s5*S.sel), 6, 6, byrow=T)
#### Juvie survivorship uninfected
A2 <- matrix(c(s1*(1-B), FJ, FA, 0, FJ*F.sel, FA*F.sel, 
              s2*(1-B), s3*(1-B)*(0.9), 0, 0, 0, 0, 
              0, s4*(1-B), s5*(1-B), 0, 0, 0,
              s1*B, 0, 0, s1*S.sel, 0, 0,
              s2*B, s3*B, 0, s2*S.sel, s3*S.sel, 0,
              0, s4*B, s5*B, 0, s4*S.sel, s5*S.sel), 6, 6, byrow=T)
#### Adult survivorship uninfected
A3 <- matrix(c(s1*(1-B), FJ, FA, 0, FJ*F.sel, FA*F.sel, 
              s2*(1-B), s3*(1-B), 0, 0, 0, 0, 
              0, s4*(1-B), s5*(1-B)*(0.9), 0, 0, 0,
              s1*B, 0, 0, s1*S.sel, 0, 0,
              s2*B, s3*B, 0, s2*S.sel, s3*S.sel, 0,
              0, s4*B, s5*B, 0, s4*S.sel, s5*S.sel), 6, 6, byrow=T)
#### Adult Fecundity
A4 <- matrix(c(s1*(1-B), FJ, FA*(0.9), 0, FJ*F.sel, FA*F.sel, 
              s2*(1-B), s3*(1-B), 0, 0, 0, 0, 
              0, s4*(1-B), s5*(1-B), 0, 0, 0,
              s1*B, 0, 0, s1*S.sel, 0, 0,
              s2*B, s3*B, 0, s2*S.sel, s3*S.sel, 0,
              0, s4*B, s5*B, 0, s4*S.sel, s5*S.sel), 6, 6, byrow=T)
#### Juvie uninfected transition to Adults
A5 <- matrix(c(s1*(1-B), FJ, FA, 0, FJ*F.sel, FA*F.sel, 
              s2*(1-B)*(0.9), s3*(1-B), 0, 0, 0, 0, 
              0, s4*(1-B), s5*(1-B), 0, 0, 0,
              s1*B, 0, 0, s1*S.sel, 0, 0,
              s2*B, s3*B, 0, s2*S.sel, s3*S.sel, 0,
              0, s4*B, s5*B, 0, s4*S.sel, s5*S.sel), 6, 6, byrow=T)

#########################################################
### Project the population to get data on individuals ###
############ Use popbio Package #########################
#########################################################
pop1 = pop.projection(A1, I.pop, Gen)
pop2 = pop.projection(A2, I.pop, Gen)
pop3 = pop.projection(A3, I.pop, Gen)
pop4 = pop.projection(A4, I.pop, Gen)
pop5 = pop.projection(A5, I.pop, Gen)


#######################################################################
############ Graphical representation of Sensitivity ##################
#######################################################################
plot(1:Gen, pop$pop.sizes,'l', ylab='Total Population', lwd=2, lty=2, xlab='Year', ylim=range(0:8))
  lines(1:Gen, pop1$pop.sizes, lwd=2, col='dark red')
  lines(1:Gen, pop2$pop.sizes, lwd=2, col='dark blue')
  lines(1:Gen, pop3$pop.sizes, lwd=2, col='dark green')
  lines(1:Gen, pop4$pop.sizes, lwd=2, col='gold')
  lines(1:Gen, pop5$pop.sizes, lwd=2, col='darkorchid')
  legend("topleft", c("Original","N. Survival","J. Survival","A. Survival","A. Fecund","J. to A."),
        cex=0.9, col=c('black','dark red','dark blue','dark green','gold','darkorchid'), lwd=2, lty=c(2,1,1,1,1,1), bg="gray95")

#### This is precisely what the elasticity analysis tells us
#### uninfected Adult survival is the most influential parameter
E <- eigen.analysis(A)$elasticities; round(E, 6)
Key.para <- matrix(c(which(E > 0.010), (E[which(E > 0.010)])), length(which(E > 0.010)), 2); Key.para; E


####################################
#### Calculate Sensitivity Matrix ##
######################################################################################
### Gets very complicated; must make demominator a scalar not a matrix by adding 'c'
### R treats v and w as column vectors depending on operation; messes everything up
### Below seems to work for real numbers but may not be universal ####################
######################################################################################
S <- (RV%*%t(SSD))/c(t(RV)%*%SSD); S
S = round(S, 5); S

####################################
#### Calculate Elasticity Matrix ###
####################################
lambda = eigen.analysis(A)$lambda

E <- (1/lambda)*S*A; E      #### Hadamard product matrix multiplication
E = round(E, 7); E

### Which parameters contribute > 5% to lambda
Key.para <- matrix(c(which(E > 0.050), (E[which(E > 0.050)])), length(which(E > 0.050)), 2); Key.para; E

######################################################
######## Keyfitz's Delta: Slightly simpler method ####
#### Doesn't distinguish the path to convergence #####
############ only distance from it ################### 
######################################################
Delta = 0.5*(sum(Mod(I.pop-SSD))); Delta 


################################################################################
################ Population Momentum: if at time t you change ##################
#### the vital rates, uless the stage distr matches exactly there will #########
########## be a period of time necessary to move to the new SSD  ###############
################## which corresponds to the new vital rates ####################
################################################################################