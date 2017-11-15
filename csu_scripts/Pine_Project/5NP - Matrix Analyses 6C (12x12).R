###########################################
###### Basic Matrix Analyses ##############
###### Stu Field from Caswell #############
########## Aug 28th 2008 ##################
############ 6 Stages #####################
###########################################
rm(list=ls(all=T)) 
require(popbio)

#############################################
### Create an initial transition matrix A ###
#############################################
F.sel = 0.121                   # LITERATURE VALUES (12.1%)
S.sel.1 = 0.2               # Fitness cost to Seedlings
S.sel.2 = 0.85              # Fitness cost to Saplings
S.sel.3 = 0.90              # Fitness cost to young adults (Juvies)
S.sel.4 = 0.925             # Fitness cost to full adults
##########################################################################
Ssd = 0.000584              # Germination (Seeds to Seedlings)
Ssd1= 0.56                  # Survivorship of 1st yr. Seedlings
SLsd2 = 0.29; Ssd2= 0.15    # Survivorship of 2nd - 4th yr Seedlings
SLsp = 0.95; Ssp= 0.026     # Survivorship of Saplings
SLjv =  0.97; Sjv= 0.017    # Survivorship of Juveniles (young adults)
SLad = 0.99                 # Survivorship of full adults
FA = 996; FJ = FA*0.66      # Seed production of adults & young adults
B = 0.016                   # Yearly infection probability
I.pop = c(1000,150,75,50,25,10,0,0,0,0,0,0); Gen = 50
A <- matrix(c(0, 0, 0, 0, FJ, FA, 0, 0, 0, 0, FJ*F.sel, FA*F.sel, 
              Ssd, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
              0, Ssd1, SLsd2*(1-B/2), 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, Ssd2*(1-B/2), SLsp*(1-B), 0, 0, 0, 0, 0, 0, 0, 0, 
              0, 0, 0, Ssp*(1-B), SLjv*(1-B), 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, Sjv*(1-B), SLad*(1-B), 0, 0, 0, 0, 0, 0, 
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, SLsd2*(B/2), 0, 0, 0, 0, 0, SLsd2*S.sel.1, 0, 0, 0,
              0, 0, Ssd2*(B/2), SLsp*B, 0, 0, 0, 0, Ssd2*S.sel.1, SLsp*S.sel.2, 0, 0,
              0, 0, 0, Ssp*B, SLjv*B, 0, 0, 0, 0, Ssp*S.sel.2, SLjv*S.sel.3, 0,
              0, 0, 0, 0, Sjv*B, SLad*B, 0, 0, 0, 0, Sjv*S.sel.3, SLad*S.sel.4), 12, 12, byrow=T); A
              
pop = pop.projection(A, I.pop, Gen)      
eigen.analysis(A)

plot(1:Gen, pop$stage.vectors[1,], 'l', ylim=c(0,150), col=1, main='Total Population Size', ylab='No.Individuals', xlab='Year', lwd=2)
  lines(1:Gen, pop$stage.vectors[2,], 'l', col=2, lwd=2)
  lines(1:Gen, pop$stage.vectors[3,], 'l', col=3, lwd=2)
  lines(1:Gen, pop$stage.vectors[4,], 'l', col=4, lwd=2)
  lines(1:Gen, pop$stage.vectors[5,], 'l', col=5, lwd=2)
  lines(1:Gen, pop$stage.vectors[6,], 'l', col=6, lwd=2)
  #lines(1:Gen, pop$stage.vectors[7,], 'l', col=7, lwd=2)
  #lines(1:Gen, pop$stage.vectors[8,], 'l', col=8, lwd=2)
  lines(1:Gen, pop$stage.vectors[9,], 'l', col=3, lwd=2)
  lines(1:Gen, pop$stage.vectors[10,] ,'l', col=4, lwd=2)
  lines(1:Gen, pop$stage.vectors[11,] ,'l', col=5, lwd=2)
  lines(1:Gen, pop$stage.vectors[12,] ,'l', col=6, lwd=2)
  
  legend("topleft", c("Seed","Seedling1","Seedling2","Sapling","Juvenile","Adult","Seedling2(i)","Sapling(i)","Juvenile(i)","Adult(i)"), cex=0.7, col=c(1:12), lwd=2, lty=1, bg="gray95")

par(mfrow=c(1,2))
##################################################
### Graphing Stable Stage Distribution (SSD) #####
##################################################
prop.1 = c(rep(0,Gen)); prop.2 = c(rep(0,Gen))
prop.3 = c(rep(0,Gen)); prop.4 = c(rep(0,Gen))
prop.5 = c(rep(0,Gen)); prop.6 = c(rep(0,Gen))
prop.7 = c(rep(0,Gen)); prop.8 = c(rep(0,Gen))
prop.9 = c(rep(0,Gen)); prop.10 = c(rep(0,Gen))
prop.11 = c(rep(0,Gen)); prop.12 = c(rep(0,Gen))


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
	#prop.7[n]= pop$stage.vectors[7,n]/sum(pop$stage.vectors[,n])
	#prop.8[n]= pop$stage.vectors[8,n]/sum(pop$stage.vectors[,n])
	prop.9[n]= pop$stage.vectors[9,n]/sum(pop$stage.vectors[,n])
	prop.10[n]= pop$stage.vectors[10,n]/sum(pop$stage.vectors[,n])
	prop.11[n]= pop$stage.vectors[11,n]/sum(pop$stage.vectors[,n])
	prop.12[n]= pop$stage.vectors[12,n]/sum(pop$stage.vectors[,n])	
}
prop.1; prop.2; prop.3; prop.4; prop.5; prop.6; prop.7; prop.8; prop.9; prop.10; prop.11; prop.12

plot(1:Gen, prop.1,'l', ylim=range(0:1), main='Stable Stage Distribution', ylab='Proportion of Population', xlab='Generations', lwd=2, col=1)
  lines(1:Gen, prop.2,'l', col=2, lwd=2)
  lines(1:Gen, prop.3,'l', col=3, lwd=2)
  lines(1:Gen, prop.4,'l', col=4, lwd=2)
  lines(1:Gen, prop.5,'l', col=5, lwd=2)
  lines(1:Gen, prop.6,'l', col=6, lwd=2)
  #lines(1:Gen, prop.7,'l', col=7, lwd=2)
  #lines(1:Gen, prop.8,'l', col=8, lwd=2)
  lines(1:Gen, prop.9,'l', col=3, lwd=2)
  lines(1:Gen, prop.10,'l', col=4, lwd=2)
  lines(1:Gen, prop.11,'l', col=5, lwd=2)
  lines(1:Gen, prop.12,'l', col=6, lwd=2)
  
  legend("topleft", c("N","J","A","Ni","Ji","Ai"), cex=0.8, lwd=2, lty=1, bg="gray95", col=c(1:12))

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
eigen.analysis(A)$damping.ratio

################################################################################
#### Time to when lambda becomes 20x more important than the sub-dominant ######
#### eigenvalue, corresponds to the approx. time until SSD is reached ##########
################################################################################
t = log(20)/log(damp.ratio); t  


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
lambda = eigen.analysis(A)$lambda; lambda

E <- (1/lambda)*S*A; E      #### Hadamard product matrix multiplication
E = round(E, 7); E

### Which parameters contribute > 5% to lambda
Key.para <- matrix(c(which(E > 0.05), (E[which(E > 0.05)])), length(which(E > 0.05)), 2); Key.para; E

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