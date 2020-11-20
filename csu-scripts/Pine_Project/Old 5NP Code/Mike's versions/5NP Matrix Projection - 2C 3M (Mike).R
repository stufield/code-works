########################################################################
################ SIMPLE 2 CLASS GENETICS MODEL #########################
########################################################################
#require(popbio)
rm(list=ls(all=TRUE))
pro <- function(v,x) {v[x]/sum(v)} ### Proportion of a cell x within a sector v

###########################################################
######## Create initial stage classified genotypes ########
###########################################################

NRR=0;	ARR=0
NRr=100;	ARr=0
Nrr=0;	Arr=0
NRRi=0;	ARRi=0
NRri=0;	ARri=0
Nrri=0;	Arri=0
I.pop <- c(NRR,ARR,NRr,ARr,Nrr,Arr,NRRi,ARRi,NRri,ARri,Nrri,Arri); stages = length(I.pop)

###############################################################
### Set parameters for survival, transitions & fecundity ######
###############################################################
Gen = 10         #  How long will this run
s1 = 0.95; s2 = 0.05; s3 = 0.95
SLn = SLin = s1  # survive within newbie class
Sn = Sin = s2    # transmision between classes	
SLa = SLia = s3  # survive within adult class
F = Fi = 1       # fecundity (of infected)

###################################################
### Matrices to store population results ############
###################################################
popn.Mat <- matrix(0,Gen,stages)          # overall storage of population projection
popn.Mat[1,]= I.pop
pop.Mat.surv <- matrix(0,Gen-1,stages)    # first multiplication of survival
#pop.Mat.surv[1,]= I.pop
pop.Mat.trans <- matrix(0,Gen-1,stages)   # second mutiplicaiton of infection
#pop.Mat.fecund <- matrix(0,Gen,stages)   # third mutiplication for reproduction
#pop.Mat <- matrix(0,Gen,stages)          # storage matrix for results through the loop 

#####################################################
######### Define infection rates  ###################
#####################################################
Beta.vec = seq(0,1,length=6)
Rep = length(Beta.vec)
Totals.Mat <- matrix(0,(Rep*Gen),15)      # storage matrix for total population for plotting, etc
colnames(Totals.Mat) = c("Beta","Gen","Total","NRR","ARR","NRr","ARr","Nrr","Arr","NRRi","ARRi",
                         "NRri","ARri","Nrri","Arri")
count = 0      # index for storing data

###############################################
####### Double Loop Begin #####################
###############################################

for (c in 1:Rep) {                        # Looping over valaues of Beta
     Beta = Beta.vec[c]

####################################################################################
########### STORE INITIAL CONDITIONS FOR PRINTING AND PLOTTING #####################
####################################################################################

count=count+1
Totals.Mat[count,1]=Beta
Totals.Mat[count,2]=1
Totals.Mat[count,3]=sum(I.pop)
Totals.Mat[count,4]=I.pop[1]
Totals.Mat[count,5]=I.pop[2]
Totals.Mat[count,6]=I.pop[3]
Totals.Mat[count,7]=I.pop[4]
Totals.Mat[count,8]=I.pop[5]
Totals.Mat[count,9]=I.pop[6]
Totals.Mat[count,10]=I.pop[7]
Totals.Mat[count,11]=I.pop[8]
Totals.Mat[count,12]=I.pop[9]
Totals.Mat[count,13]=I.pop[10]
Totals.Mat[count,14]=I.pop[11]
Totals.Mat[count,15]=I.pop[12]

##############################################################################
### Create vectors to store parameters for population matrices   #############
##############################################################################
######################################################
######## Survival or Transission of Classes ##########
######################################################
	# Should eventually be within the Gen Loop ##############################
	par.vec.surv <- c(rep(0,(stages^2)))
	par.vec.surv[1] = SLn
	par.vec.surv[13] = Sn
	par.vec.surv[14] = SLa
	par.vec.surv[27] = SLn
	par.vec.surv[39] = Sn
	par.vec.surv[40] = SLa
	par.vec.surv[53] = SLn
	par.vec.surv[65] = Sn
	par.vec.surv[66] = SLa
	par.vec.surv[79] = SLin
	par.vec.surv[91] = Sin
	par.vec.surv[92] = SLia
	par.vec.surv[105] = SLin
	par.vec.surv[117] = Sin
	par.vec.surv[118] = SLia
	par.vec.surv[131] = SLin
	par.vec.surv[143] = Sin
	par.vec.surv[144] = SLia
      M.surv = matrix(par.vec.surv,stages,stages,byrow=T); M.surv

######################################################
############# Infection Process ######################
######################################################

	par.vec.trans <- c(rep(0,(stages^2)))
	par.vec.trans[1] = 1-Beta
      par.vec.trans[14] = 1-Beta
      par.vec.trans[27] = 1-Beta
      par.vec.trans[40] = 1-Beta
      par.vec.trans[53] = 1-Beta
      par.vec.trans[66] = 1-Beta
      par.vec.trans[73] = Beta
      par.vec.trans[86] = Beta
      par.vec.trans[99] = Beta
      par.vec.trans[112] = Beta
      par.vec.trans[125] = Beta
      par.vec.trans[138] = Beta
      par.vec.trans[79] = 1
      par.vec.trans[92] = 1
      par.vec.trans[105] = 1
      par.vec.trans[118] = 1
      par.vec.trans[131] = 1
      par.vec.trans[144] = 1
      M.trans = matrix(par.vec.trans,stages,stages,byrow=T); M.trans

####################################################################################
########################### Now the Loop over generations! #########################
####################################################################################
for (n in 2:Gen) {

########################################################
#########  Survival & Transition of Classes  ###########
########################################################
# Survival or Transission of Classes and definition of par.vec.surv goes here
pop.Mat.surv[n-1,] = M.surv%*%popn.Mat[n-1,]; pop.Mat.surv

########################################################
############### Infection Process ######################
########################################################
# Infection Process and definition of par.vec.trans goes here
pop.Mat.trans[n-1,] = M.trans%*%pop.Mat.surv[n-1,]; pop.Mat.trans

########################################
####  Mating & Reproduction  ###########
########################################
#################################################################
#### Get frequencies of each genotype in mating population ######
#################################################################
Mate.pop <- pop.Mat.trans[n-1,][c(2,4,6,8,10,12)]; Mate.pop

	P=pro(Mate.pop,1); 	H=pro(Mate.pop,2); 	Q=pro(Mate.pop,3)
	Pi=pro(Mate.pop,4); 	Hi=pro(Mate.pop,5); 	Qi=pro(Mate.pop,6)

#          1     2      3       4       5      6 
#mpv = c(p.ARR, p.ARr, p.Arr, p.ARRi, p.ARri, p.Arri); mpv

############################################################################################
######  Get expected numbers of each genotype in the next generation, originating       ####
######  from infected and uninfected parents of this generation                         ####
############################################################################################

RR = (P*P)+(0.5*P*H)+(0.5*H*P)+(0.25*H*H)+(P*Pi)+(0.5*P*Hi)+(0.5*H*Pi)+(0.25*H*Hi)
Rr = (0.5*P*H)   +(P*Q)  +(0.5*H*P)  +(0.5*H*H)  +(0.5*H*Q)  +(Q*P)  +(0.5*Q*H)+
     (0.5*P*Hi)  +(P*Qi) +(0.5*H*Pi) +(0.5*H*Hi) +(0.5*H*Qi) +(Q*Pi) +(0.5*Q*Hi)
rr = (0.25*H*H)+(0.5*H*Q)+(0.5*Q*H)+(Q*Q)+(0.25*H*Hi)+(0.5*H*Qi)+(0.5*Q*Hi)+(Q*Qi)
RR;Rr;rr;sum(RR,Rr,rr)

RRi = (Pi*P)+(0.5*Pi*H)+(0.5*Hi*P)+(0.25*Hi*H)+(Pi*Pi)+(0.5*Pi*Hi)+(0.5*Hi*Pi)+(0.25*Hi*Hi)
Rri = (0.5*Pi*H)   +(Pi*Q)  +(0.5*Hi*P)  +(0.5*Hi*H)  +(0.5*Hi*Q)  +(Qi*P)  +(0.5*Qi*H)+
      (0.5*Pi*Hi)  +(Pi*Qi) +(0.5*Hi*Pi) +(0.5*Hi*Hi) +(0.5*Hi*Qi) +(Qi*Pi) +(0.5*Qi*Hi)
rri = (0.25*Hi*H)+(0.5*Hi*Q)+(0.5*Qi*H)+(Qi*Q)+(0.25*Hi*Hi)+(0.5*Hi*Qi)+(0.5*Qi*Hi)+(Qi*Qi)
RRi;Rri;rri;sum(RRi,Rri,rri)

P=Pj;H=Hj;Q=Qj
Pi=Pa;Hi=Ha;Qi=Qa

##########################################################################
####  Reproduction of newbies of each genotype & infection class  ########
##########################################################################
NRR = F*RR*sum(Mate.pop)
NRr = F*Rr*sum(Mate.pop)
Nrr = F*rr*sum(Mate.pop)
NRRi = Fi*RRi*sum(Mate.pop)
NRri = Fi*Rri*sum(Mate.pop)
Nrri = Fi*rri*sum(Mate.pop)

###################################################
### Add newbies to population & make popn.Mat #####
###################################################

popn.Mat[n,1]= NRR + NRRi + pop.Mat.trans[n-1,1]
popn.Mat[n,2]= pop.Mat.trans[n-1,2]
popn.Mat[n,3]= NRr + NRri + pop.Mat.trans[n-1,3]
popn.Mat[n,4]= pop.Mat.trans[n-1,4]
popn.Mat[n,5]= Nrr + Nrri + pop.Mat.trans[n-1,5]
popn.Mat[n,6]= pop.Mat.trans[n-1,6]
popn.Mat[n,7]= pop.Mat.trans[n-1,7]
popn.Mat[n,8]= pop.Mat.trans[n-1,8]
popn.Mat[n,9]= pop.Mat.trans[n-1,9]
popn.Mat[n,10]= pop.Mat.trans[n-1,10]
popn.Mat[n,11]= pop.Mat.trans[n-1,11]
popn.Mat[n,12]= pop.Mat.trans[n-1,12]

##########################################################################
########### STORE RESULTS FOR PRINTING AND PLOTTING  #####################
##########################################################################

count= count+1
Totals.Mat[count,1]= Beta
Totals.Mat[count,2]= n
Totals.Mat[count,3]= sum(popn.Mat[n,])
Totals.Mat[count,4]= popn.Mat[n,1]
Totals.Mat[count,5]= popn.Mat[n,2]
Totals.Mat[count,6]= popn.Mat[n,3]
Totals.Mat[count,7]= popn.Mat[n,4]
Totals.Mat[count,8]= popn.Mat[n,5]
Totals.Mat[count,9]= popn.Mat[n,6]
Totals.Mat[count,10]= popn.Mat[n,7]
Totals.Mat[count,11]= popn.Mat[n,8]
Totals.Mat[count,12]= popn.Mat[n,9]
Totals.Mat[count,13]= popn.Mat[n,10]
Totals.Mat[count,14]= popn.Mat[n,11]
Totals.Mat[count,15]= popn.Mat[n,12]

   }  # END OF GENERATION LOOP
}  #  END OF BETA LOOP

Totals.Mat

plot(1:Gen, Totals.Mat[1:Gen,3],'l', ylim=c(0,max(Totals.Mat[,3])), xlim=c(0,Gen), xlab="Year", 
	ylab="Total Whitebark Population", col='black', lwd=2)
   lines(1:Gen, Totals.Mat[(1*Gen+1):(2*Gen),3],'l', col='dark blue', lwd=2)
   lines(1:Gen, Totals.Mat[(2*Gen+1):(3*Gen),3],'l', col='dark green', lwd=2)
   lines(1:Gen, Totals.Mat[(3*Gen+1):(4*Gen),3],'l', col='yellow', lwd=2)
   lines(1:Gen, Totals.Mat[(4*Gen+1):(5*Gen),3],'l', col='purple', lwd=2)
   lines(1:Gen, Totals.Mat[(5*Gen+1):(6*Gen),3],'l', col='dark red', lwd=2)
#fix(Totals.Mat)
############################
############################
####### END PROGRAM ########
############################
############################

