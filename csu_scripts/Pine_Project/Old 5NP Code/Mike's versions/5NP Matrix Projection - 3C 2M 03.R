########################################################################
###################### 3 CLASS GENETICS MODEL ##########################
############# With sequestering mechanism of Resistance ################
########## & supplemental management of resistant individuals ##########
########################################################################
# require(popbio)
rm(list=ls())
pro <- function(v,x) {v[x]/sum(v)} ### Proportion of a cell x within a sector v

###########################################################
######## Create initial stage classified genotypes ########
###########################################################

NRR=1;	JRR=1;	ARR=1
NRr=18;	JRr=18;	ARr=18
Nrr=81;	Jrr=81;	Arr=81
NRRi=0;	JRRi=0;	ARRi=0
NRri=0;	JRri=0;	ARri=0
Nrri=0;	Jrri=0;	Arri=0
I.pop <- c(NRR,JRR,ARR,NRr,JRr,ARr,Nrr,Jrr,Arr,NRRi,JRRi,ARRi,NRri,JRri,ARri,Nrri,Jrri,Arri); stages = length(I.pop)

###############################################################
### Set parameters for survival, transitions & fecundity ######
###############################################################
Gen = 500                        #  How long will this run
par.data <- read.table('5NP Vitals.csv', sep=',', header=T)
S.sel = par.data$Value[7]            # Fitness cost to survivorship
F.sel = par.data$Value[8]            # Fitness cost to fecundity  LITERATURE VALUE = 12.1%
s1 = par.data$Value[1]; s2 = par.data$Value[2]; s3 = par.data$Value[3]; s4 = par.data$Value[4]; s5 = par.data$Value[5]  # LITERATURE VALUES

SLnRR= s1     #(01) Survive within newbie class (RR)
SnRR= s2		  #(19) Transition between n->j classes (RR)
SLjRR= s3		  #(20) Survive within juvie class (RR)
SjRR= s4		  #(38) Transition between j->a classes (RR)
SLaRR= s5		  #(39) Survive within adult class (RR)
SLnRr= s1		  #(58) Survive within newbie class (Rr)
SnRr= s2		  #(76) Transition between n->j classes (Rr)
SLjRr= s3		  #(77) Survive within juvie class (Rr)
SjRr= s4		  #(95) Transition between j->a classes (Rr)
SLaRr= s5		  #(96) Survive within adult class (Rr)
SLnrr= s1		  #(115) Survive within newbie class (rr)
Snrr= s2		  #(133) Transition between n->j classes (rr)
SLjrr= s3		  #(134) Survive within juvie class (rr)
Sjrr= s4		  #(152) Transition between j->a classes (rr)
SLarr= s5		  #(153) Survive within adult class (rr)
SLinrr= s1*S.sel		#(286) Survive within infected newbie class (rr)
Sinrr= s2*S.sel		  #(304) Transition between infected n->j classes (rr)
SLijrr= s3*S.sel		#(305) Survive within infected juvie class (rr)
Sijrr= s4*S.sel	  	#(323) Transition between infected j->a classes (rr)
SLiarr= s5*S.sel		#(324) Survive within infected adult class (rr)

###################################################
### Matrices to store population results ##########
###################################################
popn.Mat <- matrix(0,Gen,stages)          # overall storage of population projection
popn.Mat[1,]= I.pop                       # first row is the initial population
popn.Mat.SI <- matrix(0,Gen-1,stages)     # multiplication of survival & infection together (SI)

#####################################################
######### Define infection rates  ###################
#####################################################
Beta.vec = c(0, seq(0.016, 0.140, length=5))  #    Beta = 0.25; n = 2
Rep = length(Beta.vec)
Totals.Mat <- matrix(0,(Rep*Gen),(stages+5))     # storage matrix for total population for plotting, etc
colnames(Totals.Mat) = c('Beta','Gen','p(R)','Prevalence','Total','NRR','JRR','ARR','NRr','JRr','ARr','Nrr','Jrr','Arr',
                         'NRRi','JRRi','ARRi','NRri','JRri','ARri','Nrri','Jrri','Arri')
count = 0      # index for storing data
#lambda.vec = c(rep(0,Rep))

###############################################
####### Double Loop Begin #####################
###############################################
for (c in 1:Rep) {                        # Looping over valaues of Beta
     Beta = Beta.vec[c]

####################################################################################
########### STORE INITIAL CONDITIONS FOR PRINTING AND PLOTTING #####################
####################################################################################
freq.R = c(rep(0,Gen))
freq.r = c(rep(0,Gen))
freq.R[1] =(2*(sum(I.pop[1:3])+sum(I.pop[10:12]))+(sum(I.pop[4:6])+sum(I.pop[13:15])))/(sum(I.pop)*2)
freq.r[1] =(2*(sum(I.pop[7:9])+sum(I.pop[16:18]))+(sum(I.pop[4:6])+sum(I.pop[13:15])))/(sum(I.pop)*2)
total.pop = c(rep(0,Gen)); total.pop[1] = sum(I.pop)
Para.Prev = c(rep(0,Gen)); Para.Prev[1] = sum(I.pop[10:18])/sum(I.pop)

count=count+1
Totals.Mat[count,1]= Beta
Totals.Mat[count,2]= 1
Totals.Mat[count,3]= freq.R[1]
Totals.Mat[count,4]= Para.Prev[1]
Totals.Mat[count,5]= sum(I.pop)
Totals.Mat[count,6:23]= I.pop

##############################################################################
##### Create vectors to store parameters for population matrices  ############
##############################################################################
###########################################################
######## Survival & Infection (SI) together ###############
###########################################################
	par.vec.SI <- c(rep(0,(stages^2)))
	par.vec.SI[1]= SLnRR
	par.vec.SI[19]= SnRR
	par.vec.SI[20]= SLjRR
	par.vec.SI[38]= SjRR
	par.vec.SI[39]= SLaRR
	par.vec.SI[58]= SLnRr
	par.vec.SI[76]= SnRr
	par.vec.SI[77]= SLjRr
	par.vec.SI[95]= SjRr
	par.vec.SI[96]= SLaRr
	par.vec.SI[115]= SLnrr*(1-Beta)
	par.vec.SI[133]= Snrr*(1-Beta)
	par.vec.SI[134]= SLjrr*(1-Beta)
	par.vec.SI[152]= Sjrr*(1-Beta)
	par.vec.SI[153]= SLarr*(1-Beta)
	par.vec.SI[277]= SLnrr*(Beta)
	par.vec.SI[295]= Snrr*(Beta)
	par.vec.SI[296]= SLjrr*(Beta)
	par.vec.SI[314]= Sjrr*(Beta)
	par.vec.SI[315]= SLarr*(Beta)
	par.vec.SI[286]= SLinrr
	par.vec.SI[304]= Sinrr
	par.vec.SI[305]= SLijrr
	par.vec.SI[323]= Sijrr         
	par.vec.SI[324]= SLiarr
  M.SI = matrix(par.vec.SI,stages,stages,byrow=T); M.SI

####################################################################################
########################### Now the Loop over generations! #########################
####################################################################################
x = 1
for (n in 2:Gen) {

########################################################
#########  Survival & Infection (SI) of Classes  #######
########################################################
	SI.popn.vec = M.SI%*%popn.Mat[n-1,]; SI.popn.vec
	popn.Mat.SI[n-1,]= SI.popn.vec

#################################################################
#############  Mating & Reproduction  ###########################
#################################################################
#### Get frequencies of each genotype in mating population ######
#################################################################
	#Mate.pop <- SI.popn.vec[c(3,6,9,12,15,18)]; Mate.pop # Adults only mating
	Mate.pop <- SI.popn.vec[c(2,3,5,6,8,9,11,12,14,15,17,18)]; Mate.pop
	Sum.Mpop = sum(Mate.pop)
		Pj=pro(Mate.pop,1); 		Hj=pro(Mate.pop,3); 		Qj=pro(Mate.pop,5)
		Pa=pro(Mate.pop,2); 		Ha=pro(Mate.pop,4); 		Qa=pro(Mate.pop,6)
		Pij=pro(Mate.pop,7);		Hij=pro(Mate.pop,9);		Qij=pro(Mate.pop,11)
		Pia=pro(Mate.pop,8);		Hia=pro(Mate.pop,10);		Qia=pro(Mate.pop,12)

	#Pa=pro(Mate.pop,1); 		Ha=pro(Mate.pop,2); 		Qa=pro(Mate.pop,3)
	#Pia=pro(Mate.pop,4);		Hia=pro(Mate.pop,5);		Qia=pro(Mate.pop,6); Pj=Hj=Qj=Pij=Hij=Qij=0

############################################################################################
######  Get expected numbers of each genotype in the next generation, originating ##########
######  from infected and uninfected parents of this generation  ###########################
######## i.e. Rr.ij = proportion of Rr coming from infected juveniles ######################
############################################################################################
RR.j = (Pj*Pj)+(0.5*Pj*Hj)+(0.5*Hj*Pj)+(0.25*Hj*Hj)+(Pj*Pij)+(0.5*Pj*Hij)+(0.5*Hj*Pij)+(0.25*Hj*Hij)+
       (Pj*Pa)+(0.5*Pj*Ha)+(0.5*Hj*Pa)+(0.25*Hj*Ha)+(Pj*Pia)+(0.5*Pj*Hia)+(0.5*Hj*Pia)+(0.25*Hj*Hia)
Rr.j = (0.5*Pj*Hj)   +(Pj*Qj)  +(0.5*Hj*Pj)  +(0.5*Hj*Hj)  +(0.5*Hj*Qj)  +(Qj*Pj)  +(0.5*Qj*Hj)+
       (0.5*Pj*Ha)   +(Pj*Qa)  +(0.5*Hj*Pa)  +(0.5*Hj*Ha)  +(0.5*Hj*Qa)  +(Qj*Pa)  +(0.5*Qj*Ha)+
       (0.5*Pj*Hij)  +(Pj*Qij) +(0.5*Hj*Pij) +(0.5*Hj*Hij) +(0.5*Hj*Qij) +(Qj*Pij) +(0.5*Qj*Hij)+
       (0.5*Pj*Hia)  +(Pj*Qia) +(0.5*Hj*Pia) +(0.5*Hj*Hia) +(0.5*Hj*Qia) +(Qj*Pia) +(0.5*Qj*Hia)
rr.j = (0.25*Hj*Hj)+(0.5*Hj*Qj)+(0.5*Qj*Hj)+(Qj*Qj)+(0.25*Hj*Hij)+(0.5*Hj*Qij)+(0.5*Qj*Hij)+(Qj*Qij)+
       (0.25*Hj*Ha)+(0.5*Hj*Qa)+(0.5*Qj*Ha)+(Qj*Qa)+(0.25*Hj*Hia)+(0.5*Hj*Qia)+(0.5*Qj*Hia)+(Qj*Qia)
RR.j; Rr.j; rr.j; sum(RR.j,Rr.j,rr.j)

RR.ij = (Pij*Pj)+(0.5*Pij*Hj)+(0.5*Hij*Pj)+(0.25*Hij*Hj)+(Pij*Pij)+(0.5*Pij*Hij)+(0.5*Hij*Pij)+(0.25*Hij*Hij)+
        (Pij*Pa)+(0.5*Pij*Ha)+(0.5*Hij*Pa)+(0.25*Hij*Ha)+(Pij*Pia)+(0.5*Pij*Hia)+(0.5*Hij*Pia)+(0.25*Hij*Hia)
Rr.ij = (0.5*Pij*Hj)   +(Pij*Qj)  +(0.5*Hij*Pj)  +(0.5*Hij*Hj)  +(0.5*Hij*Qj)  +(Qij*Pj)  +(0.5*Qij*Hj)+
        (0.5*Pij*Ha)   +(Pij*Qa)  +(0.5*Hij*Pa)  +(0.5*Hij*Ha)  +(0.5*Hij*Qa)  +(Qij*Pa)  +(0.5*Qij*Ha)+
        (0.5*Pij*Hij)  +(Pij*Qij) +(0.5*Hij*Pij) +(0.5*Hij*Hij) +(0.5*Hij*Qij) +(Qij*Pij) +(0.5*Qij*Hij)+
        (0.5*Pij*Hia)  +(Pij*Qia) +(0.5*Hij*Pia) +(0.5*Hij*Hia) +(0.5*Hij*Qia) +(Qij*Pia) +(0.5*Qij*Hia)
rr.ij = (0.25*Hij*Hj)+(0.5*Hij*Qj)+(0.5*Qij*Hj)+(Qij*Qj)+(0.25*Hij*Hij)+(0.5*Hij*Qij)+(0.5*Qij*Hij)+(Qij*Qij)+
        (0.25*Hij*Ha)+(0.5*Hij*Qa)+(0.5*Qij*Ha)+(Qij*Qa)+(0.25*Hij*Hia)+(0.5*Hij*Qia)+(0.5*Qij*Hia)+(Qij*Qia)
RR.ij; Rr.ij; rr.ij; sum(RR.ij,Rr.ij,rr.ij)
########### ADULT HALF OF COMBINATIONS #####################
RR.a = (Pa*Pa)+(0.5*Pa*Ha)+(0.5*Ha*Pa)+(0.25*Ha*Ha)+(Pa*Pia)+(0.5*Pa*Hia)+(0.5*Ha*Pia)+(0.25*Ha*Hia)+
       (Pa*Pj)+(0.5*Pa*Hj)+(0.5*Ha*Pj)+(0.25*Ha*Hj)+(Pa*Pij)+(0.5*Pa*Hij)+(0.5*Ha*Pij)+(0.25*Ha*Hij)
Rr.a = (0.5*Pa*Ha)   +(Pa*Qa)  +(0.5*Ha*Pa)  +(0.5*Ha*Ha)  +(0.5*Ha*Qa)  +(Qa*Pa)  +(0.5*Qa*Ha)+
       (0.5*Pa*Hj)   +(Pa*Qj)  +(0.5*Ha*Pj)  +(0.5*Ha*Hj)  +(0.5*Ha*Qj)  +(Qa*Pj)  +(0.5*Qa*Hj)+
       (0.5*Pa*Hia)  +(Pa*Qia) +(0.5*Ha*Pia) +(0.5*Ha*Hia) +(0.5*Ha*Qia) +(Qa*Pia) +(0.5*Qa*Hia)+
       (0.5*Pa*Hij)  +(Pa*Qij) +(0.5*Ha*Pij) +(0.5*Ha*Hij) +(0.5*Ha*Qij) +(Qa*Pij) +(0.5*Qa*Hij)
rr.a = (0.25*Ha*Ha)+(0.5*Ha*Qa)+(0.5*Qa*Ha)+(Qa*Qa)+(0.25*Ha*Hia)+(0.5*Ha*Qia)+(0.5*Qa*Hia)+(Qa*Qia)+
       (0.25*Ha*Hj)+(0.5*Ha*Qj)+(0.5*Qa*Hj)+(Qa*Qj)+(0.25*Ha*Hij)+(0.5*Ha*Qij)+(0.5*Qa*Hij)+(Qa*Qij)
RR.a; Rr.a; rr.a; sum(RR.a,Rr.a,rr.a)

RR.ia = (Pia*Pa)+(0.5*Pia*Ha)+(0.5*Hia*Pa)+(0.25*Hia*Ha)+(Pia*Pia)+(0.5*Pia*Hia)+(0.5*Hia*Pia)+(0.25*Hia*Hia)+
        (Pia*Pj)+(0.5*Pia*Hj)+(0.5*Hia*Pj)+(0.25*Hia*Hj)+(Pia*Pij)+(0.5*Pia*Hij)+(0.5*Hia*Pij)+(0.25*Hia*Hij)
Rr.ia = (0.5*Pia*Ha)   +(Pia*Qa)  +(0.5*Hia*Pa)  +(0.5*Hia*Ha)  +(0.5*Hia*Qa)  +(Qia*Pa)  +(0.5*Qia*Ha)+
        (0.5*Pia*Hj)   +(Pia*Qj)  +(0.5*Hia*Pj)  +(0.5*Hia*Hj)  +(0.5*Hia*Qj)  +(Qia*Pj)  +(0.5*Qia*Hj)+
        (0.5*Pia*Hia)  +(Pia*Qia) +(0.5*Hia*Pia) +(0.5*Hia*Hia) +(0.5*Hia*Qia) +(Qia*Pia) +(0.5*Qia*Hia)+
        (0.5*Pia*Hij)  +(Pia*Qij) +(0.5*Hia*Pij) +(0.5*Hia*Hij) +(0.5*Hia*Qij) +(Qia*Pij) +(0.5*Qia*Hij)
rr.ia = (0.25*Hia*Ha)+(0.5*Hia*Qa)+(0.5*Qia*Ha)+(Qia*Qa)+(0.25*Hia*Hia)+(0.5*Hia*Qia)+(0.5*Qia*Hia)+(Qia*Qia)+
        (0.25*Hia*Hj)+(0.5*Hia*Qj)+(0.5*Qia*Hj)+(Qia*Qj)+(0.25*Hia*Hij)+(0.5*Hia*Qij)+(0.5*Qia*Hij)+(Qia*Qij)
RR.ia; Rr.ia; rr.ia; sum(RR.ia,Rr.ia,rr.ia)

##########################################################################
####  Reproduction of newbies of each genotype & infection class  ########
####  DON'T FORGET TO CHANGE FECUNDITY COSTS PER GENOTYPE HERE!!! ########
##########################################################################
  FA = par.data$Value[6]; FJ = FA*0.5; FAi = FA*F.sel; FJi = FAi*0.5 
	NRRj = FJ*RR.j*Sum.Mpop;		NRRij = FJi*RR.ij*Sum.Mpop
	NRrj = FJ*Rr.j*Sum.Mpop;		NRrij = FJi*Rr.ij*Sum.Mpop
	Nrrj = FJ*rr.j*Sum.Mpop;		Nrrij = FJi*rr.ij*Sum.Mpop

	NRRa = FA*RR.a*Sum.Mpop;		NRRia = FAi*RR.ia*Sum.Mpop
	NRra = FA*Rr.a*Sum.Mpop;		NRria = FAi*Rr.ia*Sum.Mpop
	Nrra = FA*rr.a*Sum.Mpop;		Nrria = FAi*rr.ia*Sum.Mpop

###################################################
### Add newbies to population & make popn.Mat #####
###################################################
	popn.Mat[n,1]= NRRj + NRRij + NRRa + NRRia + SI.popn.vec[1]
	if (n == 5*x) popn.Mat[n,1]= popn.Mat[n,1] + 1
	popn.Mat[n,2:3]= SI.popn.vec[2:3]
	popn.Mat[n,4]= NRrj + NRrij + NRra + NRria + SI.popn.vec[4]
	if (n == 5*x) popn.Mat[n,4]= popn.Mat[n,4] + 5
  if (n == 5*x) x = x + 1
	popn.Mat[n,5:6]= SI.popn.vec[5:6]
	popn.Mat[n,7]= Nrrj + Nrrij + Nrra + Nrria + SI.popn.vec[7]
	popn.Mat[n,8:18]= SI.popn.vec[8:18]

###################################################
########### OUTPUTS OF INTEREST ###################
###################################################
	freq.R[n] = (2*(sum(popn.Mat[n,1:3])+sum(popn.Mat[n,10:12]))+
			(sum(popn.Mat[n,4:6])+sum(popn.Mat[n,13:15])))/(sum(popn.Mat[n,])*2)
	freq.r[n] = (2*(sum(popn.Mat[n,7:9])+sum(popn.Mat[n,16:18]))+
			(sum(popn.Mat[n,4:6])+sum(popn.Mat[n,13:15])))/(sum(popn.Mat[n,])*2)
	Para.Prev[n] = sum(popn.Mat[n,10:18])/sum(popn.Mat[n,])
	total.pop[n]= sum(popn.Mat[n,])
	
##########################################################################
########### STORE RESULTS FOR PRINTING AND PLOTTING  #####################
##########################################################################

	count=count+1
	Totals.Mat[count,1]= Beta
	Totals.Mat[count,2]= n
	Totals.Mat[count,3]= freq.R[n]
	Totals.Mat[count,4]= Para.Prev[n]
	Totals.Mat[count,5]= sum(popn.Mat[n,])
	Totals.Mat[count,6:23]= popn.Mat[n,]

   }  # END OF GENERATION LOOP
}  #  END OF BETA LOOP

Totals.Mat = round(Totals.Mat,3); Totals.Mat

par(mfrow=c(1,3))
plot(1:Gen, Totals.Mat[1:Gen,5],'l', lty=2, ylim=c(0,max(Totals.Mat[,5])), xlim=c(0,Gen), xlab="Years", main="Total Population",
	ylab="Total Whitebark Population", col='black', lwd=1.5)
   lines(1:Gen, Totals.Mat[(1*Gen+1):(2*Gen),5],'l', col='slateblue4', lwd=1.5)
   lines(1:Gen, Totals.Mat[(2*Gen+1):(3*Gen),5],'l', col='dark green', lwd=1.5)
   lines(1:Gen, Totals.Mat[(3*Gen+1):(4*Gen),5],'l', col='gold', lwd=1.5)
   lines(1:Gen, Totals.Mat[(4*Gen+1):(5*Gen),5],'l', col='darkorchid4', lwd=1.5)
   lines(1:Gen, Totals.Mat[(5*Gen+1):(6*Gen),5],'l', col='red4', lwd=1.5)

plot(1:Gen, Totals.Mat[1:Gen,3],'l', lty=2, ylim=range(0:1.0), ylab="Allele Frequency (R) in Population", main="Frequency R",
	xlim=c(0,Gen), xlab="Years", col='black', lwd=1.5)
   lines(1:Gen, Totals.Mat[(1*Gen+1):(2*Gen),3],'l', col='slateblue4', lwd=1.5)
   lines(1:Gen, Totals.Mat[(2*Gen+1):(3*Gen),3],'l', col='dark green', lwd=1.5)
   lines(1:Gen, Totals.Mat[(3*Gen+1):(4*Gen),3],'l', col='gold', lwd=1.5)
   lines(1:Gen, Totals.Mat[(4*Gen+1):(5*Gen),3],'l', col='darkorchid4', lwd=1.5)
   lines(1:Gen, Totals.Mat[(5*Gen+1):(6*Gen),3],'l', col='red4', lwd=1.5)
   legend('topleft', cex=0.75, legend=c('Beta= 0.0','Beta= 0.016','Beta= 0.047','Beta= 0.078','Beta= 0.109','Beta= 0.140'),
   lty=c(2,rep(1,5)), bg='gray95', col=c('black','slateblue4','dark green','gold','darkorchid4','red4'), lwd=1.5)

plot(1:Gen, Totals.Mat[1:Gen,4],'l', lty=2, ylim=range(0:1.0), ylab="Parasite Prevalence", main="Prevalence",
	xlim=c(0,Gen), xlab="Years", col='black', lwd=1.5)
   lines(1:Gen, Totals.Mat[(1*Gen+1):(2*Gen),4],'l', col='slateblue4', lwd=1.5)
   lines(1:Gen, Totals.Mat[(2*Gen+1):(3*Gen),4],'l', col='dark green', lwd=1.5)
   lines(1:Gen, Totals.Mat[(3*Gen+1):(4*Gen),4],'l', col='gold', lwd=1.5)
   lines(1:Gen, Totals.Mat[(4*Gen+1):(5*Gen),4],'l', col='darkorchid4', lwd=1.5)
   lines(1:Gen, Totals.Mat[(5*Gen+1):(6*Gen),4],'l', col='red4', lwd=1.5)

########################################################
########################################################
################### END PROGRAM ########################
########################################################
########################################################
#fix(Totals.Mat)
#write.table(Totals.Mat, file="Totals.csv", sep=",", row.names=F)