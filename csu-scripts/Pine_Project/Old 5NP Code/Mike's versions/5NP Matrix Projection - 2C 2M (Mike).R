########################################################################
################ SIMPLE 2 CLASS GENETICS MODEL #########################
########################################################################
#require(popbio)
rm(list=ls(all=TRUE))
pro <- function(v,x) {v[x]/sum(v)} ### Proportion of a cell x within a sector v

###########################################################
######## Create initial stage classified genotypes ########
###########################################################

NRR=25; ARR=0
NRr=0; ARr=0
Nrr=0; Arr=0
NRRi=0; ARRi=0
NRri=0; ARri=0
Nrri=0; Arri=0
I.pop <- c(NRR,ARR,NRr,ARr,Nrr,Arr,NRRi,ARRi,NRri,ARri,Nrri,Arri); stages = length(I.pop)

###############################################################
### Set parameters for survival, transitions & fecundity ######
###############################################################
Gen = 10         #  How long will this run
s1 = 0.95; s2 = 0.05; s3 = 0.95
SLn = SLin = s1  # survive within newbie class
Sn = Sin = s2    # transition between classes	
SLa = SLia = s3  # survive within adult class
F = Fi = 1       # fecundity (of infected); add different for juveniles FA,FJ,FAi,FJi

###################################################
### Matrices to store population results ############
###################################################
popn.Mat <- matrix(0,Gen,stages)          # overall storage of population projection
popn.Mat[1,]=I.pop                        # first row is the initial population
pop.Mat.SI <- matrix(0,Gen-1,stages)      # multiplication of survival & infection together (SI)

#####################################################
######### Define infection rates  ###################
#####################################################
Beta.vec = seq(0,1,length=11)
Rep = length(Beta.vec)
Totals.Mat <- matrix(0,(Rep*Gen),(stages+3))      # storage matrix for total population for plotting, etc
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
Totals.Mat[count,1]= Beta
Totals.Mat[count,2]= 1
Totals.Mat[count,3]= sum(I.pop)
Totals.Mat[count,4:15]= I.pop

##############################################################################
### Create vectors to store parameters for population matrices   #############
##############################################################################
###########################################################
######## Survival & Infection (SI) together ###############
###########################################################
# Should eventually be within the Gen Loop ################
	par.vec.SI <- c(rep(0,(stages^2)))
	par.vec.SI[1]= SLn*(1-Beta)
	par.vec.SI[13]= Sn*(1-Beta)
	par.vec.SI[14]= SLa*(1-Beta)
	par.vec.SI[27]= SLn*(1-Beta)
	par.vec.SI[39]= Sn*(1-Beta)
	par.vec.SI[40]= SLa*(1-Beta)
	par.vec.SI[53]= SLn*(1-Beta)
	par.vec.SI[65]= Sn*(1-Beta)
	par.vec.SI[66]= SLa*(1-Beta)
	par.vec.SI[79]= SLin
	par.vec.SI[91]= Sin
	par.vec.SI[92]= SLia
	par.vec.SI[105]= SLin
	par.vec.SI[117]= Sin
	par.vec.SI[118]= SLia
	par.vec.SI[131]= SLin
	par.vec.SI[143]= Sin
	par.vec.SI[144]= SLia
	par.vec.SI[73]= SLn*Beta
	par.vec.SI[85]= Sn*Beta
	par.vec.SI[86]= SLa*Beta
	par.vec.SI[99]= SLn*Beta
	par.vec.SI[111]= Sn*Beta
	par.vec.SI[112]= SLa*Beta
	par.vec.SI[125]= SLn*Beta
	par.vec.SI[137]= Sn*Beta
	par.vec.SI[138]= SLa*Beta
  M.SI = matrix(par.vec.SI,stages,stages,byrow=T); M.SI

####################################################################################
########################### Now the Loop over generations! #########################
####################################################################################
for (n in 2:Gen) {

########################################################
#########  Survival & Infection (SI) of Classes  #######
########################################################
SI.pop.vec = M.SI%*%popn.Mat[n-1,]; SI.pop.vec
pop.Mat.SI[n-1,] = SI.pop.vec

#################################################################
#############  Mating & Reproduction  ###########################
#################################################################
#### Get frequencies of each genotype in mating population ######
#################################################################
Mate.pop <- SI.pop.vec[c(2,4,6,8,10,12)]
Sum.MP = sum(Mate.pop)

	p.ARR=pro(Mate.pop,1); 		p.ARr=pro(Mate.pop,2); 		p.Arr=pro(Mate.pop,3)
	p.ARRi=pro(Mate.pop,4); 	p.ARri=pro(Mate.pop,5); 	p.Arri=pro(Mate.pop,6)

#          1     2      3       4       5      6 
mpv = c(p.ARR, p.ARr, p.Arr, p.ARRi, p.ARri, p.Arri); mpv
P=mpv[1]; H=mpv[2]; Q=mpv[3]; Pi=mpv[4]; Hi=mpv[5]; Qi=mpv[6]

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

##########################################################################
####  Reproduction of newbies of each genotype & infection class  ########
##########################################################################

NRR = F*RR*Sum.MP
NRr = F*Rr*Sum.MP		
Nrr = F*rr*Sum.MP
NRRi = Fi*RRi*Sum.MP
NRri = Fi*Rri*Sum.MP
Nrri = Fi*rri*Sum.MP

###################################################
### Add newbies to population & make popn.Mat #####
###################################################

popn.Mat[n,1]= NRR + NRRi + SI.pop.vec[1]
popn.Mat[n,2]= SI.pop.vec[2]
popn.Mat[n,3]= NRr + NRri + SI.pop.vec[3]
popn.Mat[n,4]= SI.pop.vec[4]
popn.Mat[n,5]= Nrr + Nrri + SI.pop.vec[5]
popn.Mat[n,6:12]= SI.pop.vec[6:12]

##########################################################################
########### STORE RESULTS FOR PRINTING AND PLOTTING  #####################
##########################################################################

count=count+1
Totals.Mat[count,1]= Beta
Totals.Mat[count,2]= n
Totals.Mat[count,3]= sum(popn.Mat[n,])
Totals.Mat[count,4:15]= popn.Mat[n,]

   }  # END OF GENERATION LOOP
}  #  END OF BETA LOOP

Totals.Mat

############################
############################
####### END PROGRAM ########
############################
############################

