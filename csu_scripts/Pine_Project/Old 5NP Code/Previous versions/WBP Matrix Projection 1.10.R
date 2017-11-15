					
### All Clear ##########
require(popbio)
rm(list=ls(all=TRUE))
prop <- function(v,x) {v[x]/sum(v)}

######## Create initial population & genotype parameters #########
##################################################################
#initial.pop=c(rep(10,9),rep(0,9)) 
initial.pop=c(rep(100,3),rep(0,3),rep(0,3),rep(0,9))
#initial.pop=c(rep(1,3),rep(10,3),rep(100,3),rep(0,9))  ### Anna's starting conditions
#initial.pop=c(rep(0,3),1,0,0,rep(100,3),rep(0,9)) ### New Mutant with Resistance
stages = length(initial.pop)
Gen = 10
FA = 1; FJ = FA; FAi = 0; FJi = FAi
#FA = 0.1452581; FJ = FA*0.5; FAi = 0.017647; FJi = FAi*0.5
s1 = 0.967742; s2 = 0.012; s3 = 0.916667; s4 = 0.073; s5 = 0.99
#s1 = 0.50; s2 = 0.01; s3 = 0.85; s4 = 0.05; s5 = 0.90
############################################################
######### Infection Probabilities B & a ####################
############################################################
Beta = 0
a = 0
#Beta.nRR= 1; Beta.nRr= 1; Beta.nrr= 1; Beta.jRR= 1; Beta.jRr= 1; Beta.jrr= 1; Beta.aRR= 1; Beta.aRr= 1; Beta.arr= 1
#a.nRR= 1; a.nRr= 1; a.nrr= 1; a.jRR= 1; a.jRr= 1; a.jrr= 1; a.aRR= 1; a.aRr= 1; a.arr= 1
#########################################################################################
### Newborn Survivorship ###
NRR.L.surv=s1; NRr.L.surv=s1; Nrr.L.surv=s1; NRR.surv=s2; NRr.surv=s2; Nrr.surv=s2
NiRR.L.surv=s1; NiRr.L.surv=s1; Nirr.L.surv=s1; NiRR.surv=s2; NiRr.surv=s2; Nirr.surv=s2
###rnorm(50,0.40,0.10)                      0.85

### Juvenile Survivorship ###
JRR.L.surv=s3; JRr.L.surv=s3; Jrr.L.surv=s3; JRR.surv=s4; JRr.surv=s4; Jrr.surv=s4
JiRR.L.surv=s3; JiRr.L.surv=s3; Jirr.L.surv=s3; JiRR.surv=s4; JiRr.surv=s4; Jirr.surv=s4
###rnorm(50,0.66,0.20)                      0.90

### Adult Survivorship ###
ARR.L.surv=s5; ARr.L.surv=s5; Arr.L.surv=s5; AiRR.L.surv=s5; AiRr.L.surv=s5; Airr.L.surv=s5
###runif(50,0.90,0.99)                                                                   0.92
#random.vec=rnorm(50,1.0,0.2)

###################################################
### Matrix to store population results ############
popn.Mat = matrix(0,Gen,stages, byrow=T)
popn.Mat[1,] = initial.pop; #popn.Mat

### Define initial parameters ############ "L = loop" ##########################

SLnRR=NRR.L.surv		#(01)Survivorship of newborn RR NOT moving to next class
SLinRR=NiRR.L.surv	#(10)Survivorship of infected newborn RR NOT moving to next class
SnRR=NRR.surv		#(19)Survivorship of newborn RR moving to next class
SLjRR=JRR.L.surv		#(20)Survivorship of juvenile RR NOT moving to next class
SinRR=NiRR.surv		#(28)Survivorship of infected newborn RR moving to next class
SLijRR=JiRR.L.surv	#(29)Survivorship of infected juvenile RR NOT moving to next class
SjRR=JRR.surv		#(38)Survivorship of juvenile RR moving to next class
SLaRR=ARR.L.surv		#(39)Survivorship of adult RR
SijRR=JiRR.surv		#(47)Survivorship of infected juvenile RR moving to next class
SLiaRR=AiRR.L.surv	#(48)Survivorship of infected adult RR
SLnRr=NRr.L.surv		#(58)Survivorship of newborn Rr NOT moving to next class
SLinRr=NiRr.L.surv	#(67)Survivorship of infected newborn Rr NOT moving to next class
SnRr=NRr.surv		#(76)Survivorship of newborn Rr moving to next class
SLjRr=JRr.L.surv		#(77)Survivorship of juvenile Rr NOT moving to next class
SinRr=NiRr.surv		#(85)Survivorship of infected newborn Rr moving to next class
SLijRr=JiRr.L.surv	#(86)Survivorship of infected juvenile Rr NOT moving to next class
SjRr=JRr.surv		#(95)Survivorship of juvenile Rr moving to next class
SLaRr=ARr.L.surv		#(96)Survivorship of adult Rr
SijRr=JiRr.surv		#(104)Survivorship of infected juvenile Rr moving to next class
SLiaRr=AiRr.L.surv	#(105)Survivorship of infected adult Rr
SLnrr=Nrr.L.surv		#(115)Survivorship of newborn rr NOT moving to next class
SLinrr=Nirr.L.surv	#(124)Survivorship of infected newborn rr NOT moving to next class
Snrr=Nrr.surv		#(133)Survivorship of newborn rr moving to next class
SLjrr=Jrr.L.surv		#(134)Survivorship of juvenile rr NOT moving to next class
Sinrr=Nirr.surv		#(142)Survivorship of infected newborn rr moving to next class
SLijrr=Jirr.L.surv	#(143)Survivorship of infected juvenile rr NOT moving to next class
Sjrr=Jrr.surv		#(152)Survivorship of juvenile rr moving to next class
SLarr=Arr.L.surv		#(153)Survivorship of adult rr
Sijrr=Jirr.surv		#(161)Survivorship of infected juvenile rr moving to next class
SLiarr=Airr.L.surv	#(162)Survivorship of infected adult rr

############ Fecundities of Infected & Uninfected #######################################

Mate.pop=c(initial.pop[2:3],initial.pop[5:6],initial.pop[8:9],initial.pop[11:12],initial.pop[14:15],initial.pop[17:18])
p.jRR=prop(Mate.pop,1); p.jRr=prop(Mate.pop,3); p.jrr=prop(Mate.pop,5)
p.aRR=prop(Mate.pop,2); p.aRr=prop(Mate.pop,4); p.arr=prop(Mate.pop,6)
p.ijRR=prop(Mate.pop,7); p.ijRr=prop(Mate.pop,9); p.ijrr=prop(Mate.pop,11)
p.iaRR=prop(Mate.pop,8); p.iaRr=prop(Mate.pop,10); p.iarr=prop(Mate.pop,12)

#check=c(p.jRR,p.jRr,p.jrr,p.aRR,p.aRr,p.arr,p.ijRR,p.ijRr,p.ijrr,p.iaRR,p.iaRr,p.iarr)
#Prop.Mat=matrix(check,4,3,byrow=T); Prop.Mat; sum(check)
     
#(02)Probability that a RR juvenile produces a RR newborn
F02 = FJ*(p.jRR*((p.jRR*1)+(p.jRr*0.5)+(p.aRR*1)+(p.aRr*0.5)+
	(p.ijRR*1)+(p.ijRr*0.5)+(p.iaRR*1)+(p.iaRr*0.5)))
#(03)Probability that a RR adult produces a RR newborn
F03 = FA*(p.aRR*((p.aRR*1)+(p.aRr*0.5)+(p.jRR*1)+(p.jRr*0.5)+
	(p.iaRR*1)+(p.iaRr*0.5)+(p.ijRR*1)+(p.ijRr*0.5)))
#(05)Probability that a Rr juvenile produces a RR newborn
F05 = FJ*(p.jRr*((p.jRR*0.5)+(p.jRr*0.25)+(p.aRR*0.5)+(p.aRr*0.25)+
	(p.ijRR*0.5)+(p.ijRr*0.25)+(p.iaRR*0.5)+(p.iaRr*0.25)))
#(06)Probability that a Rr adult produces a RR newborn
F06 = FA*(p.aRr*((p.aRR*0.5)+(p.aRr*0.25)+(p.jRR*0.5)+(p.jRr*0.25)+
	(p.iaRR*0.5)+(p.iaRr*0.25)+(p.ijRR*0.5)+(p.ijRr*0.25)))
#(56)Probability that a RR juvenile produces a Rr newborn
F56 = FJ*(p.jRR*((p.jRr*0.5)+(p.jrr*1)+(p.aRr*0.5)+(p.arr*1)+
	(p.ijRr*0.5)+(p.ijrr*1)+(p.iaRr*0.5)+(p.iarr*1)))
#(57)Probability that a RR adult produces a Rr newborn
F57 = FA*(p.aRR*((p.aRr*0.5)+(p.arr*1)+(p.jRr*0.5)+(p.jrr*1)+
	(p.iaRr*0.5)+(p.iarr*1)+(p.ijRr*0.5)+(p.ijrr*1)))
#(59)Probability that a Rr juvenile produces a Rr newborn
F59 = FJ*(p.jRr*((p.jRr*0.5)+(p.jrr*0.5)+(p.jRR*0.5)+(p.aRr*0.5)+(p.aRR*0.5)+(p.arr*0.5)+
	(p.ijRr*0.5)+(p.ijrr*0.5)+(p.ijRR*0.5)+(p.iaRr*0.5)+(p.iaRR*0.5)+(p.iarr*0.5)))
#(60)Probability that a Rr adult produces a Rr newborn
F60 = FA*(p.aRr*((p.aRr*0.5)+(p.arr*0.5)+(p.aRR*0.5)+(p.jRr*0.5)+(p.jRR*0.5)+(p.jrr*0.5)+
	(p.iaRr*0.5)+(p.iarr*0.5)+(p.iaRR*0.5)+(p.ijRr*0.5)+(p.ijRR*0.5)+(p.ijrr*0.5)))
#(62)Probability that a rr juvenile produces a Rr newborn
F62 = FJ*(p.jrr*((p.jRR*1)+(p.jRr*0.5)+(p.aRR*1)+(p.aRr*0.5)+
	(p.ijRR*1)+(p.ijRr*0.5)+(p.iaRR*1)+(p.iaRr*0.5)))
#(63)Probability that a rr adult produces a Rr newborn
F63 = FA*(p.arr*((p.aRR*1)+(p.aRr*0.5)+(p.jRR*1)+(p.jRr*0.5)+
	(p.iaRR*1)+(p.iaRr*0.5)+(p.ijRR*1)+(p.ijRr*0.5)))
#(113)Probability that a Rr juvenile produces a rr newborn
F113 = FJ*(p.jRr*((p.jRr*0.25)+(p.jrr*0.5)+(p.aRr*0.25)+(p.arr*0.5)+
	(p.ijRr*0.25)+(p.ijrr*0.5)+(p.iaRr*0.25)+(p.iarr*0.5)))
#(114)Probability that a Rr adult produces a rr newborn
F114 = FA*(p.aRr*((p.aRr*0.25)+(p.arr*0.5)+(p.jRr*0.25)+(p.jrr*0.5)+
	(p.iaRr*0.25)+(p.iarr*0.5)+(p.ijRr*0.25)+(p.ijrr*0.5)))
#(116)Probability that a rr juvenile produces a rr newborn
F116 = FJ*(p.jrr*((p.jrr*1)+(p.jRr*0.5)+(p.arr*1)+(p.aRr*0.5)+
	(p.ijrr*1)+(p.ijRr*0.5)+(p.iarr*1)+(p.iaRr*0.5)))
#(117)Probability that a rr adult produces a rr newborn
F117 = FA*(p.arr*((p.arr*1)+(p.aRr*0.5)+(p.jrr*1)+(p.jRr*0.5)+
	(p.iarr*1)+(p.iaRr*0.5)+(p.ijrr*1)+(p.ijRr*0.5)))

################### INFECTED SUBPOP ##########################################

#(11)Probability that a RR infected juvenile produces a RR newborn
F11 = FJ*(p.ijRR*((p.jRR*1)+(p.jRr*0.5)+(p.aRR*1)+(p.aRr*0.5)+
	(p.ijRR*1)+(p.ijRr*0.5)+(p.iaRR*1)+(p.iaRr*0.5)))
#(12)Probability that a RR infected adult produces a RR newborn
F12 = FA*(p.iaRR*((p.aRR*1)+(p.aRr*0.5)+(p.jRR*1)+(p.jRr*0.5)+
	(p.iaRR*1)+(p.iaRr*0.5)+(p.ijRR*1)+(p.ijRr*0.5)))
#(14)Probability that a Rr infected juvenile produces a RR newborn
F14 = FJ*(p.ijRr*((p.jRR*0.5)+(p.jRr*0.25)+(p.aRR*0.5)+(p.aRr*0.25)+
	(p.ijRR*0.5)+(p.ijRr*0.25)+(p.iaRR*0.5)+(p.iaRr*0.25)))
#(15)Probability that a Rr infected adult produces a RR newborn
F15 = FA*(p.iaRr*((p.aRR*0.5)+(p.aRr*0.25)+(p.jRR*0.5)+(p.jRr*0.25)+
	(p.iaRR*0.5)+(p.iaRr*0.25)+(p.ijRR*0.5)+(p.ijRr*0.25)))
#(65)Probability that a RR infected juvenile produces a Rr newborn
F65 = FJ*(p.ijRR*((p.jRr*0.5)+(p.jrr*1)+(p.aRr*0.5)+(p.arr*1)+
	(p.ijRr*0.5)+(p.ijrr*1)+(p.iaRr*0.5)+(p.iarr*1)))
#(66)Probability that a RR infected adult produces a Rr newborn
F66 = FA*(p.iaRR*((p.aRr*0.5)+(p.arr*1)+(p.jRr*0.5)+(p.jrr*1)+
	(p.iaRr*0.5)+(p.iarr*1)+(p.ijRr*0.5)+(p.ijrr*1)))
#(68)Probability that a Rr infected juvenile produces a Rr newborn
F68 = FJ*(p.ijRr*((p.jRr*0.5)+(p.jrr*0.5)+(p.jRR*0.5)+(p.aRr*0.5)+(p.aRR*0.5)+(p.arr*0.5)+
	(p.ijRr*0.5)+(p.ijrr*0.5)+(p.ijRR*0.5)+(p.iaRr*0.5)+(p.iaRR*0.5)+(p.iarr*0.5)))
#(69)Probability that a Rr infected adult produces a Rr newborn
F69 = FA*(p.iaRr*((p.aRr*0.5)+(p.arr*0.5)+(p.aRR*0.5)+(p.jRr*0.5)+(p.jRR*0.5)+(p.jrr*0.5)+
	(p.iaRr*0.5)+(p.iarr*0.5)+(p.iaRR*0.5)+(p.ijRr*0.5)+(p.ijRR*0.5)+(p.ijrr*0.5)))
#(71)Probability that a rr infected juvenile produces a Rr newborn
F71 = FJi*(p.ijrr*((p.jRR*1)+(p.jRr*0.5)+(p.aRR*1)+(p.aRr*0.5)+
	(p.ijRR*1)+(p.ijRr*0.5)+(p.iaRR*1)+(p.iaRr*0.5)))
#(72)Probability that a rr infected adult produces a Rr newborn
F72 = FAi*(p.iarr*((p.aRR*1)+(p.aRr*0.5)+(p.jRR*1)+(p.jRr*0.5)+
	(p.iaRR*1)+(p.iaRr*0.5)+(p.ijRR*1)+(p.ijRr*0.5)))
#(122)Probability that a Rr infected juvenile produces a rr newborn
F122 = FJ*(p.ijRr*((p.jRr*0.25)+(p.jrr*0.5)+(p.aRr*0.25)+(p.arr*0.5)+
	(p.ijRr*0.25)+(p.ijrr*0.5)+(p.iaRr*0.25)+(p.iarr*0.5)))
#(123)Probability that a Rr infected adult produces a rr newborn
F123 = FA*(p.iaRr*((p.aRr*0.25)+(p.arr*0.5)+(p.jRr*0.25)+(p.jrr*0.5)+
	(p.iaRr*0.25)+(p.iarr*0.5)+(p.ijRr*0.25)+(p.ijrr*0.5)))
#(125)Probability that a rr infected juvenile produces a rr newborn
F125 = FJi*(p.ijrr*((p.jrr*1)+(p.jRr*0.5)+(p.arr*1)+(p.aRr*0.5)+
	(p.ijrr*1)+(p.ijRr*0.5)+(p.iarr*1)+(p.iaRr*0.5)))
#(126)Probability that a rr infected adult produces a rr newborn
F126 = FAi*(p.iarr*((p.arr*1)+(p.aRr*0.5)+(p.jrr*1)+(p.jRr*0.5)+
	(p.iarr*1)+(p.iaRr*0.5)+(p.ijrr*1)+(p.ijRr*0.5)))

F=c(F02,F03,F05,F06,F56,F57,F59,F60,F62,F63,F113,F114,F116,F117)
Fi=c(F11,F12,F14,F15,F65,F66,F68,F69,F71,F72,F122,F123,F125,F126)
sum(F); sum(Fi)

###############################################
####### Double Loop Begin #####################
###############################################
#Beta.vec = c(0,0.1,0.5,0.75,0.9)
#Rep = length(Beta.vec)
#Beta.Mat <- matrix(0, Rep, Gen)
#Lambda.Mat <- matrix(0, Rep, Gen)

#for (c in 1:Rep) {
#   Beta = Beta.vec[c]

################################################
###### Matrix to store parameters ##############
################################################

initial.pars = c(SLnRR*(1-Beta),F02,F03,0,F05,F06,0,0,0,  SLinRR*a,F11,F12,0,F14,F15,0,0,0,
	SnRR*(1-Beta),SLjRR*(1-Beta),0,0,0,0,0,0,0,   SinRR*a,SLijRR*a,0,0,0,0,0,0,0,
	0,SjRR*(1-Beta),SLaRR*(1-Beta),0,0,0,0,0,0,   0,SijRR*a,SLiaRR*a,0,0,0,0,0,0,
	0,F56,F57,SLnRr*(1-Beta),F59,F60,0,F62,F63,   0,F65,F66,SLinRr*a,F68,F69,0,F71,F72,
	0,0,0,SnRr*(1-Beta),SLjRr*(1-Beta),0,0,0,0,   0,0,0,SinRr*a,SLijRr*a,0,0,0,0,
	0,0,0,0,SjRr*(1-Beta),SLaRr*(1-Beta),0,0,0,   0,0,0,0,SijRr*a,SLiaRr*a,0,0,0,
	0,0,0,0,F113,F114,SLnrr*(1-Beta),F116,F117,   0,0,0,0,F122,F123,SLinrr*a,F125,F126,
	0,0,0,0,0,0,Snrr*(1-Beta),SLjrr*(1-Beta),0,   0,0,0,0,0,0,Sinrr*a,SLijrr*a,0,
	0,0,0,0,0,0,0,Sjrr*(1-Beta),SLarr*(1-Beta),   0,0,0,0,0,0,0,Sijrr*a,SLiarr*a,
      SLnRR*(Beta),0,0,0,0,0,0,0,0,            SLinRR*(1-a),0,0,0,0,0,0,0,0,
      SnRR*(Beta),SLjRR*(Beta),0,0,0,0,0,0,0,  SinRR*(1-a),SLijRR*(1-a),0,0,0,0,0,0,0,
      0,SjRR*(Beta),SLaRR*(Beta),0,0,0,0,0,0,  0,SijRR*(1-a),SLiaRR*(1-a),0,0,0,0,0,0,
      0,0,0,SLnRr*(Beta),0,0,0,0,0,            0,0,0,SLinRr*(1-a),0,0,0,0,0,
      0,0,0,SnRr*(Beta),SLjRr*(Beta),0,0,0,0,  0,0,0,SinRr*(1-a),SLijRr*(1-a),0,0,0,0,
      0,0,0,0,SjRr*(Beta),SLaRr*(Beta),0,0,0,    0,0,0,0,SijRr*(1-a),SLiaRr*(1-a),0,0,0,
      0,0,0,0,0,0,SLnrr*(Beta),0,0,            0,0,0,0,0,0,SLinrr*(1-a),0,0,
      0,0,0,0,0,0,Snrr*(Beta),SLjrr*(Beta),0,  0,0,0,0,0,0,Sinrr*(1-a),SLijrr*(1-a),0,
      0,0,0,0,0,0,0,Sjrr*(Beta),SLarr*(Beta),  0,0,0,0,0,0,0,Sijrr*(1-a),SLiarr*(1-a))

par.Mat=matrix(0,Gen,length(initial.pars), byrow=T)
par.Mat[1,]=initial.pars; #par.Mat
M1=matrix(initial.pars,stages,stages,byrow=T);#M1

##############################################################################
### Creat vectors to store values from population matrix #####################
##############################################################################
lambda.vec=c(rep(0,Gen))
freq.R=c(rep(0,Gen))
freq.r=c(rep(0,Gen))
freq.R[1]=(2*(sum(initial.pop[1:3])+sum(initial.pop[10:12]))+
          (sum(initial.pop[4:6])+sum(initial.pop[13:15])))/(sum(initial.pop)*2)
freq.r[1]=(2*(sum(initial.pop[7:9])+sum(initial.pop[16:18]))+
          (sum(initial.pop[4:6])+sum(initial.pop[13:15])))/(sum(initial.pop)*2)
total.pop=c(rep(0,Gen)); total.pop[1]=sum(initial.pop)
test.vec=c(rep(0,Gen)); test.vec.s=c(1:stages)
test.vec[1]=initial.pop[1]
SSD.Mat=matrix(0,stages,Gen)
RV.Mat=matrix(0,stages,Gen)
DPR.vec=c(rep(0,Gen))
RV.RR=c(rep(0,Gen)); RV.Rr=c(rep(0,Gen)); RV.rr=c(rep(0,Gen))

####################################################################################
########################### Now the Loop! ##########################################
####################################################################################
for (n in 2:Gen) {
	M = matrix(par.Mat[n-1,],stages,stages, byrow=T)
	popn.Mat[n,] = M%*%popn.Mat[n-1,]
	freq.R[n]=(2*(sum(popn.Mat[n,1:3])+sum(popn.Mat[n,10:12]))+
               (sum(popn.Mat[n,4:6])+sum(popn.Mat[n,13:15])))/(sum(popn.Mat[n,])*2)
	freq.r[n]=(2*(sum(popn.Mat[n,7:9])+sum(popn.Mat[n,16:18]))+
               (sum(popn.Mat[n,4:6])+sum(popn.Mat[n,13:15])))/(sum(popn.Mat[n,])*2)
	lambda.vec[n-1] = eigen.analysis(M)$lambda
	SSD.Mat[,n-1] = eigen.analysis(M)$stable.stage
	RV.Mat[,n-1] = eigen.analysis(M)$repro.value
	DPR.vec[n-1] = eigen.analysis(M)$damping.ratio
	total.pop[n]= sum(popn.Mat[n,])
	test.vec[n] = popn.Mat[n,sample(test.vec.s,1)]
	RV.RR[n] = sum(RV.Mat[1,n-1]+RV.Mat[2,n-1]+RV.Mat[3,n-1]+RV.Mat[10,n-1]+RV.Mat[11,n-1]+RV.Mat[12,n-1])
	RV.Rr[n] = sum(RV.Mat[4,n-1]+RV.Mat[5,n-1]+RV.Mat[6,n-1]+RV.Mat[13,n-1]+RV.Mat[14,n-1]+RV.Mat[15,n-1])
	RV.rr[n] = sum(RV.Mat[7,n-1]+RV.Mat[8,n-1]+RV.Mat[9,n-1]+RV.Mat[16,n-1]+RV.Mat[17,n-1]+RV.Mat[18,n-1])
############################################################################################################
##### Get proportions of MATING genotypes for probabilities ################################################
############################################################################################################
Mate.Pop <- c(popn.Mat[n,2:3],popn.Mat[n,5:6],popn.Mat[n,8:9],popn.Mat[n,11:12],popn.Mat[n,14:15],popn.Mat[n,17:18])
	p.jRR=prop(Mate.Pop,1); p.jRr=prop(Mate.Pop,3); p.jrr=prop(Mate.Pop,5)
	p.aRR=prop(Mate.Pop,2); p.aRr=prop(Mate.Pop,4); p.arr=prop(Mate.Pop,6)
	p.ijRR=prop(Mate.Pop,7); p.ijRr=prop(Mate.Pop,9); p.ijrr=prop(Mate.Pop,11)
	p.iaRR=prop(Mate.Pop,8); p.iaRr=prop(Mate.Pop,10); p.iarr=prop(Mate.Pop,12)
####################################################################################
	par.Mat[n,1] = SLnRR*(1-Beta)   ##Beta.nRR
	par.Mat[n,2] = FJ*(p.jRR*((p.jRR*1)+(p.jRr*0.5)+(p.aRR*1)+(p.aRr*0.5)+
				(p.ijRR*1)+(p.ijRr*0.5)+(p.iaRR*1)+(p.iaRr*0.5)))  #*(sample(random,1))
	par.Mat[n,3] = FA*(p.aRR*((p.aRR*1)+(p.aRr*0.5)+(p.jRR*1)+(p.jRr*0.5)+
				(p.iaRR*1)+(p.iaRr*0.5)+(p.ijRR*1)+(p.ijRr*0.5)))  #*(sample(random,1))
	par.Mat[n,5] = FJ*(p.jRr*((p.jRR*0.5)+(p.jRr*0.25)+(p.aRR*0.5)+(p.aRr*0.25)+
				(p.ijRR*0.5)+(p.ijRr*0.25)+(p.iaRR*0.5)+(p.iaRr*0.25)))  #*(sample(random,1))
	par.Mat[n,6] = FA*(p.aRr*((p.aRR*0.5)+(p.aRr*0.25)+(p.jRR*0.5)+(p.jRr*0.25)+
				(p.iaRR*0.5)+(p.iaRr*0.25)+(p.ijRR*0.5)+(p.ijRr*0.25)))  #*(sample(random,1))
	par.Mat[n,10] = SLinRR*a     ##a.nRR
	par.Mat[n,11] = FJ*(p.ijRR*((p.jRR*1)+(p.jRr*0.5)+(p.aRR*1)+(p.aRr*0.5)+
				(p.ijRR*1)+(p.ijRr*0.5)+(p.iaRR*1)+(p.iaRr*0.5)))  #*(sample(random,1))
	par.Mat[n,12] = FA*(p.iaRR*((p.aRR*1)+(p.aRr*0.5)+(p.jRR*1)+(p.jRr*0.5)+
				(p.iaRR*1)+(p.iaRr*0.5)+(p.ijRR*1)+(p.ijRr*0.5)))  #*(sample(random,1))
	par.Mat[n,14] = FJ*(p.ijRr*((p.jRR*0.5)+(p.jRr*0.25)+(p.aRR*0.5)+(p.aRr*0.25)+
				(p.ijRR*0.5)+(p.ijRr*0.25)+(p.iaRR*0.5)+(p.iaRr*0.25)))  #*(sample(random,1))
	par.Mat[n,15] = FA*(p.iaRr*((p.aRR*0.5)+(p.aRr*0.25)+(p.jRR*0.5)+(p.jRr*0.25)+
				(p.iaRR*0.5)+(p.iaRr*0.25)+(p.ijRR*0.5)+(p.ijRr*0.25)))  #*(sample(random,1))
	par.Mat[n,19] = SnRR*(1-Beta)    ##Beta.nRR
	par.Mat[n,20] = SLjRR*(1-Beta)   ##Beta.jRR
	par.Mat[n,28] = SinRR*a          ##a.nRR
	par.Mat[n,29] = SLijRR*a         ##a.jRR
	par.Mat[n,38] = SjRR*(1-Beta)    ##Beta.jRR
	par.Mat[n,39] = SLaRR*(1-Beta)   ##Beta.aRR
	par.Mat[n,47] = SijRR*a          ##a.jRR
	par.Mat[n,48] = SLiaRR*a         ##a.aRR
	par.Mat[n,56] = FJ*(p.jRR*((p.jRr*0.5)+(p.jrr*1)+(p.aRr*0.5)+(p.arr*1)+
				(p.ijRr*0.5)+(p.ijrr*1)+(p.iaRr*0.5)+(p.iarr*1)))  #*(sample(random,1))
	par.Mat[n,57] = FA*(p.aRR*((p.aRr*0.5)+(p.arr*1)+(p.jRr*0.5)+(p.jrr*1)+
				(p.iaRr*0.5)+(p.iarr*1)+(p.ijRr*0.5)+(p.ijrr*1)))  #*(sample(random,1))
	par.Mat[n,58] = SLnRr*(1-Beta)   ##Beta.nRr
	par.Mat[n,59] = FJ*(p.jRr*((p.jRr*0.5)+(p.jrr*0.5)+(p.jRR*0.5)+(p.aRr*0.5)+(p.aRR*0.5)+(p.arr*0.5)+
				(p.ijRr*0.5)+(p.ijrr*0.5)+(p.ijRR*0.5)+(p.iaRr*0.5)+(p.iaRR*0.5)+(p.iarr*0.5)))
                      #*(sample(random,1))
	par.Mat[n,60] = FA*(p.aRr*((p.aRr*0.5)+(p.arr*0.5)+(p.aRR*0.5)+(p.jRr*0.5)+(p.jRR*0.5)+(p.jrr*0.5)+
				(p.iaRr*0.5)+(p.iarr*0.5)+(p.iaRR*0.5)+(p.ijRr*0.5)+(p.ijRR*0.5)+(p.ijrr*0.5)))
                      #*(sample(random,1))
	par.Mat[n,62] = FJ*(p.jrr*((p.jRR*1)+(p.jRr*0.5)+(p.aRR*1)+(p.aRr*0.5)+
				(p.ijRR*1)+(p.ijRr*0.5)+(p.iaRR*1)+(p.iaRr*0.5)))  #*(sample(random,1))
	par.Mat[n,63] = FA*(p.arr*((p.aRR*1)+(p.aRr*0.5)+(p.jRR*1)+(p.jRr*0.5)+
				(p.iaRR*1)+(p.iaRr*0.5)+(p.ijRR*1)+(p.ijRr*0.5)))  #*(sample(random,1))
	par.Mat[n,65] = FJ*(p.ijRR*((p.jRr*0.5)+(p.jrr*1)+(p.aRr*0.5)+(p.arr*1)+
				(p.ijRr*0.5)+(p.ijrr*1)+(p.iaRr*0.5)+(p.iarr*1)))  #*(sample(random,1))
	par.Mat[n,66] = FA*(p.iaRR*((p.aRr*0.5)+(p.arr*1)+(p.jRr*0.5)+(p.jrr*1)+
				(p.iaRr*0.5)+(p.iarr*1)+(p.ijRr*0.5)+(p.ijrr*1)))  #*(sample(random,1))
	par.Mat[n,67] = SLinRr*a      ##a.nRr
	par.Mat[n,68] = FJ*(p.ijRr*((p.jRr*0.5)+(p.jrr*0.5)+(p.jRR*0.5)+(p.aRr*0.5)+(p.aRR*0.5)+(p.arr*0.5)+
				(p.ijRr*0.5)+(p.ijrr*0.5)+(p.ijRR*0.5)+(p.iaRr*0.5)+(p.iaRR*0.5)+(p.iarr*0.5)))
                      #*(sample(random,1))
	par.Mat[n,69] = FA*(p.iaRr*((p.aRr*0.5)+(p.arr*0.5)+(p.aRR*0.5)+(p.jRr*0.5)+(p.jRR*0.5)+(p.jrr*0.5)+
				(p.iaRr*0.5)+(p.iarr*0.5)+(p.iaRR*0.5)+(p.ijRr*0.5)+(p.ijRR*0.5)+(p.ijrr*0.5)))
                      #*(sample(random,1))
	par.Mat[n,71] = FJi*(p.ijrr*((p.jRR*1)+(p.jRr*0.5)+(p.aRR*1)+(p.aRr*0.5)+
				(p.ijRR*1)+(p.ijRr*0.5)+(p.iaRR*1)+(p.iaRr*0.5)))  #*(sample(random,1))
	par.Mat[n,72] = FAi*(p.iarr*((p.aRR*1)+(p.aRr*0.5)+(p.jRR*1)+(p.jRr*0.5)+
				(p.iaRR*1)+(p.iaRr*0.5)+(p.ijRR*1)+(p.ijRr*0.5)))  #*(sample(random,1))
	par.Mat[n,76] = SnRr*(1-Beta)    ##Beta.nRr
	par.Mat[n,77] = SLjRr*(1-Beta)   ##Beta.jRr
	par.Mat[n,85] = SinRr*a       ##a.nRr
	par.Mat[n,86] = SLijRr*a      ##a.jRr
	par.Mat[n,95] = SjRr*(1-Beta)    ##Beta.jRr
	par.Mat[n,96] = SLaRr*(1-Beta)   ##Beta.aRr
	par.Mat[n,104] = SijRr*a      ##a.jRr
	par.Mat[n,105] = SLiaRr*a     ##a.aRr
	par.Mat[n,113] = FJ*(p.jRr*((p.jRr*0.25)+(p.jrr*0.5)+(p.aRr*0.25)+(p.arr*0.5)+
				(p.ijRr*0.25)+(p.ijrr*0.5)+(p.iaRr*0.25)+(p.iarr*0.5)))  #*(sample(random,1))
	par.Mat[n,114] = FA*(p.aRr*((p.aRr*0.25)+(p.arr*0.5)+(p.jRr*0.25)+(p.jrr*0.5)+
				(p.iaRr*0.25)+(p.iarr*0.5)+(p.ijRr*0.25)+(p.ijrr*0.5)))  #*(sample(random,1))
	par.Mat[n,115] = SLnrr*(1-Beta)  ##Beta.nrr
	par.Mat[n,116] = FJ*(p.jrr*((p.jrr*1)+(p.jRr*0.5)+(p.arr*1)+(p.aRr*0.5)+
				(p.ijrr*1)+(p.ijRr*0.5)+(p.iarr*1)+(p.iaRr*0.5)))  #*(sample(random,1))
	par.Mat[n,117] = FA*(p.arr*((p.arr*1)+(p.aRr*0.5)+(p.jrr*1)+(p.jRr*0.5)+
				(p.iarr*1)+(p.iaRr*0.5)+(p.ijrr*1)+(p.ijRr*0.5)))  #*(sample(random,1))
	par.Mat[n,122] = FJ*(p.ijRr*((p.jRr*0.25)+(p.jrr*0.5)+(p.aRr*0.25)+(p.arr*0.5)+
				(p.ijRr*0.25)+(p.ijrr*0.5)+(p.iaRr*0.25)+(p.iarr*0.5)))  #*(sample(random,1))
	par.Mat[n,123] = FA*(p.iaRr*((p.aRr*0.25)+(p.arr*0.5)+(p.jRr*0.25)+(p.jrr*0.5)+
				(p.iaRr*0.25)+(p.iarr*0.5)+(p.ijRr*0.25)+(p.ijrr*0.5)))  #*(sample(random,1))
	par.Mat[n,124] = SLinrr*a     ##a.nrr
	par.Mat[n,125] = FJi*(p.ijrr*((p.jrr*1)+(p.jRr*0.5)+(p.arr*1)+(p.aRr*0.5)+
				(p.ijrr*1)+(p.ijRr*0.5)+(p.iarr*1)+(p.iaRr*0.5)))  #*(sample(random,1))
	par.Mat[n,126] = FAi*(p.iarr*((p.arr*1)+(p.aRr*0.5)+(p.jrr*1)+(p.jRr*0.5)+
				(p.iarr*1)+(p.iaRr*0.5)+(p.ijrr*1)+(p.ijRr*0.5)))  #*(sample(random,1))
	par.Mat[n,133] = Snrr*(1-Beta)   ##Beta.nrr
	par.Mat[n,134] = SLjrr*(1-Beta)  ##Beta.jrr
	par.Mat[n,142] = Sinrr*a      ##a.nrr
	par.Mat[n,143] = SLijrr*a     ##a.jrr
	par.Mat[n,152] = Sjrr*(1-Beta)   ##Beta.jrr
	par.Mat[n,153] = SLarr*(1-Beta)  ##Beta.arr
	par.Mat[n,161] = Sijrr*a     ##a.jrr
	par.Mat[n,162] = SLiarr*a     ##a.arr
########### Second half of sub-Matrix - No Fecundities; only Survivorship ####################
	par.Mat[n,163] = SLnRR*Beta      ##Beta.nRR
	par.Mat[n,172] = SLinRR*(1-a) ##a.nRR
	par.Mat[n,181] = SnRR*Beta       ##Beta.nRR
	par.Mat[n,182] = SLjRR*Beta      ##Beta.jRR
	par.Mat[n,190] = SinRR*(1-a)  ##a.nRR
	par.Mat[n,191] = SLijRR*(1-a) ##a.jRR
	par.Mat[n,200] = SjRR*Beta       ##Beta.jRR
	par.Mat[n,201] = SLaRR*Beta      ##Beta.aRR
	par.Mat[n,209] = SijRR*(1-a)  ##a.jRR
	par.Mat[n,210] = SLiaRR*(1-a) ##a.aRR
	par.Mat[n,220] = SLnRr*Beta      ##Beta.nRr
	par.Mat[n,229] = SLinRr*(1-a) ##a.nRr
	par.Mat[n,238] = SnRr*Beta       ##Beta.nRr
	par.Mat[n,239] = SLjRr*Beta      ##Beta.jRr
	par.Mat[n,247] = SinRr*(1-a)  ##a.nRr
	par.Mat[n,248] = SLijRr*(1-a) ##a.jRr
	par.Mat[n,257] = SjRr*Beta       ##Beta.jRr
	par.Mat[n,258] = SLaRr*Beta      ##Beta.aRr
	par.Mat[n,266] = SijRr*(1-a)  ##a.jRr
	par.Mat[n,267] = SLiaRr*(1-a) ##a.aRr
	par.Mat[n,277] = SLnrr*Beta      ##Beta.nrr
	par.Mat[n,286] = SLinrr*(1-a) ##a.nrr
	par.Mat[n,295] = Snrr*Beta       ##Beta.nrr
	par.Mat[n,296] = SLjrr*Beta      ##Beta.jrr
	par.Mat[n,304] = Sinrr*(1-a)  ##a.nrr
	par.Mat[n,305] = SLijrr*(1-a) ##a.jrr
	par.Mat[n,314] = Sjrr*Beta       ##Beta.jrr
	par.Mat[n,315] = SLarr*Beta      ##Beta.arr
	par.Mat[n,323] = Sijrr*(1-a) ##a.jrr
	par.Mat[n,324] = SLiarr*(1-a) ##a.arr

}
   ### Matrix out of loop for Lambda of last generation ###
   M = matrix(par.Mat[n,],stages,stages, byrow=T)  
   lambda.vec[n] = eigen.analysis(M)$lambda
   SSD.Mat[,n] = eigen.analysis(M)$stable.stage
   RV.Mat[,n] = eigen.analysis(M)$repro.value
   DPR.vec[n] = eigen.analysis(M)$damping.ratio

#Beta.Mat[c,] = total.pop
#Lambda.Mat[c,] = lambda.vec
#}
#####################
### END PROGRAM #####
#####################


#######################################
##### Outputs of Interest #############
#######################################
par.Mat;
popn.Mat;
test.vec;
lambda.vec;
freq.R; freq.r
MateProp=c(p.jRR,p.jRr,p.jrr,p.aRR,p.aRr,p.arr,p.ijRR,p.ijRr,p.ijrr,p.iaRR,p.iaRr,p.iarr); sum(MateProp)
#######################################################################
### Whitebark Pine Population Variables by Generation #################
#######################################################################
par(mfrow=c(2,2))
plot(1:Gen, total.pop,'l', ylim=c(0,max(total.pop)), xlim=c(0,Gen), xlab="Year", ylab="Total Whitebark Pine Population", col='dark blue', lwd=2)
plot(1:Gen, lambda.vec, 'l', xlab="Year", ylab="Matrix Lambda", col='dark green', lwd=2)
plot(1:Gen, DPR.vec, 'l', xlab="Year", ylab="Matrix Damping Ratio", col='dark red', lwd=2)
plot(1:Gen, freq.R,'l', xlab="Year", ylim=range(0:1.0), ylab="Allele Frequency in Population", col='dark red', lwd=2)
   lines(1:n, freq.r,'l', col='dark blue', lwd=2)
   legend(Gen-(Gen*0.35), 0.6, c("R","r"),cex=0.8, col=c("dark red","dark blue"),lwd=2,lty=1)

#######################################################################
############## Population & Lambda for Rep Betas ######################
#######################################################################
par(mfrow=c(1,2))
plot(1:Gen, Beta.Mat[1,],'l', ylim=c(0,max(Beta.Mat)), xlim=c(0,Gen), xlab="Year", ylab="Total Whitebark Population", col='dark red', lwd=2)
   lines(1:Gen, Beta.Mat[2,],'l', col='dark blue', lwd=2)
   lines(1:Gen, Beta.Mat[3,],'l', col='dark green', lwd=2)
   lines(1:Gen, Beta.Mat[4,],'l', col='yellow', lwd=2)
   lines(1:Gen, Beta.Mat[5,],'l', col='purple', lwd=2)
   legend(60, (max(Beta.Mat)*0.75), c("Beta = 0","Beta = 0.1","Beta = 0.15","Beta = 0.2","Beta = 0.25"), 
   cex=0.7, col=c("dark red","dark blue","dark green","yellow","purple"), lwd=2, lty=1)

plot(1:Gen, Lambda.Mat[1,], 'l', ylim=c(1.012,1.014), xlab="Year", ylab="Matrix Lambda", col='dark red', lwd=2)
   lines(1:Gen, Lambda.Mat[2,],'l', col='dark blue', lwd=2)
   lines(1:Gen, Lambda.Mat[3,],'l', col='dark green', lwd=2)
   lines(1:Gen, Lambda.Mat[4,],'l', col='yellow', lwd=2)
   lines(1:Gen, Lambda.Mat[5,],'l', col='purple', lwd=2)
   legend(250,1.014,c("Beta 0","Beta 0.1","Beta 0.15","Beta 0.2","Beta 0.25"),
   cex=0.7, col=c("dark red","dark blue","dark green","yellow","purple"), lwd=2, lty=1)

####################################################
### Plot some population graphs ####################
####################################################
##### Newborns = Red ###################
##### Juveniles = Blue #################
##### Adults = Green ###################
par(mfrow=c(2,3))
plot(1:n, popn.Mat[,1],'l', ylim=c(0,max(popn.Mat[,3])), xlim=c(0,Gen), xlab="Year",ylab="Whitebark Pine Population RR",col='dark red',lwd=2)
lines(1:n, popn.Mat[,2], 'l', col='dark blue',lwd=2)
lines(1:n, popn.Mat[,3], 'l', col='dark green',lwd=2)
plot(1:n, popn.Mat[,4],'l', ylim=c(0,max(popn.Mat[,6])), xlim=c(0,Gen), xlab="Year",ylab="Whitebark Pine Population Rr", col='dark red',lwd=2)
lines(1:n, popn.Mat[,5], 'l', col='dark blue',lwd=2)
lines(1:n, popn.Mat[,6], 'l', col='dark green',lwd=2)
plot(1:n, popn.Mat[,7],'l', ylim=c(0,max(popn.Mat[,9])), xlim=c(0,Gen), xlab="Year",ylab="Whitebark Pine Population rr", col='dark red',lwd=2)
lines(1:n, popn.Mat[,8], 'l', col='dark blue',lwd=2)
lines(1:n, popn.Mat[,9], 'l', col='dark green',lwd=2)
########## Infected Subpop ###########################################################################
plot(1:n, popn.Mat[,10],'l', ylim=c(0,max(popn.Mat[,12])), xlim=c(0,Gen), xlab="Year",ylab="Whitebark Pine Population Infected RR",col='dark red',lwd=2)
lines(1:n, popn.Mat[,11], 'l', col='dark blue',lwd=2)
lines(1:n, popn.Mat[,12], 'l', col='dark green',lwd=2)
plot(1:n, popn.Mat[,13],'l', ylim=c(0,max(popn.Mat[,15])), xlim=c(0,Gen), xlab="Year",ylab="Whitebark Pine Population Infected Rr", col='dark red',lwd=2)
lines(1:n, popn.Mat[,14], 'l', col='dark blue',lwd=2)
lines(1:n, popn.Mat[,15], 'l', col='dark green',lwd=2)
plot(1:n, popn.Mat[,16],'l', ylim=c(0,max(popn.Mat[,18])), xlim=c(0,Gen),xlab="Year",ylab="Whitebark Pine Population Infected rr", col='dark red',lwd=2)
lines(1:n, popn.Mat[,17], 'l', col='dark blue',lwd=2)
lines(1:n, popn.Mat[,18], 'l', col='dark green',lwd=2)
###################################################################
#### SSD #####
plot(1:Gen, SSD.Mat[1,], 'l', ylim=c(0,1))
lines(1:Gen, SSD.Mat[2,], 'l')
lines(1:Gen, SSD.Mat[3,], 'l')
lines(1:Gen, SSD.Mat[4,], 'l')
lines(1:Gen, SSD.Mat[5,], 'l')
lines(1:Gen, SSD.Mat[6,], 'l')
lines(1:Gen, SSD.Mat[7,], 'l')
lines(1:Gen, SSD.Mat[8,], 'l')
lines(1:Gen, SSD.Mat[9,], 'l')
lines(1:Gen, SSD.Mat[10,], 'l')
lines(1:Gen, SSD.Mat[11,], 'l')
lines(1:Gen, SSD.Mat[12,], 'l')
lines(1:Gen, SSD.Mat[13,], 'l')
lines(1:Gen, SSD.Mat[14,], 'l')
lines(1:Gen, SSD.Mat[15,], 'l')
lines(1:Gen, SSD.Mat[16,], 'l')
lines(1:Gen, SSD.Mat[17,], 'l')
lines(1:Gen, SSD.Mat[18,], 'l')

#### RV #####
plot(1:Gen, RV.Mat[1,], 'l', ylim=c(0,max(RV.Mat)))
lines(1:Gen, RV.Mat[2,], 'l')
lines(1:Gen, RV.Mat[3,], 'l')
lines(1:Gen, RV.Mat[4,], 'l')
lines(1:Gen, RV.Mat[5,], 'l')
lines(1:Gen, RV.Mat[6,], 'l')
lines(1:Gen, RV.Mat[7,], 'l')
lines(1:Gen, RV.Mat[8,], 'l')
lines(1:Gen, RV.Mat[9,], 'l')
lines(1:Gen, RV.Mat[10,], 'l')
lines(1:Gen, RV.Mat[11,], 'l')
lines(1:Gen, RV.Mat[12,], 'l')
lines(1:Gen, RV.Mat[13,], 'l')
lines(1:Gen, RV.Mat[14,], 'l')
lines(1:Gen, RV.Mat[15,], 'l')
lines(1:Gen, RV.Mat[16,], 'l')
lines(1:Gen, RV.Mat[17,], 'l')
lines(1:Gen, RV.Mat[18,], 'l')

plot(1:Gen, RV.RR, 'l', ylim=c(0,100), xlab="Year", ylab="Reproductive Value", col='dark red', lwd=2)
   lines(1:Gen, RV.Rr,'l', col='dark blue', lwd=2)
   lines(1:Gen, RV.rr,'l', col='dark green', lwd=2)
   legend(Gen*0.6,80, c("RV.RR","RV.Rr","RV.rr"), cex=0.8, col=c("dark red","dark blue","dark green"), lwd=2, lty=1)


