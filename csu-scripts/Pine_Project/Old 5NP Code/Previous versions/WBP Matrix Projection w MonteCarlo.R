					Monte-Carlo Run
### All Clear ##########
require(popbio)
rm(list=ls(all=TRUE))
prop <- function(v,x) {v[x]/sum(v)}

#####################################
#### Place to keep Means of Runs ####
#####################################
Runs = 500; Gen = 500
Total.pop.Mat = matrix(0, Runs, Gen)

#####################################
### Loops for Monte-Carlo ###########
#####################################

for (mc in 1:Runs) {

##################################################################
######## Create initial population & genotype parameters #########
##################################################################
#initial.pop=c(rep(10,18))
#initial.pop=c(rep(10,9),rep(0,9)) 
#initial.pop=c(rep(10,3),rep(0,3),rep(0,3),rep(0,9))
initial.pop=c(rep(1,3),rep(10,3),rep(100,3),rep(0,9))  ### Anna's starting conditions
#initial.pop=c(rep(0,3),1,0,0,rep(100,3),rep(0,9)) ### New Mutant with Resistance
stages=length(initial.pop)
FA=0.1452581; FJ=FA*0.5; FAi=0.017647; FJi=FAi*0.25
s1=rnorm(25,0.967742,0.10); s2=rnorm(25,0.012,0.10)
s3=rnorm(25,0.916667,0.10); s4=rnorm(25,0.073,0.10); s5=rnorm(50,0.99,0.1)###runif(50,0.90,0.99)
############################################################
######### Infection Probabilities B & a ####################
############################################################
B=0.1; a=0
#B.nRR= 1; B.nRr= 1; B.nrr= 1; B.jRR= 1; B.jRr= 1; B.jrr= 1; B.aRR= 1; B.aRr= 1; B.arr= 1
#a.nRR= 1; a.nRr= 1; a.nrr= 1; a.jRR= 1; a.jRr= 1; a.jrr= 1; a.aRR= 1; a.aRr= 1; a.arr= 1
#########################################################################################
### Newborn Survivorship ###
NRR.L.surv=s1; NRR.surv=s2; NRr.L.surv=s1
NRr.surv=s2; Nrr.L.surv=s1; Nrr.surv=s2

NiRR.L.surv=s1; NiRR.surv=s2; NiRr.L.surv=s1
NiRr.surv=s2; Nirr.L.surv=rnorm(50,0.85,0.10); Nirr.surv=s2

### Juvenile Survivorship ###
JRR.L.surv=s3; JRR.surv=s4; JRr.L.surv=s3
JRr.surv=s4; Jrr.L.surv=s3; Jrr.surv=s4

JiRR.L.surv=s3; JiRR.surv=s4; JiRr.L.surv=s3
JiRr.surv=s4; Jirr.L.surv=rnorm(50,0.90,0.10); Jirr.surv=s4

### Adult Survivorship ###
ARR.L.surv=s5; ARr.L.surv=s5; Arr.L.surv=s5
AiRR.L.surv=s5; AiRr.L.surv=s5; Airr.L.surv=rnorm(50,0.92,0.10)

### Matrix to store population results ############
popn.matrix=matrix(0,Gen,stages, byrow=T)
popn.matrix[1,]=initial.pop; #popn.matrix

### Define some initial parameters ############ "L = loop" ##########################

SLnRR=sample(NRR.L.surv,1)      #(01)Survivorship of newborn RR NOT moving to next class
SLinRR=sample(NiRR.L.surv,1)    #(10)Survivorship of infected newborn RR NOT moving to next class
SnRR=sample(NRR.surv,1)         #(19)Survivorship of newborn RR moving to next class
SLjRR=sample(JRR.L.surv,1)      #(20)Survivorship of juvenile RR NOT moving to next class
SinRR=sample(NiRR.surv,1)       #(28)Survivorship of infected newborn RR moving to next class
SLijRR=sample(JiRR.L.surv,1)    #(29)Survivorship of infected juvenile RR NOT moving to next class
SjRR=sample(JRR.surv,1)         #(38)Survivorship of juvenile RR moving to next class
SLaRR=sample(ARR.L.surv,1)      #(39)Survivorship of adult RR
SijRR=sample(JiRR.surv,1)       #(47)Survivorship of infected juvenile RR moving to next class
SLiaRR=sample(AiRR.L.surv,1)    #(48)Survivorship of infected adult RR
SLnRr=sample(NRr.L.surv,1)      #(58)Survivorship of newborn Rr NOT moving to next class
SLinRr=sample(NiRr.L.surv,1)    #(67)Survivorship of infected newborn Rr NOT moving to next class
SnRr=sample(NRr.surv,1)         #(76)Survivorship of newborn Rr moving to next class
SLjRr=sample(JRr.L.surv,1)      #(77)Survivorship of juvenile Rr NOT moving to next class
SinRr=sample(NiRr.surv,1)       #(85)Survivorship of infected newborn Rr moving to next class
SLijRr=sample(JiRr.L.surv,1)    #(86)Survivorship of infected juvenile Rr NOT moving to next class
SjRr=sample(JRr.surv,1)         #(95)Survivorship of juvenile Rr moving to next class
SLaRr=sample(ARr.L.surv,1)      #(96)Survivorship of adult Rr
SijRr=sample(JiRr.surv,1)       #(104)Survivorship of infected juvenile Rr moving to next class
SLiaRr=sample(AiRr.L.surv,1)    #(105)Survivorship of infected adult Rr
SLnrr=sample(Nrr.L.surv,1)      #(115)Survivorship of newborn rr NOT moving to next class
SLinrr=sample(Nirr.L.surv,1)    #(124)Survivorship of infected newborn rr NOT moving to next class
Snrr=sample(Nrr.surv,1)         #(133)Survivorship of newborn rr moving to next class
SLjrr=sample(Jrr.L.surv,1)      #(134)Survivorship of juvenile rr NOT moving to next class
Sinrr=sample(Nirr.surv,1)       #(142)Survivorship of infected newborn rr moving to next class
SLijrr=sample(Jirr.L.surv,1)    #(143)Survivorship of infected juvenile rr NOT moving to next class
Sjrr=sample(Jrr.surv,1)         #(152)Survivorship of juvenile rr moving to next class
SLarr=sample(Arr.L.surv,1)      #(153)Survivorship of adult rr
Sijrr=sample(Jirr.surv,1)       #(161)Survivorship of infected juvenile rr moving to next class
SLiarr=sample(Airr.L.surv,1)    #(162)Survivorship of infected adult rr

############ Fecundities of Infected & Uninfected #######################################

Mate.pop=c(initial.pop[2:3],initial.pop[5:6],initial.pop[8:9],
     initial.pop[11:12],initial.pop[14:15],initial.pop[17:18])
p.jRR=prop(Mate.pop,1); p.jRr=prop(Mate.pop,3); p.jrr=prop(Mate.pop,5)
p.aRR=prop(Mate.pop,2); p.aRr=prop(Mate.pop,4); p.arr=prop(Mate.pop,6)
p.ijRR=prop(Mate.pop,7); p.ijRr=prop(Mate.pop,9); p.ijrr=prop(Mate.pop,11)
p.iaRR=prop(Mate.pop,8); p.iaRr=prop(Mate.pop,10); p.iarr=prop(Mate.pop,12)

#check=c(p.jRR,p.jRr,p.jrr,p.aRR,p.aRr,p.arr,p.ijRR,p.ijRr,p.ijrr,p.iaRR,p.iaRr,p.iarr)
#Prop.Matrix=matrix(check,4,3,byrow=T); Prop.Matrix; sum(check)
     
#(02)Probability that a RR juvenile produces a RR newborn
FjRR1= FJ*(p.jRR*((p.jRR*1)+(p.jRr*0.5)+(p.aRR*1)+(p.aRr*0.5)+
	(p.ijRR*1)+(p.ijRr*0.5)+(p.iaRR*1)+(p.iaRr*0.5)))
#(03)Probability that a RR adult produces a RR newborn
FaRR1= FA*(p.aRR*((p.aRR*1)+(p.aRr*0.5)+(p.jRR*1)+(p.jRr*0.5)+
	(p.iaRR*1)+(p.iaRr*0.5)+(p.ijRR*1)+(p.ijRr*0.5)))
#(05)Probability that a Rr juvenile produces a RR newborn
FjRr1= FJ*(p.jRr*((p.jRR*0.5)+(p.jRr*0.25)+(p.aRR*0.5)+(p.aRr*0.25)+
	(p.ijRR*0.5)+(p.ijRr*0.25)+(p.iaRR*0.5)+(p.iaRr*0.25)))
#(06)Probability that a Rr adult produces a RR newborn
FaRr1= FA*(p.aRr*((p.aRR*0.5)+(p.aRr*0.25)+(p.jRR*0.5)+(p.jRr*0.25)+
	(p.iaRR*0.5)+(p.iaRr*0.25)+(p.ijRR*0.5)+(p.ijRr*0.25)))
#(56)Probability that a RR juvenile produces a Rr newborn
FjRR2= FJ*(p.jRR*((p.jRr*0.5)+(p.jrr*1)+(p.aRr*0.5)+(p.arr*1)+
	(p.ijRr*0.5)+(p.ijrr*1)+(p.iaRr*0.5)+(p.iarr*1)))
#(57)Probability that a RR adult produces a Rr newborn
FaRR2= FA*(p.aRR*((p.aRr*0.5)+(p.arr*1)+(p.jRr*0.5)+(p.jrr*1)+
	(p.iaRr*0.5)+(p.iarr*1)+(p.ijRr*0.5)+(p.ijrr*1)))
#(59)Probability that a Rr juvenile produces a Rr newborn
FjRr2= FJ*(p.jRr*((p.jRr*0.5)+(p.jrr*0.5)+(p.jRR*0.5)+(p.aRr*0.5)+(p.aRR*0.5)+(p.arr*0.5)+
	(p.ijRr*0.5)+(p.ijrr*0.5)+(p.ijRR*0.5)+(p.iaRr*0.5)+(p.iaRR*0.5)+(p.iarr*0.5)))
#(60)Probability that a Rr adult produces a Rr newborn
FaRr2= FA*(p.aRr*((p.aRr*0.5)+(p.arr*0.5)+(p.aRR*0.5)+(p.jRr*0.5)+(p.jRR*0.5)+(p.jrr*0.5)+
	(p.iaRr*0.5)+(p.iarr*0.5)+(p.iaRR*0.5)+(p.ijRr*0.5)+(p.ijRR*0.5)+(p.ijrr*0.5)))
#(62)Probability that a rr juvenile produces a Rr newborn
Fjrr1= FJ*(p.jrr*((p.jRR*1)+(p.jRr*0.5)+(p.aRR*1)+(p.aRr*0.5)+
	(p.ijRR*1)+(p.ijRr*0.5)+(p.iaRR*1)+(p.iaRr*0.5)))
#(63)Probability that a rr adult produces a Rr newborn
Farr1= FA*(p.arr*((p.aRR*1)+(p.aRr*0.5)+(p.jRR*1)+(p.jRr*0.5)+
	(p.iaRR*1)+(p.iaRr*0.5)+(p.ijRR*1)+(p.ijRr*0.5)))
#(113)Probability that a Rr juvenile produces a rr newborn
FjRr3= FJ*(p.jRr*((p.jRr*0.25)+(p.jrr*0.5)+(p.aRr*0.25)+(p.arr*0.5)+
	(p.ijRr*0.25)+(p.ijrr*0.5)+(p.iaRr*0.25)+(p.iarr*0.5)))
#(114)Probability that a Rr adult produces a rr newborn
FaRr3= FA*(p.aRr*((p.aRr*0.25)+(p.arr*0.5)+(p.jRr*0.25)+(p.jrr*0.5)+
	(p.iaRr*0.25)+(p.iarr*0.5)+(p.ijRr*0.25)+(p.ijrr*0.5)))
#(116)Probability that a rr juvenile produces a rr newborn
Fjrr2= FJ*(p.jrr*((p.jrr*1)+(p.jRr*0.5)+(p.arr*1)+(p.aRr*0.5)+
	(p.ijrr*1)+(p.ijRr*0.5)+(p.iarr*1)+(p.iaRr*0.5)))
#(117)Probability that a rr adult produces a rr newborn
Farr2= FA*(p.arr*((p.arr*1)+(p.aRr*0.5)+(p.jrr*1)+(p.jRr*0.5)+
	(p.iarr*1)+(p.iaRr*0.5)+(p.ijrr*1)+(p.ijRr*0.5)))

################### INFECTED SUBPOP ##########################################

#(11)Probability that a RR infected juvenile produces a RR newborn
FijRR1= FJ*(p.ijRR*((p.jRR*1)+(p.jRr*0.5)+(p.aRR*1)+(p.aRr*0.5)+
	(p.ijRR*1)+(p.ijRr*0.5)+(p.iaRR*1)+(p.iaRr*0.5)))
#(12)Probability that a RR infected adult produces a RR newborn
FiaRR1= FA*(p.iaRR*((p.aRR*1)+(p.aRr*0.5)+(p.jRR*1)+(p.jRr*0.5)+
	(p.iaRR*1)+(p.iaRr*0.5)+(p.ijRR*1)+(p.ijRr*0.5)))
#(14)Probability that a Rr infected juvenile produces a RR newborn
FijRr1= FJ*(p.ijRr*((p.jRR*0.5)+(p.jRr*0.25)+(p.aRR*0.5)+(p.aRr*0.25)+
	(p.ijRR*0.5)+(p.ijRr*0.25)+(p.iaRR*0.5)+(p.iaRr*0.25)))
#(15)Probability that a Rr infected adult produces a RR newborn
FiaRr1= FA*(p.iaRr*((p.aRR*0.5)+(p.aRr*0.25)+(p.jRR*0.5)+(p.jRr*0.25)+
	(p.iaRR*0.5)+(p.iaRr*0.25)+(p.ijRR*0.5)+(p.ijRr*0.25)))
#(65)Probability that a RR infected juvenile produces a Rr newborn
FijRR2= FJ*(p.ijRR*((p.jRr*0.5)+(p.jrr*1)+(p.aRr*0.5)+(p.arr*1)+
	(p.ijRr*0.5)+(p.ijrr*1)+(p.iaRr*0.5)+(p.iarr*1)))
#(66)Probability that a RR infected adult produces a Rr newborn
FiaRR2= FA*(p.iaRR*((p.aRr*0.5)+(p.arr*1)+(p.jRr*0.5)+(p.jrr*1)+
	(p.iaRr*0.5)+(p.iarr*1)+(p.ijRr*0.5)+(p.ijrr*1)))
#(68)Probability that a Rr infected juvenile produces a Rr newborn
FijRr2= FJ*(p.ijRr*((p.jRr*0.5)+(p.jrr*0.5)+(p.jRR*0.5)+(p.aRr*0.5)+(p.aRR*0.5)+(p.arr*0.5)+
	(p.ijRr*0.5)+(p.ijrr*0.5)+(p.ijRR*0.5)+(p.iaRr*0.5)+(p.iaRR*0.5)+(p.iarr*0.5)))
#(69)Probability that a Rr infected adult produces a Rr newborn
FiaRr2= FA*(p.iaRr*((p.aRr*0.5)+(p.arr*0.5)+(p.aRR*0.5)+(p.jRr*0.5)+(p.jRR*0.5)+(p.jrr*0.5)+
	(p.iaRr*0.5)+(p.iarr*0.5)+(p.iaRR*0.5)+(p.ijRr*0.5)+(p.ijRR*0.5)+(p.ijrr*0.5)))
#(71)Probability that a rr infected juvenile produces a Rr newborn
Fijrr1= FJi*(p.ijrr*((p.jRR*1)+(p.jRr*0.5)+(p.aRR*1)+(p.aRr*0.5)+
	(p.ijRR*1)+(p.ijRr*0.5)+(p.iaRR*1)+(p.iaRr*0.5)))
#(72)Probability that a rr infected adult produces a Rr newborn
Fiarr1= FAi*(p.iarr*((p.aRR*1)+(p.aRr*0.5)+(p.jRR*1)+(p.jRr*0.5)+
	(p.iaRR*1)+(p.iaRr*0.5)+(p.ijRR*1)+(p.ijRr*0.5)))
#(122)Probability that a Rr infected juvenile produces a rr newborn
FijRr3= FJ*(p.ijRr*((p.jRr*0.25)+(p.jrr*0.5)+(p.aRr*0.25)+(p.arr*0.5)+
	(p.ijRr*0.25)+(p.ijrr*0.5)+(p.iaRr*0.25)+(p.iarr*0.5)))
#(123)Probability that a Rr infected adult produces a rr newborn
FiaRr3= FA*(p.iaRr*((p.aRr*0.25)+(p.arr*0.5)+(p.jRr*0.25)+(p.jrr*0.5)+
	(p.iaRr*0.25)+(p.iarr*0.5)+(p.ijRr*0.25)+(p.ijrr*0.5)))
#(125)Probability that a rr infected juvenile produces a rr newborn
Fijrr2= FJi*(p.ijrr*((p.jrr*1)+(p.jRr*0.5)+(p.arr*1)+(p.aRr*0.5)+
	(p.ijrr*1)+(p.ijRr*0.5)+(p.iarr*1)+(p.iaRr*0.5)))
#(126)Probability that a rr infected adult produces a rr newborn
Fiarr2= FAi*(p.iarr*((p.arr*1)+(p.aRr*0.5)+(p.jrr*1)+(p.jRr*0.5)+
	(p.iarr*1)+(p.iaRr*0.5)+(p.ijrr*1)+(p.ijRr*0.5)))

#F=c(FjRR1,FjRR2,FaRR1,FaRR2,FjRr1,FjRr2,FjRr3,FaRr1,FaRr2,FaRr3,Fjrr1,Fjrr2,Farr1,Farr2)
#Fi=c(FijRR1,FijRR2,FiaRR1,FiaRR2,FijRr1,FijRr2,FijRr3,FiaRr1,FiaRr2,FiaRr3,Fijrr1,Fijrr2,Fiarr1,Fiarr2)
#sum(F); sum(Fi)

################################################
###### Matrix to store parameters ##############
################################################

initial.pars=c(SLnRR*(1-B),FjRR1,FaRR1,0,FjRr1,FaRr1,0,0,0,          SLinRR*a,FijRR1,FiaRR1,0,FijRr1,FiaRr1,0,0,0,
	SnRR*(1-B),SLjRR*(1-B),0,0,0,0,0,0,0,                 SinRR*a,SLijRR*a,0,0,0,0,0,0,0,
	0,SjRR*(1-B),SLaRR*(1-B),0,0,0,0,0,0,                 0,SijRR*a,SLiaRR*a,0,0,0,0,0,0,
	0,FjRR2,FaRR2,SLnRr*(1-B),FjRr2,FaRr2,0,Fjrr1,Farr1,  0,FijRR2,FiaRR2,SLinRr*a,FijRr2,FiaRr2,0,Fijrr1,Fiarr1,
	0,0,0,SnRr*(1-B),SLjRr*(1-B),0,0,0,0,                 0,0,0,SinRr*a,SLijRr*a,0,0,0,0,
	0,0,0,0,SjRr*(1-B),SLaRr*(1-B),0,0,0,                 0,0,0,0,SijRr*a,SLiaRr*a,0,0,0,
	0,0,0,0,FjRr3,FaRr3,SLnrr*(1-B),Fjrr2,Farr2,          0,0,0,0,FijRr3,FiaRr3,SLinrr*a,Fijrr2,Fiarr2,
	0,0,0,0,0,0,Snrr*(1-B),SLjrr*(1-B),0,                 0,0,0,0,0,0,Sinrr*a,SLijrr*a,0,
	0,0,0,0,0,0,0,Sjrr*(1-B),SLarr*(1-B),                 0,0,0,0,0,0,0,Sijrr*a,SLiarr*a,
      SLnRR*B,0,0,0,0,0,0,0,0,            SLinRR*(1-a),0,0,0,0,0,0,0,0,
      SnRR*B,SLjRR*B,0,0,0,0,0,0,0,       SinRR*(1-a),SLijRR*(1-a),0,0,0,0,0,0,0,
      0,SjRR*B,SLaRR*B,0,0,0,0,0,0,       0,SijRR*(1-a),SLiaRR*(1-a),0,0,0,0,0,0,
      0,0,0,SLnRr*B,0,0,0,0,0,            0,0,0,SLinRr*(1-a),0,0,0,0,0,
      0,0,0,SnRr*B,SLjRr*B,0,0,0,0,       0,0,0,SinRr*(1-a),SLijRr*(1-a),0,0,0,0,
      0,0,0,0,SjRr*B,SLaRr*B,0,0,0,       0,0,0,0,SijRr*(1-a),SLiaRr*(1-a),0,0,0,
      0,0,0,0,0,0,SLnrr*B,0,0,            0,0,0,0,0,0,SLinrr*(1-a),0,0,
      0,0,0,0,0,0,Snrr*B,SLjrr*B,0,       0,0,0,0,0,0,Sinrr*(1-a),SLijrr*(1-a),0,
      0,0,0,0,0,0,0,Sjrr*B,SLarr*B,       0,0,0,0,0,0,0,Sijrr*(1-a),SLiarr*(1-a))

par.matrix=matrix(0,Gen,324, byrow=T)
par.matrix[1,]=initial.pars; #par.matrix
#X=matrix(initial.pars,stages,stages,byrow=T);X

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
### Now the loop! (with Matrix M within loop) ######################################
####################################################################################
for (n in 2:Gen) {
     M=matrix(par.matrix[n-1,],stages,stages, byrow=T)
     popn.matrix[n,] = M%*%popn.matrix[n-1,]
####################################################################################
##### Get proportions of MATING genotypes for probabilities ########################
####################################################################################
Mate.Pop <- c(popn.matrix[n,2],popn.matrix[n,3],popn.matrix[n,5],popn.matrix[n,6],
          popn.matrix[n,8],popn.matrix[n,9],popn.matrix[n,11],popn.matrix[n,12],
          popn.matrix[n,14],popn.matrix[n,15],popn.matrix[n,17],popn.matrix[n,18])
	p.jRR=prop(Mate.Pop,1); p.jRr=prop(Mate.Pop,3); p.jrr=prop(Mate.Pop,5)
	p.aRR=prop(Mate.Pop,2); p.aRr=prop(Mate.Pop,4); p.arr=prop(Mate.Pop,6)
	p.ijRR=prop(Mate.Pop,7); p.ijRr=prop(Mate.Pop,9); p.ijrr=prop(Mate.Pop,11)
	p.iaRR=prop(Mate.Pop,8); p.iaRr=prop(Mate.Pop,10); p.iarr=prop(Mate.Pop,12)
####################################################################################
	par.matrix[n,1] = sample(NRR.L.surv,1)*(1-B)   ##B.nRR
	par.matrix[n,2] = FJ*(p.jRR*((p.jRR*1)+(p.jRr*0.5)+(p.aRR*1)+(p.aRr*0.5)+
				(p.ijRR*1)+(p.ijRr*0.5)+(p.iaRR*1)+(p.iaRr*0.5)))  #*(sample(random,1))
	par.matrix[n,3] = FA*(p.aRR*((p.aRR*1)+(p.aRr*0.5)+(p.jRR*1)+(p.jRr*0.5)+
				(p.iaRR*1)+(p.iaRr*0.5)+(p.ijRR*1)+(p.ijRr*0.5)))  #*(sample(random,1))
	par.matrix[n,5] = FJ*(p.jRr*((p.jRR*0.5)+(p.jRr*0.25)+(p.aRR*0.5)+(p.aRr*0.25)+
				(p.ijRR*0.5)+(p.ijRr*0.25)+(p.iaRR*0.5)+(p.iaRr*0.25)))  #*(sample(random,1))
	par.matrix[n,6] = FA*(p.aRr*((p.aRR*0.5)+(p.aRr*0.25)+(p.jRR*0.5)+(p.jRr*0.25)+
				(p.iaRR*0.5)+(p.iaRr*0.25)+(p.ijRR*0.5)+(p.ijRr*0.25)))  #*(sample(random,1))
	par.matrix[n,10] = sample(NiRR.L.surv,1)*a     ##a.nRR
	par.matrix[n,11] = FJ*(p.ijRR*((p.jRR*1)+(p.jRr*0.5)+(p.aRR*1)+(p.aRr*0.5)+
				(p.ijRR*1)+(p.ijRr*0.5)+(p.iaRR*1)+(p.iaRr*0.5)))  #*(sample(random,1))
	par.matrix[n,12] = FA*(p.iaRR*((p.aRR*1)+(p.aRr*0.5)+(p.jRR*1)+(p.jRr*0.5)+
				(p.iaRR*1)+(p.iaRr*0.5)+(p.ijRR*1)+(p.ijRr*0.5)))  #*(sample(random,1))
	par.matrix[n,14] = FJ*(p.ijRr*((p.jRR*0.5)+(p.jRr*0.25)+(p.aRR*0.5)+(p.aRr*0.25)+
				(p.ijRR*0.5)+(p.ijRr*0.25)+(p.iaRR*0.5)+(p.iaRr*0.25)))  #*(sample(random,1))
	par.matrix[n,15] = FA*(p.iaRr*((p.aRR*0.5)+(p.aRr*0.25)+(p.jRR*0.5)+(p.jRr*0.25)+
				(p.iaRR*0.5)+(p.iaRr*0.25)+(p.ijRR*0.5)+(p.ijRr*0.25)))  #*(sample(random,1))
	par.matrix[n,19] = sample(NRR.surv,1)*(1-B)    ##B.nRR
	par.matrix[n,20] = sample(JRR.L.surv,1)*(1-B)   ##B.jRR
	par.matrix[n,28] = sample(NiRR.surv,1)*a       ##a.nRR
	par.matrix[n,29] = sample(JiRR.L.surv,1)*a      ##a.jRR
	par.matrix[n,38] = sample(JRR.surv,1)*(1-B)    ##B.jRR
	par.matrix[n,39] = sample(ARR.L.surv,1)*(1-B)   ##B.aRR
	par.matrix[n,47] = sample(JiRR.surv,1)*a       ##a.jRR
	par.matrix[n,48] = sample(AiRR.L.surv,1)*a      ##a.aRR
	par.matrix[n,56] = FJ*(p.jRR*((p.jRr*0.5)+(p.jrr*1)+(p.aRr*0.5)+(p.arr*1)+
				(p.ijRr*0.5)+(p.ijrr*1)+(p.iaRr*0.5)+(p.iarr*1)))  #*(sample(random,1))
	par.matrix[n,57] = FA*(p.aRR*((p.aRr*0.5)+(p.arr*1)+(p.jRr*0.5)+(p.jrr*1)+
				(p.iaRr*0.5)+(p.iarr*1)+(p.ijRr*0.5)+(p.ijrr*1)))  #*(sample(random,1))
	par.matrix[n,58] = sample(NRr.L.surv,1)*(1-B)   ##B.nRr
	par.matrix[n,59] = FJ*(p.jRr*((p.jRr*0.5)+(p.jrr*0.5)+(p.jRR*0.5)+(p.aRr*0.5)+(p.aRR*0.5)+(p.arr*0.5)+
				(p.ijRr*0.5)+(p.ijrr*0.5)+(p.ijRR*0.5)+(p.iaRr*0.5)+(p.iaRR*0.5)+(p.iarr*0.5)))
                      #*(sample(random,1))
	par.matrix[n,60] = FA*(p.aRr*((p.aRr*0.5)+(p.arr*0.5)+(p.aRR*0.5)+(p.jRr*0.5)+(p.jRR*0.5)+(p.jrr*0.5)+
				(p.iaRr*0.5)+(p.iarr*0.5)+(p.iaRR*0.5)+(p.ijRr*0.5)+(p.ijRR*0.5)+(p.ijrr*0.5)))
                      #*(sample(random,1))
	par.matrix[n,62] = FJ*(p.jrr*((p.jRR*1)+(p.jRr*0.5)+(p.aRR*1)+(p.aRr*0.5)+
				(p.ijRR*1)+(p.ijRr*0.5)+(p.iaRR*1)+(p.iaRr*0.5)))  #*(sample(random,1))
	par.matrix[n,63] = FA*(p.arr*((p.aRR*1)+(p.aRr*0.5)+(p.jRR*1)+(p.jRr*0.5)+
				(p.iaRR*1)+(p.iaRr*0.5)+(p.ijRR*1)+(p.ijRr*0.5)))  #*(sample(random,1))
	par.matrix[n,65] = FJ*(p.ijRR*((p.jRr*0.5)+(p.jrr*1)+(p.aRr*0.5)+(p.arr*1)+
				(p.ijRr*0.5)+(p.ijrr*1)+(p.iaRr*0.5)+(p.iarr*1)))  #*(sample(random,1))
	par.matrix[n,66] = FA*(p.iaRR*((p.aRr*0.5)+(p.arr*1)+(p.jRr*0.5)+(p.jrr*1)+
				(p.iaRr*0.5)+(p.iarr*1)+(p.ijRr*0.5)+(p.ijrr*1)))  #*(sample(random,1))
	par.matrix[n,67] = sample(NiRr.L.surv,1)*a      ##a.nRr
	par.matrix[n,68] = FJ*(p.ijRr*((p.jRr*0.5)+(p.jrr*0.5)+(p.jRR*0.5)+(p.aRr*0.5)+(p.aRR*0.5)+(p.arr*0.5)+
				(p.ijRr*0.5)+(p.ijrr*0.5)+(p.ijRR*0.5)+(p.iaRr*0.5)+(p.iaRR*0.5)+(p.iarr*0.5)))
                      #*(sample(random,1))
	par.matrix[n,69] = FA*(p.iaRr*((p.aRr*0.5)+(p.arr*0.5)+(p.aRR*0.5)+(p.jRr*0.5)+(p.jRR*0.5)+(p.jrr*0.5)+
				(p.iaRr*0.5)+(p.iarr*0.5)+(p.iaRR*0.5)+(p.ijRr*0.5)+(p.ijRR*0.5)+(p.ijrr*0.5)))
                      #*(sample(random,1))
	par.matrix[n,71] = FJi*(p.ijrr*((p.jRR*1)+(p.jRr*0.5)+(p.aRR*1)+(p.aRr*0.5)+
				(p.ijRR*1)+(p.ijRr*0.5)+(p.iaRR*1)+(p.iaRr*0.5)))  #*(sample(random,1))
	par.matrix[n,72] = FAi*(p.iarr*((p.aRR*1)+(p.aRr*0.5)+(p.jRR*1)+(p.jRr*0.5)+
				(p.iaRR*1)+(p.iaRr*0.5)+(p.ijRR*1)+(p.ijRr*0.5)))  #*(sample(random,1))
	par.matrix[n,76] = sample(NRr.surv,1)*(1-B)    ##B.nRr
	par.matrix[n,77] = sample(JRr.L.surv,1)*(1-B)   ##B.jRr
	par.matrix[n,85] = sample(NiRr.surv,1)*a       ##a.nRr
	par.matrix[n,86] = sample(JiRr.L.surv,1)*a      ##a.jRr
	par.matrix[n,95] = sample(JRr.surv,1)*(1-B)    ##B.jRr
	par.matrix[n,96] = sample(ARr.L.surv,1)*(1-B)   ##B.aRr
	par.matrix[n,104] = sample(JiRr.surv,1)*a      ##a.jRr
	par.matrix[n,105] = sample(AiRr.L.surv,1)*a     ##a.aRr
	par.matrix[n,113] = FJ*(p.jRr*((p.jRr*0.25)+(p.jrr*0.5)+(p.aRr*0.25)+(p.arr*0.5)+
				(p.ijRr*0.25)+(p.ijrr*0.5)+(p.iaRr*0.25)+(p.iarr*0.5)))  #*(sample(random,1))
	par.matrix[n,114] = FA*(p.aRr*((p.aRr*0.25)+(p.arr*0.5)+(p.jRr*0.25)+(p.jrr*0.5)+
				(p.iaRr*0.25)+(p.iarr*0.5)+(p.ijRr*0.25)+(p.ijrr*0.5)))  #*(sample(random,1))
	par.matrix[n,115] = sample(Nrr.L.surv,1)*(1-B)  ##B.nrr
	par.matrix[n,116] = FJ*(p.jrr*((p.jrr*1)+(p.jRr*0.5)+(p.arr*1)+(p.aRr*0.5)+
				(p.ijrr*1)+(p.ijRr*0.5)+(p.iarr*1)+(p.iaRr*0.5)))  #*(sample(random,1))
	par.matrix[n,117] = FA*(p.arr*((p.arr*1)+(p.aRr*0.5)+(p.jrr*1)+(p.jRr*0.5)+
				(p.iarr*1)+(p.iaRr*0.5)+(p.ijrr*1)+(p.ijRr*0.5)))  #*(sample(random,1))
	par.matrix[n,122] = FJ*(p.ijRr*((p.jRr*0.25)+(p.jrr*0.5)+(p.aRr*0.25)+(p.arr*0.5)+
				(p.ijRr*0.25)+(p.ijrr*0.5)+(p.iaRr*0.25)+(p.iarr*0.5)))  #*(sample(random,1))
	par.matrix[n,123] = FA*(p.iaRr*((p.aRr*0.25)+(p.arr*0.5)+(p.jRr*0.25)+(p.jrr*0.5)+
				(p.iaRr*0.25)+(p.iarr*0.5)+(p.ijRr*0.25)+(p.ijrr*0.5)))  #*(sample(random,1))
	par.matrix[n,124] = sample(Nirr.L.surv,1)*a     ##a.nrr
	par.matrix[n,125] = FJi*(p.ijrr*((p.jrr*1)+(p.jRr*0.5)+(p.arr*1)+(p.aRr*0.5)+
				(p.ijrr*1)+(p.ijRr*0.5)+(p.iarr*1)+(p.iaRr*0.5)))  #*(sample(random,1))
	par.matrix[n,126] = FAi*(p.iarr*((p.arr*1)+(p.aRr*0.5)+(p.jrr*1)+(p.jRr*0.5)+
				(p.iarr*1)+(p.iaRr*0.5)+(p.ijrr*1)+(p.ijRr*0.5)))  #*(sample(random,1))
	par.matrix[n,133] = sample(Nrr.surv,1)*(1-B)   ##B.nrr
	par.matrix[n,134] = sample(Jrr.L.surv,1)*(1-B)  ##B.jrr
	par.matrix[n,142] = sample(Nirr.surv,1)*a      ##a.nrr
	par.matrix[n,143] = sample(Jirr.L.surv,1)*a     ##a.jrr
	par.matrix[n,152] = sample(Jrr.surv,1)*(1-B)   ##B.jrr
	par.matrix[n,153] = sample(Arr.L.surv,1)*(1-B)  ##B.arr
	par.matrix[n,161] = sample(Jirr.surv,1)*a     ##a.jrr
	par.matrix[n,162] = sample(Airr.L.surv,1)*a     ##a.arr
########### Second half of sub-Matrix - No Fecundities; only Survivorship ####################
	par.matrix[n,163] = sample(NRR.L.surv,1)*B      ##B.nRR
	par.matrix[n,172] = sample(NiRR.L.surv,1)*(1-a) ##a.nRR
	par.matrix[n,181] = sample(NRR.surv,1)*B       ##B.nRR
	par.matrix[n,182] = sample(JRR.L.surv,1)*B      ##B.jRR
	par.matrix[n,190] = sample(NiRR.surv,1)*(1-a)  ##a.nRR
	par.matrix[n,191] = sample(JiRR.L.surv,1)*(1-a) ##a.jRR
	par.matrix[n,200] = sample(JRR.surv,1)*B       ##B.jRR
	par.matrix[n,201] = sample(ARR.L.surv,1)*B      ##B.aRR
	par.matrix[n,209] = sample(JiRR.surv,1)*(1-a)  ##a.jRR
	par.matrix[n,210] = sample(AiRR.L.surv,1)*(1-a) ##a.aRR
	par.matrix[n,220] = sample(NRr.L.surv,1)*B      ##B.nRr
	par.matrix[n,229] = sample(NiRr.L.surv,1)*(1-a) ##a.nRr
	par.matrix[n,238] = sample(NRr.surv,1)*B       ##B.nRr
	par.matrix[n,239] = sample(JRr.L.surv,1)*B      ##B.jRr
	par.matrix[n,247] = sample(NiRr.surv,1)*(1-a)  ##a.nRr
	par.matrix[n,248] = sample(JiRr.L.surv,1)*(1-a) ##a.jRr
	par.matrix[n,257] = sample(JRr.surv,1)*B       ##B.jRr
	par.matrix[n,258] = sample(ARr.L.surv,1)*B      ##B.aRr
	par.matrix[n,266] = sample(JiRr.surv,1)*(1-a)  ##a.jRr
	par.matrix[n,267] = sample(AiRr.L.surv,1)*(1-a) ##a.aRr
	par.matrix[n,277] = sample(Nrr.L.surv,1) *B      ##B.nrr
	par.matrix[n,286] = sample(Nirr.L.surv,1)*(1-a) ##a.nrr
	par.matrix[n,295] = sample(Nrr.surv,1)*B       ##B.nrr
	par.matrix[n,296] = sample(Jrr.L.surv,1)*B      ##B.jrr
	par.matrix[n,304] = sample(Nirr.surv,1)*(1-a)  ##a.nrr
	par.matrix[n,305] = sample(Jirr.L.surv,1)*(1-a) ##a.jrr
	par.matrix[n,314] = sample(Jrr.surv,1)*B       ##B.jrr
	par.matrix[n,315] = sample(Arr.L.surv,1) *B      ##B.arr
	par.matrix[n,323] = sample(Jirr.surv,1)*(1-a) ##a.jrr
	par.matrix[n,324] = sample(Airr.L.surv,1)*(1-a) ##a.arr
##### Allelic Frequencies #########################
	freq.R[n]=(2*(sum(popn.matrix[n,1:3])+sum(popn.matrix[n,10:12]))+
               (sum(popn.matrix[n,4:6])+sum(popn.matrix[n,13:15])))/(sum(popn.matrix[n,])*2)
	freq.r[n]=(2*(sum(popn.matrix[n,7:9])+sum(popn.matrix[n,16:18]))+
               (sum(popn.matrix[n,4:6])+sum(popn.matrix[n,13:15])))/(sum(popn.matrix[n,])*2)
	lambda.vec[n-1] = eigen.analysis(M)$lambda
	SSD.Mat[,n-1] = eigen.analysis(M)$stable.stage
	RV.Mat[,n-1] = eigen.analysis(M)$repro.value
	DPR.vec[n-1] = eigen.analysis(M)$damping.ratio
	total.pop[n]= sum(popn.matrix[n,])
	test.vec[n] = popn.matrix[n,sample(test.vec.s,1)]
	RV.RR[n] = sum(RV.Mat[1,n-1]+RV.Mat[2,n-1]+RV.Mat[3,n-1]+RV.Mat[10,n-1]+RV.Mat[11,n-1]+RV.Mat[12,n-1])
	RV.Rr[n] = sum(RV.Mat[4,n-1]+RV.Mat[5,n-1]+RV.Mat[6,n-1]+RV.Mat[13,n-1]+RV.Mat[14,n-1]+RV.Mat[15,n-1])
	RV.rr[n] = sum(RV.Mat[7,n-1]+RV.Mat[8,n-1]+RV.Mat[9,n-1]+RV.Mat[16,n-1]+RV.Mat[17,n-1]+RV.Mat[18,n-1])
}
   ### Matrix out of loop for Lambda of last generation ###
   M=matrix(par.matrix[n,],stages,stages, byrow=T)  
   lambda.vec[n] = eigen.analysis(M)$lambda
   SSD.Mat[,n] = eigen.analysis(M)$stable.stage
   RV.Mat[,n] = eigen.analysis(M)$repro.value
   DPR.vec[n] = eigen.analysis(M)$damping.ratio

   ### Mean Monte-Carlo Values ###########
   Total.pop.Mat[mc,] = total.pop
}

#####################
### END PROGRAM #####
#####################

#######################################
##### Outputs of Interest #############
#######################################
Mean.Pop=c(rep(0,Gen))
for (c in 1:Gen){
   Mean.Pop[c] = mean(Total.pop.Mat[,c])
}
Mean.Pop
plot(1:Gen, Mean.Pop, 'l', xlim=c(0,Gen), xlab="Year", ylab="Population Size", col='dark blue')

par.matrix;
popn.matrix;
test.vec;
lambda.vec;
freq.R; freq.r
Mate.Prop=c(p.jRR,p.jRr,p.jrr,p.aRR,p.aRr,p.arr,p.ijRR,p.ijRr,p.ijrr,p.iaRR,p.iaRr,p.iarr); sum(Mate.Prop)

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

plot(1:Gen, RV.RR, 'l', ylim=c(0,max(RV.Rr)), xlab="Year", ylab="Reproductive Value", col='dark red', lwd=2)
   lines(1:Gen, RV.Rr,'l', col='dark blue', lwd=2)
   lines(1:Gen, RV.rr,'l', col='dark green', lwd=2)
   legend(Gen*0.75,80, c("RV.RR","RV.Rr","RV.rr"), cex=0.8, col=c("dark red","dark blue","dark green"), lwd=2, lty=1)

#######################################################################
##### Population & Lambda for Rep Betas ###############################
#######################################################################
par(mfrow=c(1,2))
plot(1:Gen, Beta[1,],'l', ylim=c(0,max(Beta)), xlim=c(0,Gen), xlab="Year", ylab="Total Whitebark Population", col='dark red', lwd=2)
   lines(1:Gen, Beta[2,],'l', col='dark blue', lwd=2)
   lines(1:Gen, Beta[3,],'l', col='dark green', lwd=2)
   lines(1:Gen, Beta[4,],'l', col='yellow', lwd=2)
   lines(1:Gen, Beta[5,],'l', col='purple', lwd=2)
   legend(60, (max(Beta)*0.75), c("Beta = 0","Beta = 0.1","Beta = 0.15","Beta = 0.2","Beta = 0.25"), 
   cex=0.7, col=c("dark red","dark blue","dark green","yellow","purple"), lwd=2, lty=1)

plot(1:Gen, Lambda.Mat[1,], 'l', ylim=c(1.012,1.014), xlab="Year", ylab="Matrix Lambda", col='dark red', lwd=2)
   lines(1:Gen, Lambda.Mat[2,],'l', col='dark blue', lwd=2)
   lines(1:Gen, Lambda.Mat[3,],'l', col='dark green', lwd=2)
   lines(1:Gen, Lambda.Mat[4,],'l', col='yellow', lwd=2)
   lines(1:Gen, Lambda.Mat[5,],'l', col='purple', lwd=2)
   legend(250,1.014,c("Lambda 0","Lambda 0.1","Lambda 0.15","Lambda 0.2","Lambda 0.25"),
   cex=0.7, col=c("dark red","dark blue","dark green","yellow","purple"), lwd=2, lty=1)

#######################################################################
### Whitebark Pine Population Variables by Generation #################
#######################################################################
par(mfrow=c(2,2))
plot(1:Gen, total.pop,'l', ylim=c(0,500), xlim=c(0,150), xlab="Year", ylab="Total Whitebark Pine Population", col='dark blue', lwd=2)
plot(1:Gen, lambda.vec, 'l', xlab="Year", ylab="Matrix Lambda", col='dark green', lwd=2)
plot(1:Gen, DPR.vec, 'l', xlab="Year", ylab="Matrix Damping Ratio", col='dark red', lwd=2)
plot(1:Gen, freq.R,'l', xlab="Year", ylim=range(0:1.0), ylab="Allele Frequency in Population", col='dark red', lwd=2)
   lines(1:n, freq.r,'l', col='dark blue', lwd=2)
   legend(Gen-(Gen*0.35), 0.6, c("R","r"),cex=0.8, col=c("dark red","dark blue"),lwd=2,lty=1)

####################################################
### Plot some population graphs ####################
####################################################
##### Newborns = Red ###################
##### Juveniles = Blue #################
##### Adults = Green ###################
par(mfrow=c(2,3))
plot(1:n, popn.matrix[,1],'l', ylim=c(0,max(popn.matrix[,3])), xlim=c(0,Gen), xlab="Year",ylab="Whitebark Pine Population RR",col='dark red',lwd=2)
lines(1:n, popn.matrix[,2], 'l', col='dark blue',lwd=2)
lines(1:n, popn.matrix[,3], 'l', col='dark green',lwd=2)
plot(1:n, popn.matrix[,4],'l', ylim=c(0,max(popn.matrix[,6])), xlim=c(0,Gen), xlab="Year",ylab="Whitebark Pine Population Rr", col='dark red',lwd=2)
lines(1:n, popn.matrix[,5], 'l', col='dark blue',lwd=2)
lines(1:n, popn.matrix[,6], 'l', col='dark green',lwd=2)
plot(1:n, popn.matrix[,7],'l', ylim=c(0,max(popn.matrix[,9])), xlim=c(0,Gen), xlab="Year",ylab="Whitebark Pine Population rr", col='dark red',lwd=2)
lines(1:n, popn.matrix[,8], 'l', col='dark blue',lwd=2)
lines(1:n, popn.matrix[,9], 'l', col='dark green',lwd=2)
########## Infected Subpop ###########################################################################
plot(1:n, popn.matrix[,10],'l', ylim=c(0,max(popn.matrix[,12])), xlim=c(0,Gen), xlab="Year",ylab="Whitebark Pine Population Infected RR",col='dark red',lwd=2)
lines(1:n, popn.matrix[,11], 'l', col='dark blue',lwd=2)
lines(1:n, popn.matrix[,12], 'l', col='dark green',lwd=2)
plot(1:n, popn.matrix[,13],'l', ylim=c(0,max(popn.matrix[,15])), xlim=c(0,Gen), xlab="Year",ylab="Whitebark Pine Population Infected Rr", col='dark red',lwd=2)
lines(1:n, popn.matrix[,14], 'l', col='dark blue',lwd=2)
lines(1:n, popn.matrix[,15], 'l', col='dark green',lwd=2)
plot(1:n, popn.matrix[,16],'l', ylim=c(0,max(popn.matrix[,18])), xlim=c(0,Gen),xlab="Year",ylab="Whitebark Pine Population Infected rr", col='dark red',lwd=2)
lines(1:n, popn.matrix[,17], 'l', col='dark blue',lwd=2)
lines(1:n, popn.matrix[,18], 'l', col='dark green',lwd=2)

