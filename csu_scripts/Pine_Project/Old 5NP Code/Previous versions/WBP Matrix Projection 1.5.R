### clear the deck
rm(list=ls(all=TRUE))

### Load PopBio package
require(popbio)

### Create initial population & survivorships for genotypes (L=loop within class; __= -> next class) ####
initial.pop=c(rep(10,18))

### Newborn Survivorship ###
NRR.L.survive=c(rep(0.4,5)); NRR.survive=c(rep(0.05,5)); NRr.L.survive=c(rep(0.4,5))
NRr.survive=c(rep(0.05,5)); Nrr.L.survive=c(rep(0.4,5)); Nrr.survive=c(rep(0.05,5))
NiRR.L.survive=c(rep(0.4,5)); NiRR.survive=c(rep(0.05,5)); NiRr.L.survive=c(rep(0.4,5))
NiRr.survive=c(rep(0.05,5)); Nirr.L.survive=c(rep(0.4,5)); Nirr.survive=c(rep(0.05,5))
###rnorm(50,0.40,0.10)

### Juvenile Survivorship ###
JRR.L.survive=c(rep(0.5,5)); JRR.survive=c(rep(0.15,5)); JRr.L.survive=c(rep(0.5,5))
JRr.survive=c(rep(0.15,5)); Jrr.L.survive=c(rep(0.5,5)); Jrr.survive=c(rep(0.15,5))
JiRR.L.survive=c(rep(0.5,5)); JiRR.survive=c(rep(0.15,5)); JiRr.L.survive=c(rep(0.5,5))
JiRr.survive=c(rep(0.15,5)); Jirr.L.survive=c(rep(0.5,5)); Jirr.survive=c(rep(0.15,5))
###rnorm(50,0.66,0.20)

### Adult Survivorship ###
ARR.L.survive=c(rep(0.95,5)); ARr.L.survive=c(rep(0.95,5)); Arr.L.survive=c(rep(0.95,5))
AiRR.L.survive=c(rep(0.95,5)); AiRr.L.survive=c(rep(0.95,5)); Airr.L.survive=c(rep(0.95,5))
###runif(50,0.90,0.99)
random=rnorm(50,1.0,0.2)

### Matrix to store population results ############
popn.matrix=matrix(0,10,18, byrow=T)
popn.matrix[1,]=initial.pop; popn.matrix

### Define some initial parameters ############ "L = loop" ##########################

SLnRR=sample(NRR.L.survive,1)      #(01)Survivorship of newborn RR NOT moving to next class
SLinRR=sample(NiRR.L.survive,1)    #(10)Survivorship of infected newborn RR NOT moving to next class
SnRR=sample(NRR.survive,1)         #(19)Survivorship of newborn RR moving to next class
SLjRR=sample(JRR.L.survive,1)      #(20)Survivorship of juvenile RR NOT moving to next class
SinRR=sample(NiRR.survive,1)       #(28)Survivorship of infected newborn RR moving to next class
SLijRR=sample(JiRR.L.survive,1)    #(29)Survivorship of infected juvenile RR NOT moving to next class
SjRR=sample(JRR.survive,1)         #(38)Survivorship of juvenile RR moving to next class
SLaRR=sample(ARR.L.survive,1)      #(39)Survivorship of adult RR
SijRR=sample(JiRR.survive,1)       #(47)Survivorship of infected juvenile RR moving to next class
SLiaRR=sample(AiRR.L.survive,1)    #(48)Survivorship of infected adult RR
SLnRr=sample(NRr.L.survive,1)      #(58)Survivorship of newborn Rr NOT moving to next class
SLinRr=sample(NiRr.L.survive,1)    #(67)Survivorship of infected newborn Rr NOT moving to next class
SnRr=sample(NRr.survive,1)         #(76)Survivorship of newborn Rr moving to next class
SLjRr=sample(JRr.L.survive,1)      #(77)Survivorship of juvenile Rr NOT moving to next class
SinRr=sample(NiRr.survive,1)       #(85)Survivorship of infected newborn Rr moving to next class
SLijRr=sample(JiRr.L.survive,1)    #(86)Survivorship of infected juvenile Rr NOT moving to next class
SjRr=sample(JRr.survive,1)         #(95)Survivorship of juvenile Rr moving to next class
SLaRr=sample(ARr.L.survive,1)      #(96)Survivorship of adult Rr
SijRr=sample(JiRr.survive,1)       #(104)Survivorship of infected juvenile Rr moving to next class
SLiaRr=sample(AiRr.L.survive,1)    #(105)Survivorship of infected adult Rr
SLnrr=sample(Nrr.L.survive,1)      #(115)Survivorship of newborn rr NOT moving to next class
SLinrr=sample(Nirr.L.survive,1)    #(124)Survivorship of infected newborn rr NOT moving to next class
Snrr=sample(Nrr.survive,1)         #(133)Survivorship of newborn rr moving to next class
SLjrr=sample(Jrr.L.survive,1)      #(134)Survivorship of juvenile rr NOT moving to next class
Sinrr=sample(Nirr.survive,1)       #(142)Survivorship of infected newborn rr moving to next class
SLijrr=sample(Jirr.L.survive,1)    #(143)Survivorship of infected juvenile rr NOT moving to next class
Sjrr=sample(Jrr.survive,1)         #(152)Survivorship of juvenile rr moving to next class
SLarr=sample(Arr.L.survive,1)      #(153)Survivorship of adult rr
Sijrr=sample(Jirr.survive,1)       #(161)Survivorship of infected juvenile rr moving to next class
SLiarr=sample(Airr.L.survive,1)    #(162)Survivorship of infected adult rr


######### Infection Probabilities B & a ####################

B=1; a=1
#B.nRR= 1; B.nRr= 1; B.nrr= 1; B.jRR= 1; B.jRr= 1; B.jrr= 1; B.aRR= 1; B.aRr= 1; B.arr= 1;
#a.nRR= 1; a.nRr= 1; a.nrr= 1; a.jRR= 1; a.jRr= 1; a.jrr= 1; a.aRR= 1; a.aRr= 1; a.arr= 1;

############ Fecundities of Infected & Uninfected #######################################
FjRR1=0.04164*2     #(02)Probability that a RR juvenile produces a RR newborn
FjRR2=0.04164*2     #(56)Probability that a RR juvenile produces a Rr newborn
FaRR1=0.04164*4     #(03)Probability that a RR adult produces a RR newborn
FaRR2=0.04164*4     #(57)Probability that a RR adult produces a Rr newborn
FjRr1=0.02082*2     #(05)Probability that a Rr juvenile produces a RR newborn
FjRr2=0.04164*2     #(59)Probability that a Rr juvenile produces a Rr newborn
FjRr3=0.02082*2     #(113)Probability that a Rr juvenile produces a rr newborn
FaRr1=0.02082*4     #(06)Probability that a Rr adult produces a RR newborn
FaRr2=0.04164*4     #(60)Probability that a Rr adult produces a Rr newborn
FaRr3=0.02082*4     #(114)Probability that a Rr adult produces a rr newborn
Fjrr1=0.04164*2     #(62)Probability that a rr juvenile produces a Rr newborn
Fjrr2=0.04164*2     #(116)Probability that a rr juvenile produces a rr newborn
Farr1=0.04164*4     #(63)Probability that a rr adult produces a Rr newborn
Farr2=0.04164*4     #(117)Probability that a rr adult produces a rr newborn
########################################################################################
FijRR1=0.04164*2     #(11)Probability that a RR infected juvenile produces a RR newborn
FijRR2=0.04164*2     #(65)Probability that a RR infected juvenile produces a Rr newborn
FiaRR1=0.04164*4     #(12)Probability that a RR infected adult produces a RR newborn
FiaRR2=0.04164*4     #(66)Probability that a RR infected adult produces a Rr newborn
FijRr1=0.02082*2     #(14)Probability that a Rr infected juvenile produces a RR newborn
FijRr2=0.04164*2     #(68)Probability that a Rr infected juvenile produces a Rr newborn
FijRr3=0.02082*2     #(122)Probability that a Rr infected juvenile produces a rr newborn
FiaRr1=0.02082*4     #(15)Probability that a Rr infected adult produces a RR newborn
FiaRr2=0.04164*4     #(69)Probability that a Rr infected adult produces a Rr newborn
FiaRr3=0.02082*4     #(123)Probability that a Rr infected adult produces a rr newborn
Fijrr1=0.04164*2     #(71)Probability that a rr infected juvenile produces a Rr newborn
Fijrr2=0.04164*2     #(125)Probability that a rr infected juvenile produces a rr newborn
Fiarr1=0.04164*4     #(72)Probability that a rr infected adult produces a Rr newborn
Fiarr2=0.04164*4     #(126)Probability that a rr infected adult produces a rr newborn

### Matrix to store parameter results ##############################################

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

par.matrix=matrix(0,10,324, byrow=T)
par.matrix[1,]=initial.pars; #par.matrix
X=matrix(initial.pars,18,18,byrow=T);X

### Creat vector to store values taken from population matrix #####################
lambda.vec=c(rep(0,10)) ### 10 here = n # generations in projection
freq.R=c(rep(0,10))     ### 10 here = n
freq.R[1]=((2*(initial.pop[1]+initial.pop[2]+initial.pop[3]+initial.pop[10]+initial.pop[11]+initial.pop[12]))
+(initial.pop[4]+initial.pop[5]+initial.pop[6]+initial.pop[13]+initial.pop[14]+initial.pop[15]))/(sum(initial.pop)*2)
freq.r=c(rep(0,10))     ### 10 here = n
freq.r[1]=((2*(initial.pop[7]+initial.pop[8]+initial.pop[9]+initial.pop[16]+initial.pop[17]+initial.pop[18]))
+(initial.pop[4]+initial.pop[5]+initial.pop[6]+initial.pop[13]+initial.pop[14]+initial.pop[15]))/(sum(initial.pop)*2)
test.vec=c(rep(0,10))   ### 10 here = n = # generations in projection
test.vec[1]=initial.pop[1]


### Now the loop! (with Matrix M within loop) ######## Generations = 10 ############
for (n in 2:10) {
     M=matrix(par.matrix[n-1,],18,18, byrow=T)
     popn.matrix[n,] = M%*%popn.matrix[n-1,]
##### Get proportions of MATING genotypes for probabilities ##########################################################
     prop.jRR = popn.matrix[n-1,2]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]+popn.matrix[n-1,10]+popn.matrix[n-1,13]+popn.matrix[n-1,16]))
     prop.jRr = popn.matrix[n-1,5]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]+popn.matrix[n-1,10]+popn.matrix[n-1,13]+popn.matrix[n-1,16]))
     prop.jrr = popn.matrix[n-1,8]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]+popn.matrix[n-1,10]+popn.matrix[n-1,13]+popn.matrix[n-1,16]))
     prop.aRR = popn.matrix[n-1,3]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]+popn.matrix[n-1,10]+popn.matrix[n-1,13]+popn.matrix[n-1,16]))
     prop.aRr = popn.matrix[n-1,6]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]+popn.matrix[n-1,10]+popn.matrix[n-1,13]+popn.matrix[n-1,16]))
     prop.arr = popn.matrix[n-1,9]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]+popn.matrix[n-1,10]+popn.matrix[n-1,13]+popn.matrix[n-1,16]))
#####
     prop.ijRR = popn.matrix[n-1,11]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]+popn.matrix[n-1,10]+popn.matrix[n-1,13]+popn.matrix[n-1,16]))
     prop.ijRr = popn.matrix[n-1,14]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]+popn.matrix[n-1,10]+popn.matrix[n-1,13]+popn.matrix[n-1,16]))
     prop.ijrr = popn.matrix[n-1,17]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]+popn.matrix[n-1,10]+popn.matrix[n-1,13]+popn.matrix[n-1,16]))
     prop.iaRR = popn.matrix[n-1,12]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]+popn.matrix[n-1,10]+popn.matrix[n-1,13]+popn.matrix[n-1,16]))
     prop.iaRr = popn.matrix[n-1,15]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]+popn.matrix[n-1,10]+popn.matrix[n-1,13]+popn.matrix[n-1,16]))
     prop.iarr = popn.matrix[n-1,18]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]+popn.matrix[n-1,10]+popn.matrix[n-1,13]+popn.matrix[n-1,16]))
######################################################################################################################
     par.matrix[n,1] = (sample(NRR.L.survive,1))*(1-B) ##B.nRR
     par.matrix[n,2] = 2*((prop.jRR*prop.jRR*1)+(prop.jRR*prop.jRr*0.5)+(prop.jRR*prop.aRR*1)+(prop.jRR*prop.aRr*0.5)+
                      (prop.jRR*prop.ijRR*1)+(prop.jRR*prop.ijRr*0.5)+(prop.jRR*prop.iaRR*1)+(prop.jRR*prop.iaRr*0.5)) #*(sample(random,1))
     par.matrix[n,3] = 4*((prop.aRR*prop.aRR*1)+(prop.aRR*prop.aRr*0.5)+(prop.aRR*prop.jRR*1)+(prop.aRR*prop.jRr*0.5)+
                      (prop.aRR*prop.iaRR*1)+(prop.aRR*prop.iaRr*0.5)+(prop.aRR*prop.ijRR*1)+(prop.aRR*prop.ijRr*0.5)) #*(sample(random,1))
     par.matrix[n,5] = 2*((prop.jRr*prop.jRR*0.5)+(prop.jRr*prop.jRr*0.25)+(prop.jRr*prop.aRR*0.5)+(prop.jRr*prop.aRr*0.25)+
                      (prop.jRr*prop.ijRR*0.5)+(prop.jRr*prop.ijRr*0.25)+(prop.jRr*prop.iaRR*0.5)+(prop.jRr*prop.iaRr*0.25)) #*(sample(random,1))
     par.matrix[n,6] = 4*((prop.aRr*prop.aRR*0.5)+(prop.aRr*prop.aRr*0.25)+(prop.aRr*prop.jRR*0.5)+(prop.aRr*prop.jRr*0.25)+
                      (prop.aRr*prop.iaRR*0.5)+(prop.aRr*prop.iaRr*0.25)+(prop.aRr*prop.ijRR*0.5)+(prop.aRr*prop.ijRr*0.25)) #*(sample(random,1))
     par.matrix[n,10] = 
     par.matrix[n,11] = 
     par.matrix[n,12] = 
     par.matrix[n,14] = 
     par.matrix[n,15] = 
     par.matrix[n,19] = (sample(NRR.survive,1))*(1-B)  #B.nRR
     par.matrix[n,20] = (sample(JRR.L.survive,1))*(1-B)  #B.nRR
     par.matrix[n,28] = 
     par.matrix[n,29] = 
     par.matrix[n,38] = (sample(JRR.survive,1))*(1-B)  #B.jRR
     par.matrix[n,39] = (sample(ARR.L.survive,1))*(1-B)  #B.aRR
     par.matrix[n,47] = 
     par.matrix[n,48] = 
     par.matrix[n,56] = 2*((prop.jRR*prop.jRr*0.5)+(prop.jRR*prop.jrr*1)+(prop.jRR*prop.aRr*0.5)+(prop.jRR*prop.arr*1)+
                      (prop.jRR*prop.ijRr*0.5)+(prop.jRR*prop.ijrr*1)+(prop.jRR*prop.iaRr*0.5)+(prop.jRR*prop.iarr*1)) #*(sample(random,1))
     par.matrix[n,57] = 4*((prop.aRR*prop.aRr*0.5)+(prop.aRR*prop.arr*1)+(prop.aRR*prop.jRr*0.5)+(prop.aRR*prop.jrr*1)+
                      (prop.aRR*prop.iaRr*0.5)+(prop.aRR*prop.iarr*1)+(prop.aRR*prop.ijRr*0.5)+(prop.aRR*prop.ijrr*1)) #*(sample(random,1))
     par.matrix[n,58] = (sample(NRr.L.survive,1))*(1-B)  #B.nRr
     par.matrix[n,59] = 2*((prop.jRr*prop.jRr*0.5)+(prop.jRr*prop.jrr*0.5)+(prop.jRr*prop.jRR*0.5)+(prop.jRr*prop.aRr*0.5)+(prop.jRr*prop.aRR*0.5)+(prop.jRr*prop.arr*0.5)+
                      (prop.jRr*prop.ijRr*0.5)+(prop.jRr*prop.ijrr*0.5)+(prop.jRr*prop.ijRR*0.5)+(prop.jRr*prop.iaRr*0.5)+(prop.jRr*prop.iaRR*0.5)+(prop.jRr*prop.iarr*0.5)) #*(sample(random,1))
     par.matrix[n,60] = 4*((prop.aRr*prop.aRr*0.5)+(prop.aRr*prop.arr*0.5)+(prop.aRr*prop.aRR*0.5)+(prop.aRr*prop.jRr*0.5)+(prop.aRr*prop.jRR*0.5)+(prop.aRr*prop.jrr*0.5)+
                      (prop.aRr*prop.iaRr*0.5)+(prop.aRr*prop.iarr*0.5)+(prop.aRr*prop.iaRR*0.5)+(prop.aRr*prop.ijRr*0.5)+(prop.aRr*prop.ijRR*0.5)+(prop.aRr*prop.ijrr*0.5)) #*(sample(random,1))
     par.matrix[n,62] = 2*((prop.jrr*prop.jRR*1)+(prop.jrr*prop.jRr*0.5)+(prop.jrr*prop.aRR*1)+(prop.jrr*prop.aRr*0.5)+
                      (prop.jrr*prop.ijRR*1)+(prop.jrr*prop.ijRr*0.5)+(prop.jrr*prop.iaRR*1)+(prop.jrr*prop.iaRr*0.5)) #*(sample(random,1))
     par.matrix[n,63] = 4*((prop.arr*prop.aRR*1)+(prop.arr*prop.aRr*0.5)+(prop.arr*prop.jRR*1)+(prop.arr*prop.jRr*0.5)+
                      (prop.arr*prop.iaRR*1)+(prop.arr*prop.iaRr*0.5)+(prop.arr*prop.ijRR*1)+(prop.arr*prop.ijRr*0.5)) #*(sample(random,1))
     par.matrix[n,65] = 
     par.matrix[n,66] = 
     par.matrix[n,67] = 
     par.matrix[n,68] = 
     par.matrix[n,69] = 
     par.matrix[n,71] = 
     par.matrix[n,72] = 
     par.matrix[n,76] = (sample(NRr.survive,1))*(1-B)  #B.nRr
     par.matrix[n,77] = (sample(JRr.L.survive,1))*(1-B)  #B.jRr
     par.matrix[n,85] = 
     par.matrix[n,86] = 
     par.matrix[n,95] = (sample(JRr.survive,1))*(1-B)  #B.jRr
     par.matrix[n,96] = (sample(ARr.L.survive,1))*(1-B)  #B.aRr
     par.matrix[n,104] = 
     par.matrix[n,105] = 
     par.matrix[n,113] = 2*((prop.jRr*prop.jRr*0.25)+(prop.jRr*prop.jrr*0.5)+(prop.jRr*prop.aRr*0.25)+(prop.jRr*prop.arr*0.5)+
                       (prop.jRr*prop.ijRr*0.25)+(prop.jRr*prop.ijrr*0.5)+(prop.jRr*prop.iaRr*0.25)+(prop.jRr*prop.iarr*0.5)) #*(sample(random,1))
     par.matrix[n,114] = 4*((prop.aRr*prop.aRr*0.25)+(prop.aRr*prop.arr*0.5)+(prop.aRr*prop.jRr*0.25)+(prop.aRr*prop.jrr*0.5)+
                       (prop.aRr*prop.iaRr*0.25)+(prop.aRr*prop.iarr*0.5)+(prop.aRr*prop.ijRr*0.25)+(prop.aRr*prop.ijrr*0.5)) #*(sample(random,1))
     par.matrix[n,115] = (sample(Nrr.L.survive,1))*(1-B)  #B.nrr
     par.matrix[n,116] = 2*((prop.jrr*prop.jrr*1)+(prop.jrr*prop.jRr*0.5)+(prop.jrr*prop.arr*1)+(prop.jrr*prop.aRr*0.5)+
                       (prop.jrr*prop.ijrr*1)+(prop.jrr*prop.ijRr*0.5)+(prop.jrr*prop.iarr*1)+(prop.jrr*prop.iaRr*0.5)) #*(sample(random,1))
     par.matrix[n,117] = 4*((prop.arr*prop.arr*1)+(prop.arr*prop.aRr*0.5)+(prop.arr*prop.jrr*1)+(prop.arr*prop.jRr*0.5)+
                       (prop.arr*prop.iarr*1)+(prop.arr*prop.iaRr*0.5)+(prop.arr*prop.ijrr*1)+(prop.arr*prop.ijRr*0.5)) #*(sample(random,1))
     par.matrix[n,122] = 
     par.matrix[n,123] = 
     par.matrix[n,124] = 
     par.matrix[n,125] = 
     par.matrix[n,126] = 
     par.matrix[n,133] = (sample(Nrr.survive,1))*(1-B)  #B.nrr
     par.matrix[n,134] = (sample(Jrr.L.survive,1))*(1-B)  #B.jrr
     par.matrix[n,142] = 
     par.matrix[n,143] = 
     par.matrix[n,152] = (sample(Jrr.survive,1))*(1-B)  #B.jrr
     par.matrix[n,153] = (sample(Arr.L.survive,1))*(1-B)  #B.arr
     par.matrix[n,161] = 
     par.matrix[n,162] = 
#######################
     par.matrix[n,163] = 
     par.matrix[n,172] = 
     par.matrix[n,181] = 
     par.matrix[n,182] = 
     par.matrix[n,190] = 
     par.matrix[n,191] = 
     par.matrix[n,200] = 
     par.matrix[n,201] = 
     par.matrix[n,209] = 
     par.matrix[n,210] = 
     par.matrix[n,220] = 
     par.matrix[n,229] = 
     par.matrix[n,238] = 
     par.matrix[n,239] = 
     par.matrix[n,247] = 
     par.matrix[n,248] = 
     par.matrix[n,257] = 
     par.matrix[n,258] = 
     par.matrix[n,266] = 
     par.matrix[n,267] = 
     par.matrix[n,277] = 
     par.matrix[n,286] = 
     par.matrix[n,295] = 
     par.matrix[n,296] = 
     par.matrix[n,304] = 
     par.matrix[n,305] = 
     par.matrix[n,314] = 
     par.matrix[n,315] = 
     par.matrix[n,323] = 
     par.matrix[n,324] = 
#####
     freq.R[n] = ((2*(popn.matrix[n,1]+popn.matrix[n,2]+popn.matrix[n,3]))+(popn.matrix[n,4]+popn.matrix[n,5]+popn.matrix[n,6]))/(sum(popn.matrix[n,])*2)
     freq.r[n] = ((2*(popn.matrix[n,7]+popn.matrix[n,8]+popn.matrix[n,9]))+(popn.matrix[n,4]+popn.matrix[n,5]+popn.matrix[n,6]))/(sum(popn.matrix[n,])*2)
     lambda.vec[n-1] = eigen.analysis(M)$lambda
     test.vec[n] = popn.matrix[n,n-1]
}

### Matrix out of loop for Lambda of last generation ###
M=matrix(par.matrix[n,],9,9, byrow=T)  
lambda.vec[n] = eigen.analysis(M)$lambda

##### Outputs of Interest #############
par.matrix;
popn.matrix;
test.vec;
lambda.vec;
freq.R; freq.r
Mating.props=c(rep(0,12)); Mating.props[1]=prop.jRR; Mating.props[2]=prop.jRr; Mating.props[3]=prop.jrr;
Mating.props[4]=prop.aRR; Mating.props[5]=prop.aRr; Mating.props[6]=prop.arr; Mating.props[7]=prop.ijRR;
Mating.props[8]=prop.ijRr; Mating.props[9]=prop.ijrr; Mating.props[10]=prop.iaRR; Mating.props[11]=prop.iaRr;
Mating.props[12]=prop.iarr; Mating.props

### Whitebark Pine Population Variables by Generation #################
popsize1=sum(popn.matrix[1,]); #popsize1
popsize2=sum(popn.matrix[2,]); #popsize2
popsize3=sum(popn.matrix[3,]); #popsize3
popsize4=sum(popn.matrix[4,]); #popsize4
popsize5=sum(popn.matrix[5,]); #popsize5
popsize6=sum(popn.matrix[6,]); #popsize6
popsize7=sum(popn.matrix[7,]); #popsize7
popsize8=sum(popn.matrix[8,]); #popsize8
popsize9=sum(popn.matrix[9,]); #popsize9
popsize10=sum(popn.matrix[10,]); #popsize10
total.popn=c(popsize1,popsize2,popsize3,popsize4,popsize5,popsize6,popsize7,popsize8,popsize9,popsize10)
plot(1:n, total.popn,'l', xlab="Year", ylab="Total Whitebark Pine Population", col='dark red', lwd=2)

### Now lets plot some population graphs ####################
par(mfrow=c(1,3))
plot(1:n, popn.matrix[,1],'l', ylim=range(0,40), xlab="Year",ylab="Whitebark Pine Population RR",col='dark red',lwd=2)
lines(1:n, popn.matrix[,2], 'l', col='dark blue',lwd=2)
lines(1:n, popn.matrix[,3], 'l', col='dark green',lwd=2)
text(4,6,'Red = Newborns'); text(4,5.5,'Blue = Juveniles'); text(4,5,'Green = Adults')
plot(1:n, popn.matrix[,4],'l', ylim=range(0,40), xlab="Year",ylab="Whitebark Pine Population Rr", col='dark red',lwd=2)
lines(1:n, popn.matrix[,5], 'l', col='dark blue',lwd=2)
lines(1:n, popn.matrix[,6], 'l', col='dark green',lwd=2)
text(4,6,'Red = Newborns'); text(4,5.5,'Blue = Juveniles'); text(4,5,'Green = Adults')
plot(1:n, popn.matrix[,7],'l', ylim=range(0,40), xlab="Year",ylab="Whitebark Pine Population rr", col='dark red',lwd=2)
lines(1:n, popn.matrix[,8], 'l', col='dark blue',lwd=2)
lines(1:n, popn.matrix[,9], 'l', col='dark green',lwd=2)
text(4,6,'Red = Newborns'); text(4,5.5,'Blue = Juveniles'); text(4,5,'Green = Adults')


x1=popsize2-popsize1; x2=popsize3-popsize2; x3=popsize4-popsize3
x4=popsize5-popsize4; x5=popsize6-popsize5; x6=popsize7-popsize6
x7=popsize8-popsize7; x8=popsize9-popsize8; x9=popsize10-popsize9

pop.diffs=c(x1,x2,x3,x4,x5,x6,x7,x8,x9)
par(mfrow=c(1,2))
plot(1:9, pop.diffs, 'l', xlab="1-9", ylab="PopSize Difference", col='dark red', lwd=2)
plot(1:10, lambda.vec, 'l', xlab="Year", ylab="Matrix Lambda", col='dark red', lwd=2)


