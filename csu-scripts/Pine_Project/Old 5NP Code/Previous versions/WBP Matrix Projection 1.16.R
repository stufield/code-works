########################################################################
###### 3 CLASS 2 MATRIX SETUP ##########################################
########################################################################
require(popbio)
rm(list=ls())
pro <- function(v,x) {v[x]/sum(v)}
P <- function(v,x,y) {v[x]*v[y]}

##################################################################
######## Create initial population & genotype fitnesses ##########
##################################################################
NRR=25; JRR=0; ARR=0
NRr=50; JRr=0; ARr=0
Nrr=25; Jrr=0; Arr=0
NRRi=0; JRRi=0; ARRi=0
NRri=0; JRri=0; ARri=0
Nrri=0; Jrri=0; Arri=0
I.pop <- c(NRR,JRR,ARR,NRr,JRr,ARr,Nrr,Jrr,Arr,NRRi,JRRi,ARRi,NRri,JRri,ARri,Nrri,Jrri,Arri); stages = length(I.pop)
Gen = 10
FA = FJ = FAi = FJi = 1
s1 = 0.95; s2 = 0.05; s3 = 0.95; s4 = 0.05; s5 = 0.99

###################################################
### Matrix for Popn & Parameters ##################
###################################################
popn.Mat <- matrix(0, Gen, stages, byrow=T); popn.Mat[1,] = I.pop
Surviv.Mat <- matrix(0,Gen-1,stages)
Fecund.Mat <- matrix(0,Gen-1,stages)

###################################################
### Define genotype survivorships #################
###################################################

SLnRR = s1			#(01)Survivorship of newborn RR NOT moving to next class
SLinRR = s1			#(10)Survivorship of infected newborn RR NOT moving to next class
SnRR = s2			#(19)Survivorship of newborn RR moving to next class
SLjRR = s3			#(20)Survivorship of juvenile RR NOT moving to next class
SinRR = s2			#(28)Survivorship of infected newborn RR moving to next class
SLijRR = s3			#(29)Survivorship of infected juvenile RR NOT moving to next class
SjRR = s4			#(38)Survivorship of juvenile RR moving to next class
SLaRR = s5			#(39)Survivorship of adult RR
SijRR = s4			#(47)Survivorship of infected juvenile RR moving to next class
SLiaRR = s5			#(48)Survivorship of infected adult RR
SLnRr = s1			#(58)Survivorship of newborn Rr NOT moving to next class
SLinRr = s1			#(67)Survivorship of infected newborn Rr NOT moving to next class
SnRr = s2			#(76)Survivorship of newborn Rr moving to next class
SLjRr = s3			#(77)Survivorship of juvenile Rr NOT moving to next class
SinRr = s2			#(85)Survivorship of infected newborn Rr moving to next class
SLijRr = s3			#(86)Survivorship of infected juvenile Rr NOT moving to next class
SjRr = s4			#(95)Survivorship of juvenile Rr moving to next class
SLaRr = s5			#(96)Survivorship of adult Rr
SijRr = s4			#(104)Survivorship of infected juvenile Rr moving to next class
SLiaRr = s5			#(105)Survivorship of infected adult Rr
SLnrr = s1			#(115)Survivorship of newborn rr NOT moving to next class
SLinrr = s1			#(124)Survivorship of infected newborn rr NOT moving to next class
Snrr = s2			#(133)Survivorship of newborn rr moving to next class
SLjrr = s3			#(134)Survivorship of juvenile rr NOT moving to next class
Sinrr = s2			#(142)Survivorship of infected newborn rr moving to next class
SLijrr = s3			#(143)Survivorship of infected juvenile rr NOT moving to next class
Sjrr = s4			#(152)Survivorship of juvenile rr moving to next class
SLarr = s5			#(153)Survivorship of adult rr
Sijrr = s4			#(161)Survivorship of infected juvenile rr moving to next class
SLiarr = s5			#(162)Survivorship of infected adult rr

###############################################
############### Beta Loop #####################
###############################################
Beta.vec = seq(0,1,length=6); Rep = length(Beta.vec)
Totals.Mat <- matrix(0,(Rep*Gen), stages+3)      # storage matrix for total population for plotting, etc
colnames(Totals.Mat) = c("Beta","Gen","Total","NRR","JRR","ARR","NRr","JRr","ARr","Nrr","Jrr","Arr","NRRi",
                          "JRRi","ARRi","NRri","JRri","ARri","Nrri","Jrri","Arri")
count = 0      # index for storing data
Sum.F = matrix(0, Gen-1, Rep)

for (c in 1:Rep) {
   Beta = Beta.vec[c]

#################################################################
################ Update Totals Matrix ###########################
#################################################################
count= count+1
Totals.Mat[count,1]= Beta
Totals.Mat[count,2]= 1
Totals.Mat[count,3]= sum(I.pop)
Totals.Mat[count,4:(stages+3)]= I.pop

	##############################################################################
	########## TEMPORARY SUBROUTINE TO CREATE FECUNDITY AFTER SURVIVORSHIP #######
	##############################################################################
	par.vec.SI = c(rep(0,324))
  par.vec.SI[1] = SLnRR*(1-Beta)
	par.vec.SI[19] = SnRR*(1-Beta)
	par.vec.SI[20] = SLjRR*(1-Beta)
	par.vec.SI[38] = SjRR*(1-Beta)
	par.vec.SI[39] = SLaRR*(1-Beta)
	par.vec.SI[58] = SLnRr*(1-Beta)
	par.vec.SI[76] = SnRr*(1-Beta)
	par.vec.SI[77] = SLjRr*(1-Beta)
	par.vec.SI[95] = SjRr*(1-Beta)
	par.vec.SI[96] = SLaRr*(1-Beta)
	par.vec.SI[115] = SLnrr*(1-Beta)
	par.vec.SI[133] = Snrr*(1-Beta)
	par.vec.SI[134] = SLjrr*(1-Beta)
	par.vec.SI[152] = Sjrr*(1-Beta)
	par.vec.SI[153] = SLarr*(1-Beta)
	par.vec.SI[163]= SLnRR*(Beta)
	par.vec.SI[181]= SnRR*(Beta)
	par.vec.SI[182]= SLjRR*(Beta)
	par.vec.SI[200]= SjRR*(Beta)
	par.vec.SI[201]= SLaRR*(Beta)
	par.vec.SI[220]= SLnRr*(Beta)
	par.vec.SI[238]= SnRr*(Beta)
	par.vec.SI[239]= SLjRr*(Beta)
	par.vec.SI[257]= SjRr*(Beta)
	par.vec.SI[258]= SLaRr*(Beta)
	par.vec.SI[277]= SLnrr*(Beta)
	par.vec.SI[295]= Snrr*(Beta)
	par.vec.SI[296]= SLjrr*(Beta)
	par.vec.SI[314]= Sjrr*(Beta)
	par.vec.SI[315]= SLarr*(Beta)
	par.vec.SI[172] = SLinRR
	par.vec.SI[190] = SinRR
	par.vec.SI[191] = SLijRR
	par.vec.SI[209] = SijRR
	par.vec.SI[210] = SLiaRR
	par.vec.SI[229] = SLinRr
	par.vec.SI[247] = SinRr
	par.vec.SI[248] = SLijRr
	par.vec.SI[266] = SijRr
	par.vec.SI[267] = SLiaRr
	par.vec.SI[286] = SLinrr
	par.vec.SI[304] = Sinrr
	par.vec.SI[305] = SLijrr
	par.vec.SI[323] = Sijrr
	par.vec.SI[324] = SLiarr
	M.SI = matrix(par.vec.SI, stages, stages, byrow=T)
	
####################################################################################
########################### Now the Loop! ##########################################
####################################################################################
for (n in 2:Gen) {

	SI.popn.vec = M.SI%*%popn.Mat[n-1,]
	Surviv.Mat[n-1,] = SI.popn.vec

	##############################################################################
	############################## Mating & Reproduction #########################
	##############################################################################
	Mate.Pop <- SI.popn.vec[c(2,3,5,6,8,9,11,12,14,15,17,18)]; Mate.Pop
	Sum.Mpop = sum(Mate.Pop)

	   p.jRR=pro(Mate.Pop,1); 		p.jRr=pro(Mate.Pop,3); 		p.jrr=pro(Mate.Pop,5)
	   p.aRR=pro(Mate.Pop,2); 		p.aRr=pro(Mate.Pop,4); 		p.arr=pro(Mate.Pop,6)
	   p.ijRR=pro(Mate.Pop,7); 	p.ijRr=pro(Mate.Pop,9); 	p.ijrr=pro(Mate.Pop,11)
	   p.iaRR=pro(Mate.Pop,8); 	p.iaRr=pro(Mate.Pop,10); 	p.iarr=pro(Mate.Pop,12)

#	    	    1      2      3      4      5     6       7       8       9       10      11      12
	mpv = c(p.jRR, p.aRR, p.jRr, p.aRr, p.jrr, p.arr, p.ijRR, p.iaRR, p.ijRr, p.iaRr, p.ijrr, p.iarr)

	par.vec = c(rep(0,324))
	par.vec[2] = FJ*((P(mpv,1,1)*1)+(P(mpv,1,3)*0.5)+(P(mpv,1,2)*1)+(P(mpv,1,4)*0.5)+(P(mpv,1,7)*1)+(P(mpv,1,9)*0.5)+(P(mpv,1,8)*1)+(P(mpv,1,10)*0.5))

	par.vec[3] = FA*((P(mpv,2,2)*1)+(P(mpv,2,4)*0.5)+(P(mpv,2,1)*1)+(P(mpv,2,3)*0.5)+(P(mpv,2,8)*1)+(P(mpv,2,10)*0.5)+(P(mpv,2,7)*1)+(P(mpv,2,9)*0.5))

	par.vec[5] = FJ*(P(mpv,3,1)*0.5+P(mpv,3,3)*0.25+P(mpv,3,2)*0.5+P(mpv,3,4)*0.25+P(mpv,3,7)*0.5+P(mpv,3,9)*0.25+P(mpv,3,8)*0.5+P(mpv,3,10)*0.25)

	par.vec[6] = FA*(P(mpv,4,2)*0.5+P(mpv,4,4)*0.25+P(mpv,4,1)*0.5+P(mpv,4,3)*0.25+P(mpv,4,8)*0.5+P(mpv,4,10)*0.25+P(mpv,4,7)*0.5+P(mpv,4,9)*0.25)

	par.vec[11] = FJ*((P(mpv,7,1)*1)+(P(mpv,7,3)*0.5)+(P(mpv,7,2)*1)+(P(mpv,7,4)*0.5)+(P(mpv,7,7)*1)+(P(mpv,7,9)*0.5)+(P(mpv,7,8)*1)+(P(mpv,7,10)*0.5))

	par.vec[12] = FA*((P(mpv,8,2)*1)+(P(mpv,8,4)*0.5)+(P(mpv,8,1)*1)+(P(mpv,8,3)*0.5)+(P(mpv,8,8)*1)+(P(mpv,8,10)*0.5)+(P(mpv,8,7)*1)+(P(mpv,8,9)*0.5))

	par.vec[14] = FJ*(P(mpv,9,1)*0.5+P(mpv,9,3)*0.25+P(mpv,9,2)*0.5+P(mpv,9,4)*0.25+P(mpv,9,7)*0.5+P(mpv,9,9)*0.25+P(mpv,9,8)*0.5+P(mpv,9,10)*0.25)

	par.vec[15] = FA*(P(mpv,10,2)*0.5+P(mpv,10,4)*0.25+P(mpv,10,1)*0.5+P(mpv,10,3)*0.25+P(mpv,10,8)*0.5+P(mpv,10,10)*0.25+P(mpv,10,7)*0.5+P(mpv,10,9)*0.25)

	par.vec[56] = FJ*(P(mpv,1,3)*0.5+P(mpv,1,5)*1+P(mpv,1,4)*0.5+P(mpv,1,6)*1+P(mpv,1,9)*0.5+P(mpv,1,11)*1+P(mpv,1,10)*0.5+P(mpv,1,12)*1)

	par.vec[57] = FA*(P(mpv,2,3)*0.5+P(mpv,2,5)*1+P(mpv,2,4)*0.5+P(mpv,2,6)*1+P(mpv,2,9)*0.5+P(mpv,2,11)*1+P(mpv,2,10)*0.5+P(mpv,2,12)*1)

	par.vec[59] = FJ*(P(mpv,3,1)*0.5+P(mpv,3,2)*0.5+P(mpv,3,3)*0.5+P(mpv,3,4)*0.5+P(mpv,3,5)*0.5+P(mpv,3,6)*0.5+
				P(mpv,3,7)*0.5+P(mpv,3,8)*0.5+P(mpv,3,9)*0.5+P(mpv,3,10)*0.5+P(mpv,3,11)*0.5+P(mpv,3,12)*0.5)

	par.vec[60] = FA*(P(mpv,4,1)*0.5+P(mpv,4,2)*0.5+P(mpv,4,3)*0.5+P(mpv,4,4)*0.5+P(mpv,4,5)*0.5+P(mpv,4,6)*0.5+
				P(mpv,4,7)*0.5+P(mpv,4,8)*0.5+P(mpv,4,9)*0.5+P(mpv,4,10)*0.5+P(mpv,4,11)*0.5+P(mpv,4,12)*0.5)

	par.vec[62] = FJ*(P(mpv,5,1)*1+P(mpv,5,3)*0.5+P(mpv,5,2)*1+P(mpv,5,4)*0.5+P(mpv,5,7)*1+P(mpv,5,9)*0.5+P(mpv,5,8)*1+P(mpv,5,10)*0.5)

	par.vec[63] = FA*(P(mpv,6,1)*1+P(mpv,6,3)*0.5+P(mpv,6,2)*1+P(mpv,6,4)*0.5+P(mpv,6,7)*1+P(mpv,6,9)*0.5+P(mpv,6,8)*1+P(mpv,6,10)*0.5)

	par.vec[65] = FJ*(P(mpv,7,3)*0.5+P(mpv,7,5)*1+P(mpv,7,4)*0.5+P(mpv,7,6)*1+P(mpv,7,9)*0.5+P(mpv,7,11)*1+P(mpv,7,10)*0.5+P(mpv,7,12)*1)

	par.vec[66] = FA*(P(mpv,8,3)*0.5+P(mpv,8,5)*1+P(mpv,8,4)*0.5+P(mpv,8,6)*1+P(mpv,8,9)*0.5+P(mpv,8,11)*1+P(mpv,8,10)*0.5+P(mpv,8,12)*1)

	par.vec[68] = FJ*(P(mpv,9,1)*0.5+P(mpv,9,2)*0.5+P(mpv,9,3)*0.5+P(mpv,9,4)*0.5+P(mpv,9,5)*0.5+P(mpv,9,6)*0.5+
				P(mpv,9,7)*0.5+P(mpv,9,8)*0.5+P(mpv,9,9)*0.5+P(mpv,9,10)*0.5+P(mpv,9,11)*0.5+P(mpv,9,12)*0.5)

	par.vec[69] = FA*(P(mpv,10,1)*0.5+P(mpv,10,2)*0.5+P(mpv,10,3)*0.5+P(mpv,10,4)*0.5+P(mpv,10,5)*0.5+P(mpv,10,6)*0.5+
				P(mpv,10,7)*0.5+P(mpv,10,8)*0.5+P(mpv,10,9)*0.5+P(mpv,10,10)*0.5+P(mpv,10,11)*0.5+P(mpv,10,12)*0.5)

	par.vec[71] = FJi*(P(mpv,11,1)*1+P(mpv,11,3)*0.5+P(mpv,11,2)*1+P(mpv,11,4)*0.5+P(mpv,11,7)*1+P(mpv,11,9)*0.5+P(mpv,11,8)*1+P(mpv,11,10)*0.5)

	par.vec[72] = FAi*(P(mpv,12,1)*1+P(mpv,12,3)*0.5+P(mpv,12,2)*1+P(mpv,12,4)*0.5+P(mpv,12,7)*1+P(mpv,12,9)*0.5+P(mpv,12,8)*1+P(mpv,12,10)*0.5)

	par.vec[113] = FJ*(P(mpv,3,3)*0.25+P(mpv,3,5)*0.5+P(mpv,3,4)*0.25+P(mpv,3,6)*0.5+P(mpv,3,9)*0.25+P(mpv,3,11)*0.5+P(mpv,3,10)*0.25+P(mpv,3,12)*0.5)

	par.vec[114] = FA*(P(mpv,4,3)*0.25+P(mpv,4,5)*0.5+P(mpv,4,4)*0.25+P(mpv,4,6)*0.5+P(mpv,4,9)*0.25+P(mpv,4,11)*0.5+P(mpv,4,10)*0.25+P(mpv,4,12)*0.5)

	par.vec[116] = FJ*(P(mpv,5,5)*1+P(mpv,5,3)*0.5+P(mpv,5,6)*1+P(mpv,5,4)*0.5+P(mpv,5,11)*1+P(mpv,5,9)*0.5+P(mpv,5,12)*1+P(mpv,5,10)*0.5)

	par.vec[117] = FA*(P(mpv,6,5)*1+P(mpv,6,3)*0.5+P(mpv,6,6)*1+P(mpv,6,4)*0.5+P(mpv,6,11)*1+P(mpv,6,9)*0.5+P(mpv,6,12)*1+P(mpv,6,10)*0.5)

	par.vec[122] = FJ*(P(mpv,9,3)*0.25+P(mpv,9,5)*0.5+P(mpv,9,4)*0.25+P(mpv,9,6)*0.5+P(mpv,9,9)*0.25+P(mpv,9,11)*0.5+P(mpv,9,10)*0.25+P(mpv,9,12)*0.5)

	par.vec[123] = FA*(P(mpv,10,3)*0.25+P(mpv,10,5)*0.5+P(mpv,10,4)*0.25+P(mpv,10,6)*0.5+P(mpv,10,9)*0.25+P(mpv,10,11)*0.5+P(mpv,10,10)*0.25+P(mpv,10,12)*0.5)

	par.vec[125] = FJi*(P(mpv,11,5)*1+P(mpv,11,3)*0.5+P(mpv,11,6)*1+P(mpv,11,4)*0.5+P(mpv,11,11)*1+P(mpv,11,9)*0.5+P(mpv,11,12)*1+P(mpv,11,10)*0.5)

	par.vec[126] = FAi*(P(mpv,12,5)*1+P(mpv,12,3)*0.5+P(mpv,12,6)*1+P(mpv,12,4)*0.5+P(mpv,12,11)*1+P(mpv,12,9)*0.5+P(mpv,12,12)*1+P(mpv,12,10)*0.5)
	M.FC = matrix(par.vec, stages, stages, byrow=T)
  Sum.F[n-1,c] = sum(M.FC)
  
	################################################
	F.vec = M.FC%*%SI.popn.vec
	Fecund.Mat[n-1,] = F.vec
	popn.Mat[n,] = SI.popn.vec+F.vec
  count=count+1
  Totals.Mat[count,1]= Beta
  Totals.Mat[count,2]= n
  Totals.Mat[count,3]= sum(popn.Mat[n,])
  Totals.Mat[count,4:(stages+3)]= popn.Mat[n,]

  }       # END GENERATION LOOP
}       # END BETA LOOP

Totals.Mat
#######################################################################
############## Population Graphs ######################################
#######################################################################
#plot(1:Gen, Beta.Mat[1,],'l', ylim=c(0,max(Beta.Mat)), xlim=c(0,Gen), xlab="Year",
#	ylab="Total Whitebark Population", col='dark red', lwd=2)
#   lines(1:Gen, Beta.Mat[2,],'l', col='dark blue', lwd=2)
#   lines(1:Gen, Beta.Mat[3,],'l', col='dark green', lwd=2)
#   lines(1:Gen, Beta.Mat[4,],'l', col='yellow', lwd=2)
#   lines(1:Gen, Beta.Mat[5,],'l', col='purple', lwd=2)
#   legend((Gen*0.25),(max(Beta.Mat)*0.75), c("Beta = 0","Beta = 0.1","Beta = 0.2","Beta = 0.3","Beta = 0.5"), 
#   cex=0.7, col=c("dark red","dark blue","dark green","yellow","purple"), lwd=2, lty=1)

#######################################################################
### Whitebark Pine Population Variables by Generation #################
#######################################################################
#par(mfrow=c(2,2))
#plot(1:Gen, total.pop,'l', ylim=c(0,max(total.pop)), xlim=c(0,Gen), xlab="Year", ylab="Total Whitebark Pine Population", col='dark blue', lwd=2)
#plot(1:Gen, lambda.vec, 'l', xlab="Year", ylab="Matrix Lambda", col='dark green', lwd=2)
#plot(1:Gen, DPR.vec, 'l', xlab="Year", ylab="Matrix Damping Ratio", col='dark red', lwd=2)
#plot(1:Gen, freq.R,'l', xlab="Year", ylim=range(0:1.0), ylab="Allele Frequency in Population", col='dark red', lwd=2)
#   lines(1:n, freq.r,'l', col='dark blue', lwd=2)
#   legend(Gen-(Gen*0.35), 0.6, c("R","r"),cex=0.8, col=c("dark red","dark blue"),lwd=2,lty=1)