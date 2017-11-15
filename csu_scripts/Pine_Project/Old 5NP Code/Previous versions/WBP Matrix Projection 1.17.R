#################################################################
###### 2 CLASS 2 MATRIX - ORIGINAL MATING FREQUENCIES ###########
#################################################################
#require(popbio)
rm(list=ls())
pro <- function(v,x) {v[x]/sum(v)}
P <- function(v,x,y) {v[x]*v[y]}

##################################################################
######## Create initial population & genotype fitnesses ##########
##################################################################
NRR=25; ARR=0
NRr=50; ARr=0
Nrr=25; Arr=0
NRRi=0; ARRi=0
NRri=0; ARri=0
Nrri=0; Arri=0
I.pop <- c(NRR,ARR,NRr,ARr,Nrr,Arr,NRRi,ARRi,NRri,ARri,Nrri,Arri); stages = length(I.pop)
Gen = 10
#F = Fi = 1    # fecundity (of infected)
s1 = 0.95; s2 = 0.05; s3 = 0.95
SLn = SLin = s1  # survive within newbie class
Sn = Sin = s2    # transition between classes	
SLa = SLia = s3  # survive within adult class


###################################################
### Matrix for Popn & Parameters ##################
###################################################
popn.Mat <- matrix(0, Gen, stages)
popn.Mat[1,] = I.pop
Survive.Mat <- matrix(0, Gen-1, stages)
Fecund.Mat <- matrix(0, Gen-1, stages)

###############################################
####### Double Loop Begin #####################
###############################################
Beta.vec = seq(0,1,length=6)
Rep = length(Beta.vec)
Totals.Mat <- matrix(0,(Rep*Gen), stages+3)      # storage matrix for total population for plotting, etc
colnames(Totals.Mat) = c("Beta","Gen","Total","NRR","ARR","NRr","ARr","Nrr","Arr","NRRi","ARRi",
                         "NRri","ARri","Nrri","Arri")
count = 0      # index for storing data
Sum.F = matrix(0, Gen-1, Rep)

for (c in 1:Rep) {         # looping over values of Beta
   Beta = Beta.vec[c]

##############################################################################
### Creat vectors/matrices to store running values ###########################
##############################################################################
total.pop = c(rep(0,Gen)); total.pop[1] = sum(I.pop)
count=count+1
Totals.Mat[count,1]= Beta
Totals.Mat[count,2]= 1
Totals.Mat[count,3]= sum(I.pop)
Totals.Mat[count,4:15]= I.pop

	##############################################################################
	########## TEMPORARY SUBROUTINE TO CREATE FECUNDITY AFTER SURVIVORSHIP #######
	##############################################################################
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
########################### Now the Loop! ##########################################
####################################################################################
for (n in 2:Gen) {

	SI.popn.vec = M.SI%*%popn.Mat[n-1,]
	Survive.Mat[n-1,] = SI.popn.vec

	##############################################################################
	############################## Mating & Reproduction #########################
	##############################################################################
	Mate.Pop <- SI.popn.vec[c(2,4,6,8,10,12)]; Mate.Pop
	Sum.Mpop = sum(Mate.Pop)

	p.aRR=pro(Mate.Pop,1); 		p.aRr=pro(Mate.Pop,2); 		p.arr=pro(Mate.Pop,3)
	p.aRRi=pro(Mate.Pop,4); 	p.aRri=pro(Mate.Pop,5); 	p.arri=pro(Mate.Pop,6)

#          1     2      3       4       5      6 
mpv = c(p.aRR, p.aRr, p.arr, p.aRRi, p.aRri, p.arri); mpv
#P=mpv[1]; H=mpv[2]; Q=mpv[3]; Pi=mpv[4]; Hi=mpv[5]; Qi=mpv[6]

	par.vec = c(rep(0,stages^2))
	par.vec[2] = (P(mpv,1,1)*1)+(P(mpv,1,2)*0.5)+(P(mpv,1,4)*1)+(P(mpv,1,5)*0.5)

	par.vec[4] = (P(mpv,2,2)*0.25)+(P(mpv,2,5)*0.25)+(P(mpv,2,1)*0.5)+(P(mpv,2,4)*0.5)

 	par.vec[8] = (P(mpv,4,1)*1)+(P(mpv,4,4)*1)+(P(mpv,4,2)*0.5)+(P(mpv,4,5)*0.5)

	par.vec[10] = (P(mpv,5,5)*0.25)+(P(mpv,5,2)*0.25)+(P(mpv,5,1)*0.5)+(P(mpv,5,4)*0.5)

	par.vec[26] = (P(mpv,1,2)*0.5)+(P(mpv,1,3)*1)+(P(mpv,1,5)*0.5)+(P(mpv,1,6)*1)

	par.vec[28] = (P(mpv,2,2)*0.5)+(P(mpv,2,5)*0.5)+(P(mpv,2,1)*0.5)+(P(mpv,2,4)*0.5)+(P(mpv,2,3)*0.5)+(P(mpv,2,6)*0.5)

	par.vec[30] = (P(mpv,3,1)*1)+(P(mpv,3,4)*1)+(P(mpv,3,2)*0.5)+(P(mpv,3,5)*0.5)

	par.vec[32] = (P(mpv,4,2)*0.5)+(P(mpv,4,5)*0.5)+(P(mpv,4,3)*1)+(P(mpv,4,6)*1)

	par.vec[34] = (P(mpv,5,2)*0.5)+(P(mpv,5,5)*0.5)+(P(mpv,5,1)*0.5)+(P(mpv,5,4)*0.5)+(P(mpv,5,3)*0.5)+(P(mpv,5,6)*0.5)
	
	par.vec[36] = (P(mpv,6,1)*1)+(P(mpv,6,4)*1)+(P(mpv,6,2)*0.5)+(P(mpv,6,5)*0.5)

	par.vec[52] = (P(mpv,2,2)*0.25)+(P(mpv,2,5)*0.25)+(P(mpv,2,3)*0.5)+(P(mpv,2,6)*0.5)

	par.vec[54] = (P(mpv,3,3)*1)+(P(mpv,3,6)*1)+(P(mpv,3,2)*0.5)+(P(mpv,3,5)*0.5)

	par.vec[58] = (P(mpv,5,2)*0.25)+(P(mpv,5,5)*0.25)+(P(mpv,5,3)*0.5)+(P(mpv,5,6)*0.5)

	par.vec[60] = (P(mpv,6,3)*1)+(P(mpv,6,6)*1)+(P(mpv,6,2)*0.5)+(P(mpv,6,5)*0.5)
	
  M.FC = matrix(par.vec, stages, stages, byrow=T)
  Sum.F[n-1,c] = sum(M.FC)
	################################################
	F.vec = M.FC%*%SI.popn.vec
	Fecund.Mat[n-1,] = F.vec
	popn.Mat[n,] = SI.popn.vec+F.vec
	total.pop[n]= sum(popn.Mat[n,])
	
  count=count+1
  Totals.Mat[count,1]= Beta
  Totals.Mat[count,2]= n
  Totals.Mat[count,3]= sum(popn.Mat[n,])
  Totals.Mat[count,4:15]= popn.Mat[n,]
    }
}
Totals.Mat = round(Totals.Mat,3); Totals.Mat
############################
############################
####### END PROGRAM ########
############################
############################