###############################################
#######   36 CLASS GENETICS MODEL  ############
#######   with Infection           ############
#######   with Genetics 36x36      ############
#########################################
#         Stu Field                     #
#         Department of Biology         #
#         Colorado State University     #
#         Fort Collins, CO              #
#         80523-1878                    #
#         sgf@colostate.edu             #
#########################################
#         June 2, 2011                 #
##########################################################################
pine36 <- function(                                                      #
     Gen = 50,                                                           # 
     x1 = cbind(c(10000,100,87,82,123,244,   0,0,0,0,0,0),               #
                c(10000,100,87,82,123,244,   0,0,0,0,0,0) * 2,           #
                c(10000,100,87,82,123,244,   0,0,0,0,0,0)),              #
     M = c(1, 0.152, 0.105, 0.02, 0.015), m6= 0.005,                     #   
     R = c(1, 4, 16, 20, 50, 0),                                         #
     Beta = 0.044,                                                       #
     dbh.v = c(0, 0, 0, 2.05, 12.5, 37),                                 #
     #s1 = 0.01, s2= 0.13,                                               #
     s1 = 0.99,                                                          #
     s2 = 0.87,                                                          #
     delta = 0.15,                                                       #
     alpha1 = 0.456,                                                     #
     alpha2 = 0.0736,                                                    #
     alpha3 = 2.070,                                                     #
     LAIb = 0,                                                           #
     Cmax = 7.5,                                                         #
     S.cone = 46,                                                        # 
     P.find = 0.8,                                                       #
     P.cons = 0.3,                                                       #
     SpC = 3.7,                                                          #
     cf = 0.875,                                                         #
     nBirds = 3,                                                         #
     r.site = 1,                                                         #
     h = 0,                                                              #
     rho = 0.1,                                                          #
     qpollen = FALSE,                                                    #
     NoSeln = FALSE,                                                     #
     plot = FALSE,                                                       #
     csv = FALSE,                                                        #
     csvname,                                                            #
     out = TRUE) {                                                       #
##########################################################################
### Gen = number of generations to run the model; default = 100          #
### Beta default = 0.044                                                 #
### plot = plots or no plots; default = TRUE                             #
### out = display model output values; default = TRUE                    #
### qpollen = pollen cloud - frequency of the recessive allele           #
### Cost parameters = s1, s2, cf (sd2, sd3, cost to fecundity)           #
### NoSeln = default is FALSE; for testing H-W                           #
##########################################################################
### Storage array & vectors for results #
#########################################
stages <- length(x1)
classes <- c("C1","C2","C3","C4","C5","C6","C1i","C2i","C3i","C4i","C5i","C6i")
I.Gtype <- apply(x1[-1,],2,sum)
### overall storage of population projection ###
Popn <- array(0, dim = c(stages/3,3,Gen),
              dimnames = list(classes, names(I.Gtype), NULL))
Popn[,,1]= x1              # first plane is the initial population

### storage of intermediate popn following survival ### not used
Popn.S <- array(0, dim= dim(Popn), dimnames= dimnames(Popn))

### Setup storage vectors for variables of interest ###
freq.R <- freqz(I.Gtype)[1]
freq.r <- freqz(I.Gtype)[2]
Gtype.Sum <- matrix(NA, ncol= 3, nrow= Gen, dimnames=list(NULL,names(I.Gtype)))
Gtype.Sum[1,] <- I.Gtype                  ### sum pop by genotype
Pop.Total <- sum(x1[-1,])				  ### Initial PopSize (remove seeds)
Prev.v <- sum(x1[8:12,]) / (sum(x1[-1,])) ### Initial prevalence
SR.v = NA                                 ### storage vector Seedling Recruitment
LAI.v = NA                                ### storage vector LAI
r.cache.v = NA                            ### storage vector r.cache


########################################
#### Calculate Vital Rates #############
#### using vitals() function ###########
########################################
M <- c(M, m6)
S.par <- vitals(R, M)["S",]
T.par <- vitals(R, M)["T",]

###################################################
#### Set linear Parameters ########################
###################################################
### Leaf Area Calculation ##############
########################################
LA.v <- alpha2 * (dbh.v^alpha3)
LA.v[3]= alpha1					### Don't forget the sedondary seedlings

################
# Fitness pars #
################
### w[1] = R1R1
### w[2] = R1R2
### w[3] = R2R2
###########################
### Viability Cost - Survivorship & Transition
#s.vec <- c(1, s1, s2, exp(-delta*(dbh.v[4:6])) )
s.vec <- c(0, s1, s2, exp(-delta*(dbh.v[4:6])) ) # Doesn't matter which; multiplied by zero because seeds don't survive the matrix multiplication.

I6 <- diag(6)
w.hs <- diag(1 - s.vec*h)
w.s <- diag(1 - s.vec)

##########################
#### Beta parameters #####
##########################
B.vec <- c(0, rep(Beta,5))	 ### Beta is constant for all classes

####################################
#### Set up projection matrix ######
######## Linear Map Matrix #########
####################################
# Survivorship Matrix ##############
S <- diag(S.par) + R.diag(T.par[1:5],-1)

############# Fitness Matrix ##########################
W.AA <- BlockMat(list(I6,zeros(6), zeros(6),I6), b=2)
W.Aa <- BlockMat(list(I6,zeros(6), zeros(6),w.hs), b=2)
W.aa <- BlockMat(list(I6,zeros(6), zeros(6),w.s), b=2)

# Zeros Matrix for filling #
Z <- zeros(stages/3)

blocks <- list( W.AA, Z, Z,
				Z, W.Aa, Z,
				Z, Z, W.aa)
				
W.s <- BlockMat(blocks, b=3)
if (NoSeln) W.s <- diag(36)   ### No selection

# Beta Matrix
B <- diag(B.vec)

# Combine sub-matrices for survival
SM12 <- BlockMat(list(S,zeros(6), zeros(6),S), b=2)
SM36 <- BlockMat(list(SM12,Z,Z, Z,SM12,Z, Z,Z,SM12), b=3)

# Combine sub-matrices for infection
BM12 <- BlockMat(list(I6-B,zeros(6), B,I6), b=2)
BM36 <- BlockMat(list(BM12,Z,Z, Z,BM12,Z, Z,Z,BM12), b=3)

#####################################################
########## Now the Loop over generations! ###########
#####################################################
for (n in 2:Gen) {
	###################################
	### Reset x.vec for new iteration
	###################################
	x.vec <- Popn[,,n-1]
	
	################################
	##### LAI.x Calculation ########
	################################
	### Projected LAI of last year's popn (x) (1 ha = 10000 m^2)
	LAI.x <- sum(LA.v * as.vector(x.vec)) / 10000 + LAIb

	############################
	### Determine New Seedlings:
	############################
	SpB <- sum(x.vec[1,]) / nBirds
	r.cache <- 0.73 / (1 + exp((31000 - SpB)/3000)) + 0.27
	r.cache.v[n] = r.cache					### Store & track r.cache
	r.ALs <- 1 / (1 + exp(2*(LAI.x - 3)))

	### Seedling transition (proportion seeds becoming seedlings)
	SR <- (((1 - P.find) * (1 - P.cons)) / SpC) * r.cache * r.ALs * r.site
	SR.v[n] = SR							### Store SR for plotting
	New.Seedlings <- Popn[1,,n-1] * SR		### Determine number of seedlings

	### Add seedlings to population
	x.vec[2,] <- x.vec[2,] + New.Seedlings

	#########################
	### Survive & transition
	#########################
	### POST-multiplication of cost matrix W.s
	y.vec <- (SM36 %*% W.s) %*% as.vector(x.vec)
	
	Popn.S[,,n]= y.vec # Matrix stores survivorship vectors at each iteration

	################################
	##### LAI.y Calculation ########
	################################
	LAI.y <- sum(LA.v * y.vec) / 10000 + LAIb	### LAI of this year's popn
	LAI.v[n]= LAI.y					   ### Store and track of LAI over time
	
	########################
	### Determine New Seeds:
	########################
	r.cones <- (0.5/(1 + exp(5*(LAI.y - 2.25))) + 0.5)
	C.tree <- r.cones * Cmax

	#################################
	### Set up matrix of Fecundities
	#################################
	### Fecundity Matrix ############
	F.mat <- matrix(0, nrow= stages/3, ncol= 3)
	
	F.mat[c(5,11),]= rho * (S.cone * C.tree)
	F.mat[c(6,12),]= S.cone * C.tree

	### Fitness Matrix ###################
	W <- matrix(0, nrow=stages/3, ncol=3)
	W[5:6,]= 1 									# no cost to uninfected genotypes
	W[11:12,]= rep(c(1, 1-h*cf, 1-cf), each=2)  # fitness of infected gtypes
	if (NoSeln) W[11:12,] <- 1                  # No selection
	
	### Hadamard product #######
	#W.f <- W * F.mat
	
	###########################################
	### convenient matrix Y of the 
	### surviving population; cols = genotypes
	###########################################
	Y <- matrix(y.vec, ncol=3)
	
	#######################
	####### Mating ########
	#######################
	### This uses Jesse's simplified code from FEScUE
	### assumes explicitely random mating
	### outputs matrix with next year's seeds
	###########################################
	if (!qpollen) Seeds <- mating36(Y, W, F.mat)
	if (qpollen>0) Seeds <- mating36p(Y, W, F.mat, qpollen) # fixed pollen cloud
	
	#########################################
	#### Add newborns to population  ########
	#########################################
	y.vec <- as.vector(Y + Seeds)		### Add seeds to population in the Fall

	#############################
	### Infect the population ###
	#############################
	Popn[,,n]= BM36 %*% y.vec			### Infect in Fall (last operation)

	###################################################
	########### OUTPUTS OF INTEREST ###################
	###################################################
	Gtypes <- apply(Popn[-1,,n], 2, sum) 	### sum the cols (Gtypes); 
	                                        ### put [-1,x,n] to remove seeds
	Gtype.Sum[n,] <- Gtypes
	freq.R[n] <- freqz(Gtypes)[1]			### Calc p
	freq.r[n] <- freqz(Gtypes)[2]			### Calc q
	Pop.Total[n]= sum(Popn[-1,,n])			### remove seeds
	Prev.v[n]= ifelse(h==0, 
	                  sum(Popn[8:12,3,n])/(sum(Popn[-1,,n])), 
	                  sum(Popn[8:12,2:3,n])/(sum(Popn[-1,,n])))
}  # END OF LOOP


############################
### Output of parameters ###
############################
theta <- data.frame(ParValue=rbind(
     Gen = Gen,
     Transmission = Beta,
     LAIb = LAIb, 
     r.site = r.site,
     delta = delta, 
     cf = cf,
     Cmax = Cmax, 
     SpC = SpC, 
     S.cone = S.cone,
     P.find = P.find, 
     nBirds = nBirds, 
     P.cons = P.cons,
     alpha1 = alpha1, 
     alpha2 = alpha2,
     alpha3 = alpha3,
     s1 = s1, 
     s2 = s2,
     h = h,
     rho = rho,
     qpollen = qpollen,
     NoSeln = NoSeln,
     plot = plot,
     csv = csv,
     out = out)
)

theta2 <- as.data.frame(rbind(
   dbh.v = dbh.v, M = M, R = R)
)
colnames(theta2) <- paste("Stage",1:ncol(theta2))


#############################
### POST-PROCESS ANALYSIS ###
#############################
# function for calculating # dead trees (not used in pine36)
# DeadPine <- DeadTrees36(Popn, B=Beta, w=w.s)
########################
# Post-process Lambda (Proportional Growth)
Lambda <- LambdaGrow(Pop.Total)               # Total population Lambda
Lambda2 <- apply(Gtype.Sum, 2, LambdaGrow)    # Lambda by genotype


#########################################
### Convert Solutions & Export to CSV ###
#########################################
if (csv) {
   full2x2 <- do.call(rbind, 
                      lapply(1:Gen, function(i) rbind(as.vector(Popn[,,i]))))
   rownames(full2x2) <- paste("t", 1:Gen, sep="")
   colnames(full2x2) <- paste("Class_", 1:stages, sep="")
   write.csv(full2x2, file= paste(csvname,"_FullSoln_",Sys.Date(),".csv", sep=""))
}


#################################################
#################################################
####### Plot variables of interest ##############
#################################################
#################################################
if (plot)   {
   par(mfrow=c(2,3))
   plot(1:Gen, Pop.Total, type="l", 
      xlim=c(0,Gen), 
      xlab="Years", 
      main="Total Population", 
      ylab="Total Whitebark Population (no seeds)", 
      col='navy', lwd=2); grid()
   plot(1:Gen, freq.r, 
      xlab="Years", 
      main="R2 Allele", 
      ylab="q", 
      col="darkgreen"); grid()
   plot(1:Gen, Prev.v, type="l", 
      col='dark red', 
      lwd=2, 
      xlab="Years", 
      main="Rust Prevalence", 
      ylab="Proportion infected individuals"); grid()
   plot(1:(Gen-1), LAI.v[2:Gen], 
      xlab="Years", 
      main="Post-Survival Leaf Area Index", 
      ylab="LAI.y"); grid()
   plot(1:(Gen-1), SR.v[2:Gen], 
      xlab="Years", 
      main="Germination Rate", 
      ylab="SR"); grid()
   plot(1:(Gen-1), r.cache.v[2:Gen], 
      xlab="Years", 
      main="r.cache", 
      ylab="r.cache"); grid()
}
############################################################################
   Out <- list(                        # Create output list
      Theta.k = list(pars= theta, pars2= theta2), # pars
      Initial.Pop = x1,                # initial popn
      Final.Popn = Popn[,,Gen],        # Final Solution
      Gtypes = Gtype.Sum,              # Solutions summed by Genotype
      Final.Sum = Pop.Total[Gen],      # Total Popn
      Pop.Totals = Pop.Total,          # Total Popn vector over time
      R2 = unname(freq.r),        # Frequency of 'q' allele final time (t)
      Germ.rate = SR.v,                # Vector of Germination rates
      r.cones = r.cones,               # r.cones in final time (t)
      Final.Seeds = Seeds,             # Seeds produced in final time (t)
      LAIy = LAI.v,                    # LAI.y in final time (t)
      Prevalence = Prev.v,             # Vector of disease prevalence
      #Dead.Pines = DeadPine,          # Not used
      Delta.Grow = Lambda,             # Lambda = x(t) / x(t-1)
      Delta.Grow.Gtype = Lambda2,      # Lambda by Genotype
      FullSolution = Popn)             # ALL Solutions by Generation
##############################################################################   
   if (out) Out                        # print 'Out'

}  # END OF pine36

####################
### Run pine36 #####
####################
pine36(Gen=10, csv=T, csvname="Run1", plot=FALSE, out=TRUE)

