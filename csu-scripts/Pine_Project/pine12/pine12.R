#########################################
#         12 CLASS INFECTION MODEL      #
#         without Genetics 12x12        #
#########################################
#         Stu Field                     #
#         Department of Biology         #
#         Colorado State University     #
#         Fort Collins, CO              #
#         80523-1878                    #
#         sgf@colostate.edu             #
#########################################
#         June 2, 2011                  #
###########################################################################
pine12 <- function(Gen = 200,                                             #
     x1 = c(62580, 38, 79, 65, 91, 353, 0,0,0,0,0,0),                     #
     #x1 = c(35504, 22, 45, 36, 51, 201, 0,0,0,0,0,0), # LAIb=2           #
     Beta = 0.044,                                                        #
     LAIb = 0,                                                            #
     r.site = 1,                                                          #
     delta = 0.15,                                                        #
     C.f = 1/8,                                                           #
     rho = 0.1,                                                           #
     Cmax = 7.5,                                                          #
     SpC = 3.7,                                                           #
     S.cone = 46,                                                         #
     P.find = 0.8,                                                        #
     nBirds = 3,                                                          #
     P.cons = 0.3,                                                        #
     alpha1 = 0.456,                                                      #
     alpha2 = 0.0736,                                                     #
     alpha3 = 2.070,                                                      #
     c2 = 0.99,                                                           #
     c3 = 0.87,                                                           #
     dbh.v = c(0, 0, 0, 2.05, 12.5, 37),                                  #
     M = c(1, 0.152, 0.105, 0.02, 0.015), m6= 0.005,                      #
     R = c(1, 4, 16, 20, 50, 0),                                          #
     plot = FALSE,                                                        #
     csv = FALSE,                                                         #
     csvname,                                                            #
     out = TRUE) {                                                        #
###########################################################################
### State.vector = c(SD,SD1,SD2,SP,YA,MA,SDi,SD1i,SD2i,SPi,YAi,MAi)
### Gen = number of generations to run the model; default = 100
### Beta for the Adult stage (Range = 0.016 - 0.14); default = 0.044
### plot = plots or no plots; default = TRUE
### out = display model output values; default = TRUE
#####################################################
######## Create storage matrices & vectors ##########
#####################################################
stages <- length(x1)
popn.Mat <- matrix(0, Gen, stages, ### overall storage of population projection
               dimnames=list(paste("Gen",1:Gen),paste("Stage",1:stages)))

popn.Mat[1,]= x1							### first row is the initial population
Pop.Total = sum(x1[-1])	  				### Remove seeds from total population
LAI.v = NA									### storage vector for LAI
Prev.v = sum(x1[8:12])/(sum(x1[-1]))	### storage vector for prevalence
r.cache.v = NA    							### storage vector for r.cache
Cones.v = NA								### storage vector for Cones per tree
SR.v = NA									### storage vector for Seedling Recruitment

########################################
#### Calculate Vital Rates #############
#### using vitals() function ###########
########################################
M <- c(M,m6)
S.par <- vitals(R, M)["S",]
T.par <- vitals(R, M)["T",]

########################################
#### Set linear Parameters #############
########################################
### Leaf Area Calculation ##############
########################################
LA.v <- alpha2 * (dbh.v^alpha3)
LA.v[3]= alpha1					### Don't forget the sedondary seedlings

##########################
#### Beta parameters #####
##########################
B.par <- c(0, rep(Beta,5))			### Beta is constant for all classes; Used to make Beta Matrix (BM)
#B.par = (LA.v/LA.v) * Beta 		### This is if Beta is class-dependent

##########################
#### Cost parameters #####
##########################
C.par <- 1 - exp(-delta*(dbh.v))		# Cost of infection to survivorship
C.par[2]= 1 - c2
C.par[3]= 1 - c3
C.par <- c(rep(1,6), C.par)			# Make into 12 entry matrix, 1s for uninfected classes

####################################
#### Set up projection matrix ######
######## Linear Map Matrix #########
####################################
# Survivorship Matrix
S <- diag(S.par) + R.diag(T.par[1:5],-1)

# Cost Matrix
C <- diag(C.par)

# Beta Matrix
B = diag(B.par)

# 6x6 Identity
I6 <- diag(6)

### Combine sub-matrices using MatComb function
SM <- BlockMat(list(S,zeros(6), zeros(6),S), b=2) %*% C

# Combine sub-matrices using MatComb function
BM <- BlockMat(list(I6-B,zeros(6), B,I6), b=2)

#############################################################
############ Loop over generations ##########################
#############################################################
for (n in 2:Gen) {
######################################
### Reset for new iteration ##########
######################################
	f1.vec = rep(0,stages)
	f2.vec = rep(0,stages)
	x.vec = popn.Mat[n-1,]
	################################
	##### LAI.x Calculation ########
	################################
	### Projected LAI of last year's popn (x.vec)
	### 1 ha = 10000 m^2
	LAI.x = sum(LA.v * x.vec) / 10000 + LAIb	### Additiion of background LAI (sgf 12/16)

	#####################
	### Determine f[2]:
	#####################
	SpB <- x.vec[1] / nBirds
	r.cache <- 0.73 / (1 + exp((31000 - SpB)/3000)) + 0.27
	r.cache.v[n] = r.cache					### Store & track r.cache
	r.ALs <- 1 / (1 + exp(2*(LAI.x - 3)))
	
	#############
	### Seedling Recruitment (proportion seeds becoming seedlings)
	#############
	SR <- (((1 - P.find) * (1 - P.cons)) / SpC) * r.cache * r.ALs * r.site   # r.site (sgf added 12/16)
	SR.v[n] = SR							### Store & track SR for plotting
	f2.vec[2] <- x.vec[1] * SR				### Determine number of seedlings f_2
	x.vec <- x.vec + f2.vec				### Add seedlings to population
	
	############################
	### Survive & transition ###
	############################
	y.vec <- SM %*% x.vec					### Matrix multiplication of population trajectory

	################################
	##### LAI.y Calculation ########
	################################
	LAI.y <- sum(LA.v * y.vec) / 10000 + LAIb	### LAI of this year's popn (sgf 12/16)
	LAI.v[n] = LAI.y							### Store and track of LAI over time

	#####################
	### Determine f[1]:
	#####################
	r.cones <- (0.5/(1 + exp(5*(LAI.y - 2.25))) + 0.5)
	C.tree <- r.cones * Cmax
	Cones.v[n] = C.tree
	f1.vec[1] <- (S.cone * C.tree) * (rho*y.vec[5] + y.vec[6] + C.f * (rho*y.vec[11] + y.vec[12]))
	
	######################################################################
	### add f_1(y.vec) the nonlinear function to y.vec & infect with B ###
	######################################################################
	y.vec <- y.vec + f1.vec			### Add seeds to population; no seed bank, thus S[1,1]=0 & y.vec is 0 until here
	popn.Mat[n,] <- BM %*% y.vec		### Infection process to population in Fall

###############################################
### Calculate some variables to follow ########
###############################################
	Pop.Total[n] = sum(popn.Mat[n,-1])			               ### Remove seedlings from population count
	Prev.v[n] = sum(popn.Mat[n,8:12])/(sum(popn.Mat[n,-1]))     ### Calculate prevalence of rust in popn excluding seeds

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
     C.f = C.f, 
     rho = rho,
     Cmax = Cmax, 
     SpC = SpC, 
     S.cone = S.cone,
     P.find = P.find, 
     nBirds = nBirds, 
     P.cons = P.cons,
     alpha1 = alpha1, 
     alpha2 = alpha2,
     alpha3 = alpha3,
     c2 = c2, 
     c3 = c3,
     plot = plot,
     out = out)
)

theta2 <- as.data.frame(rbind(
   dbh.v = dbh.v, M = M, R = R)
)
colnames(theta2) <- paste("Stage",1:ncol(theta2))

#############################
### Post-process variables
#############################
Dead <- dead.trees(popn.Mat, s=S.par, t=T.par, cc=C.par)   # calculate dead trees per class
Groups <- group.class(popn.Mat)		# group projection by susc & inf classes
PrevGroup <- prev.class(popn.Mat,Prev.v)
Lambda <- LambdaGrow(Pop.Total)

#########################################
### Convert Solutions & Export to CSV ###
#########################################
if (csv) {
   rownames(popn.Mat) <- paste("t", 1:Gen, sep="")
   colnames(popn.Mat) <- paste("Class_", 1:stages, sep="")
   write.csv(popn.Mat, file= paste(csvname,"_FullSoln_",Sys.Date(),".csv", sep=""))
}

#################################################
#################################################
####### Plot variables of interest ##############
#################################################
#################################################
if (plot) {
   par(mfrow=c(3,3))
   plot(1:Gen, Pop.Total, type= "l", 
        xlim = c(0,Gen), xlab= "Years", 
        main = "Total Population", 
        ylab = "Total Whitebark Population: Class 2-12", 
        col = "navy", lwd= 1.5); grid()
   plot(1:Gen, popn.Mat[,1], type= "l", 
        xlim = c(0,Gen), xlab= "Years", 
        main = "Seed Population", ylab = "Total Seeds", 
        col = "darkgreen", lwd= 1.5, lty= 1); grid()
   plot(1:Gen, PrevGroup[,"total"], type= "l", 
        col = 'dark red', xlab= "Years", 
        main = "Rust Prevalence", 
        ylab = "Proportion infected individuals", lwd= 1.5); grid()

   for (p in 2:stages) {
 	  if (p==2) {   plot(1:Gen, popn.Mat[,p], type= "l", 
 	  	         ylim = c(0,max(popn.Mat[,2:stages])), 
 	  	         xlim = c(0,Gen), xlab = "Years", 
 	  	         main = "Susceptible Population", 
 	  	         ylab = "Whitebark Population by Class", col= p, lwd= 1.5); grid()
  	  legend("topright", legend= c(2:6), 
  	         lty = c(rep(1,5)), col= c(2:6), 
  	         lwd = 1.5, cex= 0.4, bg= "gray95")
  	  }
      if (p >= 3 && p <= 6) lines(1:Gen, popn.Mat[,p], type= "l", 
                                  xlim= c(0,Gen), col= p, lwd= 1.5)

  	  if (p == 8) {   plot(1:Gen, popn.Mat[,p], type= "l", 
  	  	              ylim = c(0,max(popn.Mat[,9:stages])), 
  	  	              xlim = c(0,Gen), xlab= "Years", 
  	  	              main = "Infected Population", 
  	  	              ylab = "Whitebark Population by Class", 
  	  	              lty = 2, col= p-6, lwd= 1.5); grid()
  	   legend("topright", legend= c(8:12), 
  	          lty = c(rep(2,5)), col= c(2:6), 
  	          lwd = 1.5, cex= 0.4, bg= "gray95")
  	   }
  	   if (p >= 9) lines(1:Gen, popn.Mat[,p], type= "l", 
  	                     xlim= c(0,Gen), lty= 2, col= p-6, lwd= 1.5)
	}

	plot(1:(Gen-1), LAI.v[2:Gen], type = "l", 
	     xlab = "Years", main="Leaf Area Index", 
	     ylab = "LAI.y", lty = 4, lwd = 1.5); grid()
	plot(1:(Gen-1), SR.v[2:Gen], type = "l", 
	     xlim = c(0,Gen), xlab="Years", 
	     main = "Seedling Recruitment", 
	     ylab = "Germination rate", 
	     col = "darkorchid", 
	     lwd = 1.5, lty = 4); grid()
	plot(1:(Gen-1), r.cache.v[2:Gen], type= "l", 
	        xlab = "Years", 
	        ylab = "r.cache", 
	        main = "r.cache", col = "red", 
	        lwd = 1.5, lty = 4); grid()
	plot(1:(Gen-1), Cones.v[2:Gen], type = "l", 
	     xlab = "Years", ylab = "C.tree", 
	     main = "Total Cones per Tree", 
	     sub = "A function of r.cones", 
	     col = "brown", lwd = 1.5, lty = 4); grid()
}
	
	### What pine12 returns
   Out <- list(
        Theta.k = list(pars=theta, pars2=theta2),
        Initial.Pop = x1,
        Pop.Projection = round(popn.Mat,3), 
        Pop.Proj.GroupClass = round(Groups,3), 
        Pop.Totals = round(Pop.Total,2), 
        Prevalence = PrevGroup,
        LAI.y = LAI.v,
        r.cache = r.cache.v,
        Cones.tree = Cones.v,
        Germination.rate = SR.v,
        S.Matrix = SM, B_Matrix = BM,
        y.vec = y.vec,
        f.vec = f1.vec + f2.vec,
        FinalPopVec = popn.Mat[Gen,],
        FinalSum = sum(popn.Mat[Gen,-1]),
        Adults = sum(popn.Mat[Gen,c(6,12)]),
        Dead.Trees = Dead,
        Delta.Grow = Lambda)
   if (out) Out

}	# END OF pine12



####################
### Run pine12 #####
####################
# LAIb=2
#x1 <- c(35504, 22, 45, 36, 51, 201, 0,0,0,0,0,0)
####################
#pine12(Gen=20)
