########################################
###	Mike Antolin and Stu Field #########
###	Department of Biology ##############
###	Colorado State University ##########
###	Fort Collins, CO 80523-1878 ########
###	antolin@colostate.edu ##############
###	sgf@colostate.edu ##################
########################################
rm(list=ls()) ##########################
########################################
### Tree Characteristics Parameters ####
####### and data storage arrays ########
########################################
dbh.v <- c(2.05, 12.5, 37)   # Diameter @ breast height for 3 classes "SP", "YA", "MA"
delta <- seq(0, 0.25, by=0.0025)
L = length(delta)
sel.coeff <- matrix(NA, nrow=101, ncol=3, dimnames=list(NULL,c("SP","YA","MA")))
#########################################################
#########################################################
#### Relative fitness C.par with partial dominance is 
#### C.par[i] = w[i] = 1 - exp(-delta[j]*(dbh.v))*h
#########################################################
################################################
###### Loop over to selection coefficient ######
###### and plot slection coefficients ##########
################################################
for (i in 1:L) {
	sel.coeff[i,] <- exp(-delta[i]*(dbh.v))
}

costs <- as.data.frame(cbind(1:L, delta, sel.coeff)); costs
fitness <- as.data.frame(cbind(1:L, delta,1-sel.coeff)); fitness
############
# Plotting
############
attach(costs)
par(mfrow=c(2,3))
plot(delta,SP,ylim=c(0,1),bty="n",ylab="Selection Against Infected Saplings",xlab="Delta", main="Saplings")
plot(delta,YA,ylim=c(0,1),bty="n",ylab="Selection Against Infected Young Adults",xlab="Delta", main="Young Adults")
plot(delta,MA,ylim=c(0,1),bty="n",ylab="Selection Against Infected Mature Adults",xlab="Delta", main="Mature Adults")
detach(costs)
attach(fitness)
plot(delta,SP,ylim=c(0,1),bty="n",ylab="Relative Fitness of Infected Saplings",xlab="Delta", main="Saplings")
plot(delta,YA,ylim=c(0,1),bty="n",ylab="Relative Fitness of Infected Young Adults",xlab="Delta", main="Young Adults")
plot(delta,MA,ylim=c(0,1),bty="n",ylab="Relative Fitness of Infected Mature Adults",xlab="Delta", main="Mature Adults")
detach(fitness)

#######################################
#### Fitness is by partial dominance  
#### C.par[j,] <- 1 - exp(-delta[j]*(dbh.v)) * h
#######################################
h <- seq(0, 1, by=0.1)
Lh <- length(h)
delta2 <- seq(0, 0.25, by=0.0025)   # can now change this to whatever increment you want
Ld <- length(delta2)
out <- matrix(NA, nrow=Lh*Ld, ncol=6, dimnames=list(NULL,c("Loop","Delta","Dominance","w_SP","w_YA","w_MA")))
################################
### Loop over to values of h ###
################################
for (k in 1:Lh) {
#################################
###### Loop over cost values ####
#################################
	for (j in 1:Ld) {
		out[(Ld*(k-1))+j,1]= (Ld*(k-1))+j
		out[(Ld*(k-1))+j,2]= delta2[j]
		out[(Ld*(k-1))+j,3]= h[k]
		out[(Ld*(k-1))+j,4:6]= 1 - exp(-delta2[j]*(dbh.v)) * h[k]
	}	
}
out <- as.data.frame(out)
out

######################################################################
#### 3-D plots of Fitness as Functions of Delta and Relative Fitness
######################################################################
#X11() # For windows
require(akima)
require(graphics)
attach(out)
par(mfrow=c(1,3))

###  Saplings
zz_sp<-interp(Dominance, Delta, w_SP)
persp(zz_sp,ylab="Delta",xlab="Dominance (h)",zlab="Relative fitness of Rr genotype",zlim=c(0,1),xrev, theta=40,phi=20,col="navy",ticktype = "detailed",cex.lab=1.25,cex.axis=1.25,tcl=0.01)
mtext(side=3,"Saplings",font=4,cex=1.25)

###  Young Adults
zz_ya<-interp(Dominance, Delta, w_YA)
persp(zz_ya,ylab="Delta",xlab="Dominance (h)",zlab="Relative fitness of Rr genotype",zlim=c(0,1), theta=40,phi=20,col="darkred",ticktype = "detailed",cex.lab=1.25,cex.axis=1.25,tcl=0.01)
mtext(side=3,"Young Adults",font=4,cex=1.25)

###  Mature Adults
zz_ma<-interp(Dominance, Delta, w_MA)
persp(zz_ma,ylab="Delta",xlab="Dominance (h)",zlab="Relative fitness of Rr genotype",zlim=c(0,1), theta=40,phi=20,col="darkolivegreen4",ticktype = "detailed",cex.lab=1.25,cex.axis=1.25,tcl=0.01)
mtext(side=3,"Mature Adults",font=4,cex=1.25)

detach(out)
