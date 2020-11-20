###################################################
### Functions for Non-linear Parameters
####################################################
rm(list=ls())
######################################################################
nonlinear <- function(Grid=FALSE,KeanePts=FALSE,Coop=FALSE,Keane=FALSE) {

# Coop = Plot Jon Coop's field data relationship between age vs. fecundity
# Keane = Plot r.ALs relationships from Keane et al. 1996
# KeanePts = Plot the raw points from Keane et al. 1990 paper for r.cache function
# Plots = how many plots to make (4 = all)
# Grid = plot graphs in a 2 graphs/pane window

##################################################################
################# General Sigmoid Function #######################
### K = asymptote; c = inflection point; m = slope of line at c ##
##################################################################
if (Coop) {
K = 0.5; c = 24; m = 5.5; a=1              ### From Jon Coop's data on Age vs Fecundity field data
curve(K/(1+exp((-x+c)/m)), from = 0, to = 75)
}
########################################################
### Make decreasing fecundity as a fuction of density ##
############################################################################
### a + K = max fecundity (upper bound); K = min fecundity (lower bound) 
### b = strength of relationship (initial slope)
############################################################################
### x = LAI in following equations; from Keane et al. (1996) Fire-BGC
if (Keane) {
par(mfrow=c(1,2))
curve(exp(-0.8*x), from= 0, to= 5, col=2, lwd=1.5, xlab = expression(paste(Leaf," ", Area," ", Index," ", (m^2/m^2))), ylab="r.als", main="Relationship between LAI and r.als")
curve(exp(-0.35*(x+0.5)), from= 0, to= 5, col=3, lwd=1.5, add=T) 
curve(1-exp(-0.25*x-0.2), from= 0, to= 5, col=4, lwd=1.5, add=T)
}




#######################
### r.ALs in our Model
#######################
if (Grid) par(mfrow=c(2,2))
p = 3; m = 2; a = 1
curve(a/(1+exp(m*(x-p))), from= 0, to= 7, 
	ylim= c(0,1), 
	xlab = expression(paste(Leaf," ", Area," ", Index," ", (m^2/m^2))),
	ylab= expression(italic(r[ALs])), 
	main= "",
	col='navy', 
	lwd=1.5); box()





#####################################
### r.cache factor function  (Keane et al. 1996)
#####################################
if (KeanePts) {
x = c(0.00001, seq(120, 600, 60))*60/1000
y = c(0.24, 0.25, 0.255, 0.27, 0.29, 0.33, 0.4, 0.48, 0.6, 0.99)
plot(x,y, ylim=c(0,1), xlab='Seeds/Bird')
}






########################
########################
### 58.8 seed/cone
### K + b = Upper bound ### b = Lower bound ### c = point of inflexion ### m = slope at c
K = 0.73; c = 31; m = 3; b = 0.27
curve(K/(1+exp((c-x)/m)) + b, from= 0, to= 55, 
	ylim=c(0,1), 
	col= "navy", lwd=1.5, 
	main= "", 
	xlab= "Seeds per bird (x1000)", 
	ylab=expression(italic(r[cache]))); box()

# Alternative version w/o nBirds
#K = 0.73; c = 93; m = 9; b = 0.27
#curve(K/(1+exp((c-x)/m)) + b, from= 0, to= 150,
#	ylim = c(0,1), 
#	col = "navy", lwd=1.5, 
#	main = "", 
#	xlab = "Seeds/ha (x1000)",
#	#xlab = expression(paste(frac("seeds","hectare"))),
#	ylab = expression(italic(r[cache])))






##################################################################
### a+K = max fecundity (upper bound); K = min fecundity (lower bound)
### p = inflection point; m = slope at p
##################################################################
p = 2.25; m = 5; a = 0.5; K= 0.5
par(mar=c(5,5.5,4,2)+0.1)
curve((a/(1+exp(m*(x-p)))+K), from= 0, to= 5, ylim=c(0.4,1), 
	ylab = expression(paste(italic(r[cones])==frac(0.5,1+italic(exp)(5*(LAI-2.25)))+0.5)),
	main = "", 
	xlab = expression(paste(Leaf," ", Area," ", Index," ", (m^2/m^2))), 
	col='navy', 
	lwd=1.5); box()


#####################################
### Infection cost reduction factor (FireBGC - Keane et al. 1996)
#####################################
par(mar=c(5,5,4,2)+0.1)
delta = c(0.05, 0.10, 0.15, 0.20, 0.25)
curve(1-exp(-delta[3]*x), 
	from= 0, to= 40,
	lwd= 1.5, 
	col= "navy",
	xlab="dbh (cm)",
	ylab= expression(italic(c[i])==1-italic(e)^(-delta*" • "*dbh)),
	main= ""); box()

for (i in c(1,2,4,5)){
curve(1-exp(-delta[i]*x), 
	from= 0, to= 40,
	col= "navy",
	xlab= "",
	ylab= "",
	main= "",
	lwd= 1,
	lty = 2,
	add= T)}
	text(25,0.65, expression(delta==0.05), cex=0.75)
	text(5,0.9, expression(delta==0.25), cex=0.75)

} # END FUNCTION

nonlinear()
