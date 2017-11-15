###################################
## Solving ODEs with lsoda in R
###################################
require(deSolve)
#####################
## Logistic growth
#####################
ODEfun <- function(t, y, parms) {
   N <- y[1]
   with(as.list(parms), {
     	dN <- r * N * (1 - N/K)
     	list(dN) 
   })
} 

logistic <- as.data.frame(lsoda(c(N=0.1), times=seq(0, 10, by=0.1), func=ODEfun, parms=c(r=0.9, K=5)))
head(logistic)
plot(N ~ time, data=logistic, ylab="N", xlab="time", col="navy")


#############################
### The SI model 
#############################
# assumes births & deaths equal and deaths from disease occur on timescale longer than we're interested in.
tInt    <- seq(0, 25, by=1/2)
pars    <- c(beta=0.75)
Initial <- c(S=4999, I=1)

SIfun <- function(t, y, parms) { 
	S <- y[1] 
	I <- y[2]
	with(as.list(parms), {
		dS   <- -beta * S * (I/(S+I))
		dI   <- beta * S * (I/(S+I))
		ODEs <- c(dS, dI)
		list(ODEs)
	})
} 

SIout <- as.data.frame(lsoda(Initial, times=tInt, func=SIfun, parms=pars))
head(SIout)

par(mfrow=c(1, 2)) 
plot(S ~ time, data=SIout, ylab="Susceptible= blue", xlab="time", col="navy")
points(I ~ time, data=SIout, col="darkred")
plot(I ~ S, data=SIout, ylab="Susceptible", xlab="Infected") 


########################
### SIR model
########################
tInt    <- seq(0, 150, by=1/2) 
pars    <- c(beta=0.9, gamma=0.8) 
initial <- c(S=4999, I=1, R=0) 

SIRfun <- function(t, y, parms) { 
	S <- y[1] 
	I <- y[2]
	R <- y[3]
	with(as.list(parms), {
		dS <- -beta * S * (I/(S+I))
		dI <- beta * S * (I/(S+I)) - (gamma * I)
		dR <- gamma * I
		ODEs <- c(dS, dI, dR) 
		list(ODEs)
	})
} 

SIRout <- as.data.frame(lsoda(initial, times=tInt, parms=pars, SIRfun))
head(SIRout)

par(mfrow=c(2, 2))
plot(S ~ time, data=SIRout, ylab="Susceptible", xlab="time", col="navy")
plot(I ~ time, data=SIRout, ylab="Infected", xlab="time", col="darkred")
plot(R ~ time, data=SIRout, ylab= "Recovered", xlab="time", col="darkgreen")
plot(I ~ S, data=SIRout, ylab="Infected", xlab="Susceptible") 


######################
### Ottar's example 
### SEIR model; from EEID 2007
#################################
tInt       <- seq(0, 10, by=1/120) 
parameters <- c(mu=1/50, N=1, beta=1000, sigma=365/8, gamma=365/5) 
Init       <- c(S=0.06, E=0, I=0.001, R=0.939) 

SEIRmod <- function(t, y, parms) { 
	S <- y[1]
	E <- y[2]
	I <- y[3]
	R <- y[4]
	with(as.list(parms), { 
		dS <- mu * (N - S) - beta * S * I/N 
		dE <- beta * S * I/N - (mu + sigma) * E 
		dI <- sigma * E - (mu + gamma) * I 
		dR <- gamma * I - mu * R 
		ODEs <- c(dS, dE, dI, dR) 
		list(ODEs) 
	}) 
} 

SEIRout <- as.data.frame(lsoda(Init, times= tInt, parms= parameters, SEIRmod))
head(SEIRout)

par(mfrow = c(2, 2)) 
plot(S ~ time, data=SEIRout, ylab="Susceptible", xlab="time")
plot(I ~ time, data=SEIRout, ylab="Infected", xlab="time", col="darkred")
plot(R ~ time, data=SEIRout, ylab="Recovered", xlab="time", col="green")
plot(I ~ S, data=SEIRout, ylab="Infected", xlab="Susceptible")


##############################################
### Exercises Evolutionary response (a) & (b)
###############################################
tInt <- 0:100
pars <- c(r=0.2, aopt=10, E=0.03)

EvolFun <-= function(t, y, parms) {
	x <- y[1] 
	a <- y[2]
	with(as.list(parms), {
		dx <- x * (r - (a - aopt)^2)
		da <- E * (-2 * (a - aopt))
		ODEs <- c(dx, da)
		list(ODEs)
	})
} 

Evol <- as.data.frame(lsoda(c(x= 2, a= 9.15), times= tInt, parms= pars, EvolFun))
head(Evol)

par(mfrow=c(1,2))
plot(x ~ time, data=Evol, ylab="Population Size", xlab="time", col="navy")
plot(a ~ time, data=Evol, ylab="Trait Value (mean activation temp.)", xlab="time", col="darkred")

