############################################
######### SIMPLE FUNCTION EQUATIONS ########
############################################
#############################################
### Get the right Exponential function
#############################################
par(mfrow=c(2,2))
a= -10; b= 1
curve(b*(exp(x*a)), from=0, to=10)    ### I'll just try this one for now
curve((b*exp(-x*a)), from=0, to=10)
curve((1-(b*exp(-x*a))), from=0, to=10)
curve((1-(b*exp(x*a))), from=0, to=10)


##################################################################
################# General Sigmoid Function #######################
### K = asymptote; c = inflection point; m = slope of line at c ##
##################################################################
K = 0.5; c = 24; m = 5.5; a=1              ### From Jon Coop's data on Age vs Fecundity field data
curve(K/(1+exp((-x+c)/m)), from = 0, to = 75)

###################################################
### r.cache factor function  (Keane et al.1996)
###################################################
x = c(0.00001, seq(120, 600, 60))*60
y = c(0.24, 0.25, 0.255, 0.27, 0.29, 0.33, 0.4, 0.48, 0.6, 0.99)
K=0.73; c=31000; m=3000; Smax= 600*60
par(mfrow=c(1,2))
plot(x,y, ylim=c(0,1.1), xlim=c(0,45000), ylab= 'r.cache', xlab='Seeds/Bird')
curve(K/(1+exp((-x+c)/m)) + 0.27, from = 0, to = 55000, ylim=c(0,1), add=T, col='dark red', lwd=1.5)
curve(exp((x/Smax)^10) - 0.3 * (1 - (x/Smax)) - 0.5, from=0, to=33020, xlim=c(0,55000), ylim=c(0,1), col='navy', lwd=1.5, main='Figure 1B', xlab='Seeds per bird (x1000)', ylab='r.cache', add=T); curve(x/x, from=33020, to=55000, col='navy', lwd=1.5, add=T)

##################################################################
### r.ALs function with continuous approximation
### a+K = max fecundity (upper bound); K = min fecundity (lower bound)
### p = inflection point; m = slope at p
##################################################################
p = 3; m = 2; a = 1; K= 0
curve((a/(1+exp(m*(x-p)))+K), from= 0, to= 10, ylim=c(0,1), col='dark red', lwd=1.5, add=T)


