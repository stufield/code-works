#########################
### STDs & Sexual Seln
### R.J. Knell
########################
beta = 1
alpha = seq(0, 0.8, 0.01)
y = 0.5
c = 2
z = c(0, 1, 2)
b = 0.1
gamma = 0.1 
Out <- matrix(0, length(alpha), length(z))

for (i in 1:length(z)){
Out[,i] <- ((beta*alpha^y)*(c/(1+alpha)^z[i]))/(alpha + b + gamma)
}
plot(alpha, Out[,1], 'l', xlab='Virulence (alpha)', ylab='Basic Reproductive Rate (Ro)', lwd=1.5, col='navy', main='R.J. Knell (1999) - Fig. 1')
lines(alpha, Out[,2], 'l', lwd=1.5, col='dark red')
lines(alpha, Out[,3], 'l', lwd=1.5, col='yellow')
abline(h=1, lty=2)