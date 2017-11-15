########################
#		clear the deck
####################

######################################
##### Example of Genetic Drift #######
######################################

founderfun <- function(xvec, Gen) {
#
	freqz <- function(x) {
		A <- (2*(x[1]) + x[2]) / (sum(x)*2)
		a <- (2*(x[3]) + x[2]) / (sum(x)*2)
		c(A,a)
	}
#

	xvecM <- matrix(0, Gen, length(xvec), dimnames=list(NULL,c("AA","Aa","aa")))
	xvecM[1,]= xvec
	pvec <- rep(0,Gen); pvec[1] = freqz(xvecM[1,])[1]
	qvec <- rep(0,Gen); qvec[1] = freqz(xvecM[1,])[2]

	for (i in 2:Gen) {
		c = runif(1,0,1)
		P.AA = xvecM[i-1,1]/sum(xvecM[i-1,])
		P.Aa = xvecM[i-1,2]/sum(xvecM[i-1,])
		P.aa = xvecM[i-1,3]/sum(xvecM[i-1,])
		if (c < P.AA) {
			xvecM[i,1]= xvecM[i-1,1] + 1
			xvecM[i,2]= xvecM[i-1,2] + 0
			xvecM[i,3]= xvecM[i-1,3] + 0
		}
		if (c < (P.AA + P.Aa) && c > P.AA) {
			xvecM[i,1]= xvecM[i-1,1] + 0
			xvecM[i,2]= xvecM[i-1,2] + 1
			xvecM[i,3]= xvecM[i-1,3] + 0
		}
		if (c > (P.AA + P.Aa)) {
			xvecM[i,1]= xvecM[i-1,1] + 0
			xvecM[i,2]= xvecM[i-1,2] + 0
			xvecM[i,3]= xvecM[i-1,3] + 1
		}
		pvec[i] <- freqz(xvecM[i,])[1]
		qvec[i] <- freqz(xvecM[i,])[2]
	}

	Out <- list(xvecM, pvec, qvec) %names% c("PopMat", "p", "q")
	Out

}

Gen = 1000
sim = 50
pMat <- matrix(0,Gen,sim)

for (r in 1:sim) {
	pMat[,r] <- founderfun(c(1,1,1),Gen)$p
}

plot(1:Gen, pMat[,1], 'l', ylab="p", xlab="time", ylim=c(0,1), main="Allele Frequency")
for (i in 2:sim) lines(1:Gen, pMat[,i])


