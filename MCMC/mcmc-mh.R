##########################
#   Aaron King MCMC Example
##########################
#   Using Metropolis-Hasings Algorithm
##########################################
mcmc.mh <- function (theta, fun, niter, thin = 1) {
  chain <- matrix(
                  nrow=as.integer(niter),
                  ncol=length(theta),
                  dimnames=list(NULL,names(theta))
                  )
  f <- fun(theta)
  chain[1,] <- theta
  k <- 1                                # step in the chain
  nsave <- 1                            # number of values saved so far
  nprop <- 0                            # number of proposals made
  nacpt <- 0                            # number of proposals accepted
  while (nsave < niter) {
    theta.star <- runif(n=1, min= theta-10, max= theta+10) # propose a new parameter
    f.star <- fun(theta.star)
    nprop <- nprop + 1
    alpha <- f.star/f                   # Metropolis' rule (since proposal is symmetric)
    if ( (alpha>=1) || (runif(1)<alpha) ) {
      theta <- theta.star
      f <- f.star
      nacpt <- nacpt+1
    }
    k <- k + 1	
    if (k%%thin == 0) {
      nsave <- nsave + 1
      chain[nsave,] <- theta
    }
  }
  cat("acceptance fraction =", nacpt/nprop, "\n")
  mcmc(data=chain, thin=thin)            # store the result in a CODA object
}
