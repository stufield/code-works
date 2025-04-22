###############################
#    missing data augmentation
###############################


missing.data <- function(rdist=rnorm, n=100, missing=round(n*.2,0), ...){
  x <- rdist(n,...)
  if ( n < missing )
    stop("missing data must be less than n")
  tmp <- sample(1:n, missing, replace=F)
  x[tmp] = NA
  x
}




mcmc.aug.mh <- function (theta=0.5, data, iter=10000, plot=TRUE, burn.in=FALSE, thin=10) {
  chain <- matrix(NA, nrow=as.integer(iter), ncol=length(theta), 
                  dimnames=list(NULL,names(theta)))
  chain[1,] <- theta
  nprop <- 0                            # number of proposals made
  nacpt <- 0                            # number of proposals accepted
  nsave <- 1
  k <- 1
  # calculate P(Data | theta) and P(theta) ; numerator of Bayes rule
  #            Likelihood          Prior
  #########################################################################
  posterior.fun <- function(data, theta){
    if (theta > 1 || theta < 0)
      theta = 0
    likelihood <- dbinom(sum(data), length(data), prob=theta)
    p.theta <- dbeta(theta, shape1=1, shape2=1)
    if (p.theta > 1 || p.theta < 0)
      p.theta = 0
    likelihood * p.theta
  }
  
  # estimate of the Posterior Distn with initial theta
  target <- posterior.fun(data=data, theta=theta)    
  
  print(target)
  while (nsave < iter) {
    theta.star <- runif(1, theta-.2, theta+.2)               # propose new theta
    target.star <- posterior.fun(data=data, theta=theta.star) 
    nprop <- nprop + 1

    # calculate ratio alpha #
    alpha <- target.star / target   # Metropolis' rule (since proposal is symmetric)
    
    # accept if #
    if ( (alpha >= 1) || (runif(1) < alpha) ) {
      theta <- theta.star
      target <- target.star
      nacpt <- nacpt + 1
    }
    k <- k + 1
    if (k%%thin == 0) {
      nsave <- nsave + 1
      chain[nsave,] <- theta
      cat("Iteration...", k, "\n", sep="")
    }
  }
  
  if (burn.in)
    chain <- tail(chain, ceiling(iter*.9))

  cat("Acceptance fraction =", nacpt/nprop, "\n")

  #    plot   #
  if (plot){ 
    ylab <- bquote("Posterior [P(data|" * theta * ")]")
    hist(chain[,1], col=8, freq=FALSE, breaks=150, border=NA,
         ylab=ylab, xlab=bquote(theta), main="Histogram of Theta")
    bayes.est <- apply(chain, 2, mean)
    ci95 <- apply(chain, 2, quantile, prob=c(0.025, 0.975))
    abline(v=bayes.est, col="darkred", lwd=1)
    abline(v=ci95, col="darkred", lwd=2, lty=2)
  } 

  list(MCMC= mcmc(data=chain, thin=thin),            # store the result in a CODA object
       BayesEst= bayes.est, 
       CI95= ci95)
}



