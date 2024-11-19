###################################
# Maximum Likelihood Estimation
###################################
loglik <- function(x,param) {
  N <- length(x)
  mu <- param[1]
  sigma <- param[2]
  ll <- -0.5*N*log(2*pi) - N*log(sigma) - sum(0.5*(x - mu)^2/sigma^2)
  ll
}

my.confint <- function(boot, x, start, FUN, method) {
  est <- coef(maxLik(FUN, x=x, start= start, method= method))
  cat("Profiling...", "\n")
  boot.est <- sapply(1:boot, function(i) {
               dat <- sample(x, length(x), replace=TRUE)
               coef(maxLik(FUN, x=dat, start= start, method= method))
      } )
  par1 <- quantile(boot.est[1,], prob= c(0.025,0.975))
  par2 <- quantile(boot.est[2,], prob= c(0.025,0.975))
  hist(x, col=8, prob=TRUE, ylim=range(density(x)$y))
  lines(density(x), col="navy")
  abline(v=est[1], lty=2, col=2)
  abline(v=par1, lty=2, col=5)
  cbind(est,rbind(par1,par2))
}

#dat.v <- rnorm(1000, mean=1, sd=2.5)
#my.confint(boot=500, x=dat.v, start=c(0,1), FUN=loglik, method="NM")

