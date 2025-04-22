##########################
# bayesian binomial
# user defined prior (set around 0.5)
###########################
bayes_binomial <- function(n_theta = 3, data = c(rep(1, 3), rep(0, 9))) {

  theta = seq(1 / (n_theta + 1), n_theta / (1 + n_theta), by= 1 / (n_theta + 1))
  p_theta = pmin(theta, 1 - theta)
  p_theta = p_theta / sum(p_theta)
  nheads = sum(data == 1)
  ntails = sum(data == 0)
  #Likelihood = theta^nheads * (1-theta)^ntails
  Likelihood = dbinom(nheads, length(data), theta)
  p.data = sum(Likelihood * p_theta)
  ptheta_given_data = Likelihood * p_theta / p.data   # bayes rule
  # plot #
  par(mfrow=c(1,3))
  plot(theta, p_theta, type="h", col="navy", lwd=5, main="Prior")
  plot(theta, Likelihood, type="h", col="navy", lwd=5, main="Likelihood")
  plot(theta, ptheta_given_data, type="h", col="navy", lwd=5, main="Posterior")
  # lines(theta, ptheta_given_data, type="b", col="darkred", lwd=1)
  list(theta=theta, 
       pData=p_data, 
       Prior=p_theta, 
       Likelihood=Likelihood, 
       Posterior=ptheta_given_data)
}

##########################
# bayesian binomial with beta prior
##########################
bayes.betaPrior.binom <- function(n_theta = 100, priorShape = c(3,3),
                                  data = c(rep(1,11), rep(0,3))) {

  N = length(data)
  nheads = sum(data)
  theta = seq(0.005, 0.995, length=n_theta) 
  p_theta = dbeta(theta, shape1=priorShape[1], shape2=priorShape[2])
  Likelihood = dbinom(nheads, N, theta)
  p.data = sum(Likelihood * p_theta)
  ptheta_given_data = Likelihood * p_theta / p.data   # bayes rule
  postShape = c( priorShape[1] + nheads , priorShape[2] + (N - nheads) )
  #   plotting   #
  par(mfrow = c(1, 3L))
  plot.list = list(p_theta, Likelihood, ptheta_given_data) 
  plot.titles = c("Prior","Likelihood","Posterior") 
  ylabs = list(bquote(p(theta)), bquote( "p(D|" * theta * ")"), bquote( "p(" * theta * "|D)")) 
  sapply(1:3, function(x) plot(theta, plot.list[[x]], type="h", col="navy", lwd=3, 
                               main=plot.titles[x], xlab=bquote(theta), ylab=ylabs[[x]]))
  #   CI95   #
  draws = rbeta(10000, shape1=postShape[1], shape2=postShape[2])
  bayes.est = mean(draws)
  ci95 = quantile(draws, prob=c(0.025, 0.975))
  abline(v=bayes.est, col="darkred", lwd=1)
  abline(v=ci95, col="darkred", lwd=1, lty=2)
  list(theta= theta, 
       Prior= p_theta, 
       Likelihood= Likelihood, 
       Posterior= ptheta_given_data,
       pData= p.data, 
       BayesEst= bayes.est,
       CI95= ci95,
       postShape= postShape)
}
