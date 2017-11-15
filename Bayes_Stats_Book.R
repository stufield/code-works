##################################
### Applied Bayesian Statistics
### by Scott Lynch
########################
### Normal Probability Density Function
mu=0; sigma= 1
curve(1/sqrt(2*pi*sigma^2)*exp(-((x-mu)^2)/(2*sigma^2)), from=-5, to=5, col=4)

### Binomial Probability Density (mass) Function
p = 0.5; N = 50
curve(choose(N,x) * p^x*(1-p)^(N-x), type="b", from=0, to=N, col=4, n=N+1, pch=19)
### Note doesn't like this because x must be discrete
### Cumulative Distn
plot(0:50,pbinom(0:N,N,p))
plot(0:50,cumsum(dbinom(0:N, size=N, p=0.5)))

### Uniform Probability Density Function
a = 2; b = 5
plot(c(a,b), rep(1/(b-a),2), type="b", pch=19, col=4, xlim=c(0,7), ylim=c(0,1))
points(c(a,b),rep(0,2), col=4)
lines(c(0,a), rep(0,2), col=4); lines(c(b,7), rep(0,2), col=4)


### Example on pg. 36
pvec = seq(0, 1, 0.01)
N = 1067; k=556
Lik = dbinom(x=k, size=N, prob=pvec)
plot(pvec, Lik, "l", xlab="probability, p", ylab="Likelihood, L(p | k=556)", sub="(k = 556, N = 1067)", main="Likelihood Function", lwd=1.5, xlim=c(0.2,0.8), ylim=c(0,0.03))
abline(v=pvec[which.max(Lik)], col=4, lty=2)
abline(h=max(Lik), col=5, lty=2)
par(new=T)
LLik = dbinom(x=k, size=N, prob=pvec, log=TRUE)
plot(pvec, log(Lik), type="l", lty=2, col=2, ylab="", xlab="", axes=FALSE, ylim=c(-100,300),xlim=c(0.2,0.8), lwd=1.5)
axis(4)
abline(h=max(log(Lik)), col=5, lty=2)


### Bayes Example
p.preg = 0.15 			# P(pregnant) aka the prior
p.pos_preg = 0.9		# P(+ test | pregnant) aka the likelihood of the data
p.pos_notpreg = 0.5		# P(+ test | not pregnant) aka false positives

#posterior = p.preg_pos

p.preg_pos <- p.pos_preg * p.preg / (p.pos_preg*p.preg + p.pos_notpreg*(1-p.preg))
p.preg_pos


### What if there is anohter positive test?
### Use the posterior as the next prior!

p.preg2 <- p.preg_pos
p.preg_pos <- p.pos_preg * p.preg2 / (p.pos_preg*p.preg2 + p.pos_notpreg*(1-p.preg2))
p.preg_pos

### What about 5 positive tests in a row?
prior = p.preg 	# set prior equal to initial P(pregnant)
for (i in 1:6){
	p.preg_pos <- p.pos_preg * prior / (p.pos_preg*prior + p.pos_notpreg*(1-prior))
	prior = p.preg_pos # update prior with new data
}
p.preg_pos

###################################
## The Beta Distribution
####################################
alpha = 50
beta = 50
p.vec = seq(0,1,0.01)
plot(p.vec, dbeta(p.vec, shape1= 50, shape2= 50), type="l", xlab="")
lines(p.vec, dbeta(p.vec, shape1= 5, shape2= 5), type="l", col=2)
lines(p.vec, dbeta(p.vec, shape1= 1, shape2= 1), type="l", col=3)
lines(p.vec, dbeta(p.vec, shape1= 10, shape2= 2), type="l", col=4)
lines(p.vec, dbeta(p.vec, shape1= 2, shape2= 10), type="l", col=5)
title("The Beta Distribution", xlab="Proportion/probability")
legend("topright", legend=c("Beta(50,50)","Beta(5,5)","Beta(1,1)","Beta(10,2)","Beta(2,10)"), col=c(1:5), lty=1, bg="gray95", cex=0.75)

###################################
## Election Poll Example; pg 56
####################################
# P(K | a,b,x) = P(poll data | K) * P(K)
# P(poll data | K) is the Likelihood approximated by the Binomial
# P(K) is the prior approximated by a Beta distn
# post <- k.vec^1497 * (1-k.vec)^1518

k.vec <- seq(0,1,0.001)
posterior <- dbeta(k.vec, shape1=1498, shape2=1519)
posterior <- posterior / max(posterior) # scale to max=1
plot(k.vec, posterior, type="l", xlim=c(0.4,0.6))
abline(v=0.5, col=2, lty=2)
prior <- dbeta(k.vec, shape1=942, shape2=1008)
prior <- prior / max(prior) # scale to max=1
lines(k.vec, prior, col=4)
Lik <- dbinom(556, size=1067, k.vec)
Lik <- Lik / max(Lik) # scale to max=1
lines(k.vec, Lik, col=2)
legend("topleft", legend=c("Prior","Posterior","Likelihood"), col=c(4,1,2), lty=1, bg="gray95", cex=0.75)


# Prob Bush wins:
pbeta(k.vec, shape1=1498, shape2=1519)[which(k.vec==0.5)]

# Prob Kerry wins:
1 - pbeta(k.vec, shape1=1498, shape2=1519)[which(k.vec==0.5)]


###################################
## Posterior Distribution Example
####################################
prior = c(1, 1, 1, 2, 2, 2, 4, 4, 4, 4, 2, 2, 2, 2, 1, 1, 1)
names(prior) = seq(0.2, 0.36, by= 0.01)
prior <- prior / sum(prior)
prior

y <- c(20, 22, 19, 29)
#y <- c(2, 5, 66, 25)
n <- c(80, 80, 80, 80)

barplot(prior, ylim=c(0,0.2), main="Prior Probabilities", xlab="p"); box()

discrete.bayes <- function (df, prior, y, ...){
	param <- as.numeric(names(prior))
	lk <- function(j) prod(df(y, param[j], ...))
	likelihood <- sapply(1:length(param), lk)
	pred <- sum(prior * likelihood)
	prob <- prior * likelihood / pred
	obj <- list(prob = prob, pred = pred)
	class(obj) <- "bayes"
	obj
}

out <- discrete.bayes(dbinom, prior, y, size=n)
post <- out$prob

par(mfrow=c(2,1))
barplot(prior, ylim=c(0,0.2), main="Prior Probabilities", xlab="p")
barplot(post, main="Posterior Probabilities", xlab="p")










