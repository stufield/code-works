

x <- c(1.99,1.46,2.01,0.18,4.35,0.30,0.34,0.91,0.24,0.27,0.82,5.86,1.63,1.35,1.26,
		 1.40,2.59,1.34,0.70,1.39,1.40,0.83,2.87,3.62,0.52,4.15,1.72,1.23,1.60,1.61)

mlogl <- function(alpha, x) {
    if (length(alpha) > 1) stop("alpha must be scalar")
    if (alpha <= 0) stop("alpha must be positive")
    -sum(dgamma(x, shape=alpha, log=TRUE))
}

alpha.start <- mean(x)
fit <- optim(alpha.start, mlogl, x=x, hessian=TRUE)
print(fit)

fred <- function(alpha) apply(as.matrix(alpha), 1, mlogl, x=x)
curve(fred, from=min(x), to=max(x), xlab=expression(alpha), ylab=expression(loglik(alpha)))

length(x)
summary(x)
hist(x) 





########## 2 parameters #########################

mlogl <- function(theta, x) {
    if (length(theta) != 2) stop("theta must be vector of length 2")
    alpha <- theta[1]
    lambda <- theta[2]
    if (alpha <= 0) stop("theta[1] must be positive")
    if (lambda <= 0) stop("theta[2] must be positive")
    -sum(dgamma(x, shape=alpha, rate=lambda, log=TRUE))
}

alpha.start <- mean(x)^2 / var(x)
lambda.start <- mean(x) / var(x)
theta.start <- c(alpha.start, lambda.start)

fit <- optim(mlogl, theta.start, x=x, hessian=TRUE)
print(fit)
print(theta.start)
eigen(fit$hessian, symmetric=TRUE)

###############################################

mlogl <- function(theta) {
    stopifnot(is.numeric(theta))
    stopifnot(length(theta) == 5)
    mu1 <- theta[1]
    mu2 <- theta[2]
    v1 <- theta[3]
    v2 <- theta[4]
    p <- theta[5]
    -sum(log(p * dnorm(x, mu1, sqrt(v1)) + (1 - p) * dnorm(x, mu2, sqrt(v2))))
}

n <- length(x)
p.start <- 0.5
mu1.start <- mean(sort(x)[seq(along=x) <= n / 2])
mu2.start <- mean(sort(x)[seq(along=x) > n / 2])
v1.start <- var(sort(x)[seq(along=x) <= n / 2])
v2.start <- var(sort(x)[seq(along=x) > n / 2])
theta.start <- c(mu1.start, mu2.start, v1.start, v2.start, p.start)

fit <- nlm(mlogl, theta.start, print.level=2, fscale=length(x))
fit <- nlm(mlogl, fit$estimate, print.level=2, fscale=length(x), hessian=TRUE)
print(fit)
print(theta.start)

mu1.hat <- fit$estimate[1]
mu2.hat <- fit$estimate[2]
sigma1.hat <- sqrt(fit$estimate[3])
sigma2.hat <- sqrt(fit$estimate[4])
p.hat <- fit$estimate[5]
fred <- function(x) 
	p.hat * dnorm(x, mu1.hat, sigma1.hat) + (1 - p.hat) * dnorm(x, mu2.hat, sigma2.hat)
hist(x, freq=FALSE)
curve(fred, add=TRUE)
hist(x, freq=FALSE, ylim=c(0, fred(mu1.hat)))
curve(fred, add=TRUE)
eigen(fit$hessian, symmetric=TRUE)





#### Using a different optimizer ############

x <- c(1.99,1.46,2.01,0.18,4.35,0.30,0.34,0.91,0.24,0.27,0.82,5.86,1.63,1.35,1.26,
		 1.40,2.59,1.34,0.70,1.39,1.40,0.83,2.87,3.62,0.52,4.15,1.72,1.23,1.60,1.61)

mlogl <- function(theta) {
    stopifnot(is.numeric(theta))
    stopifnot(length(theta)==5)
    mu1 <- theta[1]
    mu2 <- theta[2]
    v1 <- theta[3]
    v2 <- theta[4]
    p <- theta[5]
    -sum(log(p * dnorm(x, mu1, sqrt(v1)) + (1 - p) * dnorm(x, mu2, sqrt(v2))))
}

### Get starting parameters ####
n <- length(x)
p.start <- 1 / 2
mu1.start <- mean(sort(x)[seq(along=x) <= n / 2])
mu2.start <- mean(sort(x)[seq(along=x) > n / 2])
v1.start <- var(sort(x)[seq(along=x) <= n / 2])
v2.start <- var(sort(x)[seq(along=x) > n / 2])
theta.start <- c(mu1.start, mu2.start, v1.start, v2.start, p.start)

fit <- optim(theta.start, mlogl, method="Nelder-Mead",
				 lower   = c(-Inf, -Inf, 0, 0, 0),
				 upper   = c(Inf, Inf, Inf, Inf, 1),
				 control = list(fnscale=length(x), trace=2),
				 hessian = TRUE)
print(fit)
print(theta.start)




#### Using MLE library of Stats4 ###############

x <- c(1.99,1.46,2.01,0.18,4.35,0.30,0.34,0.91,0.24,0.27,0.82,5.86,1.63,1.35,1.26,
		 1.40,2.59,1.34,0.70,1.39,1.40,0.83,2.87,3.62,0.52,4.15,1.72,1.23,1.60,1.61)

mlogl <- function(theta) {
    stopifnot(is.numeric(theta))
    stopifnot(length(theta)==5)
    mu1 <- theta[1]
    mu2 <- theta[2]
    v1 <- theta[3]
    v2 <- theta[4]
    p <- theta[5]
    -sum(log(p * dnorm(x, mu1, sqrt(v1)) + (1 - p) * dnorm(x, mu2, sqrt(v2))))
}

n <- length(x)
p.start <- 0.5
mu1.start <- mean(sort(x)[seq(along = x) <= n / 2])
mu2.start <- mean(sort(x)[seq(along = x) > n / 2])
v1.start <- var(sort(x)[seq(along = x) <= n / 2])
v2.start <- var(sort(x)[seq(along = x) > n / 2])
theta.start <- c(mu1.start, mu2.start, v1.start, v2.start, p.start)

fit <- stats4::mle(mlogl, c(mu1.start, mu2.start, v1.start, v2.start, p.start), 
						 method="Nelder-Mead", fixed=list())

