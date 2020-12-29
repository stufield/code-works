# -----------------------------
# stuRpkg::optimizer() Function
# Finds Max or Min to a
# specified function over
# a given range of values
# then graphs it
# -----------------------------
library(stuRpkg)
myfun <- function(x, a, b, t) {
  -t * (x - a)^2 + b
}

myfun2 <- function(x, a, b, t) {
  2 * x^3 + a * x^2 - b * x + 2
}

myfun3 <- function(x, a, b, t) {
  -t * (x - a)^3 + b
}


# -------------------
# stuRpkg::optimizer()
# -------------------
optimizer(fn = myfun, I = c(-10, 10), max = TRUE, a = 2, b = 12, t = 0.1)
optimizer(fn = myfun, I = c(-10, 10), max = FALSE, a = 2, b = 12, t = 0.1)
optimizer(fn = myfun2, I = c(-10, 10), max = TRUE, a = 2, b = 12, t = 0.1)
optimizer(fn = myfun2, I = c(-10, 10), max = FALSE, a = 2, b = 12, t = 0.1)
optimizer(fn = myfun3, I = c(-10, 10), max = TRUE, a = 2, b = 12, t = 0.1)
optimizer(fn = myfun3, I = c(-10, 10), max = FALSE, a = 2, b = 12, t = 0.1)


# -------------------
# By Least Squares
# -------------------
x <- seq(1, 10, by = 0.1)
a <- 1
b <- 2
y.det <- (a - b / x)
#y <- jitter((a-b/x) + c, amount=0.1)
y <- rnorm(length(x), mean = y.det, sd = 0.05)
mydata <- cbind(x, y)
plot(mydata)

sse <- function(pars, data) {
  a <- pars[1L]
  b <- pars[2L]
  x <- data[, 1L]
  y <- data[, 2L]
  y_det <- (a - b / x)
  sse <- sum((y - y_det)^2)
  sse
}

pars <- c(a = 0.5, b = 2)
fit  <- optim(par = pars, fn = sse, data = mydata)
CI95_fit <- confint(fit)

plot(mydata)
lines(x, (fit$par[1] - fit$par[2]/x), col=2)
lines(x, y.det, col=4)

# --------------------
# fit using stats::nls()
# --------------------
fit2 <- nls(y ~ a - b / x, start= list(a = 1, b = 2))
summary(fit2)
a_hat <- coef(fit2)["a"]
b_hat <- coef(fit2)["b"]
CI95_fit2 <- confint(fit2)
CI95_fit2

plot(mydata)
lines(x, (a_hat - b_hat / x), col = 2)
lines(x, (fit$par[1L] - fit$par[2L] / x), col = 3)
lines(x, y.det, col = 4)
