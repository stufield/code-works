# --------------------------
# Optim() given a function
# that takes a vector of
# parameters and turns them
# into a number, then finds
# the parameter set that
# maximizes that value
# --------------------------
require(emdbook)
# --------------------------
a <- 0.5
b <- 12
t <- 0.1
curve(-t * (x - a)^2 + b, from = -10, to = 10)

a <- 5
b <- 3
t <- 0.1
curve(-t * (x - b)^2-a, from = -10, to = 10)

myfun <- function(x,a,b,t) {
  -t * (x - a)^2 + b
}

myfun2 <- function(x, a, b, t) {
  2 * x^3 + a * x^2 - b * x + 2
}

myfun3 <- function(x, a, b, t) {
 -t * (x - a)^3 + b
}



# --------------------
# optimizer function
# --------------------
optimizer(fn = myfun, I = c(-10, 10), max = TRUE, a = 2, b = 12, t = 0.1)
optimizer(fn = myfun, I = c(-10, 10), max = FALSE, a = 2, b = 12, t = 0.1)

optimizer(fn = myfun2, I = c(-10, 10), max = TRUE, a = 2, b = 12, t = 0.1)
optimizer(fn = myfun2, I = c(-10, 10), max = FALSE, a = 2, b = 12, t = 0.1)

optimizer(fn = myfun3, I = c(-10, 10), max = TRUE, a = 2, b = 12, t = 0.1)
optimizer(fn = myfun3, I = c(-10, 10), max = FALSE, a = 2, b = 12, t = 0.1)

# For one parameter (x)
optimize(f = myfun, interval = c(-10, 10), maximum = TRUE, a = a, b = b, t = t)
optimize(f = myfun2, interval = c(-10, 10), maximum = TRUE, a = a, b = b, t = t)

#######################
# Using optim()
# 2 parameter example
#######################
?optim
fun1 <- function(x) {
  (x[1L] - 3)^2 + (x[2L] - 1)^2 + 5.5
}

optim(par = c(0, 0), fn = fun1, method = "BFGS", # or "Nelder-Mead" or "SANN"
      lower = -Inf, upper = Inf, control = list(), hessian = TRUE)

curve((x-3)^2 + (x-1)^2 + 5.5, from = -10, to = 10)

# ---------------
# with mle2
# ---------------
# must tell it vector name if pars
# are not separate arguments
parnames(myfun) <- c("x", "y")
fit2 <- bbmle::mle2(myfun, start =  list(x = -20, y = 3), vecpar = TRUE, method = "Nelder-Mead")


