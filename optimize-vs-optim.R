# --------------------------
# stats::optimize() given a function
# that takes a vector of
# parameters and turns them
# into a number, then finds
# the parameter set that
# maximizes that value
# --------------------------
a <- 0.5
b <- 12
t <- 0.1
curve(-t * (x - a)^2 + b, from = -10, to = 10)

a <- 5
b <- 3
t <- 0.1
curve(-t * (x - b)^2-a, from = -10, to = 10)

myfun <- function(x, a, b, t) {
  -t * (x - a)^2 + b
}

myfun2 <- function(x, a, b, t) {
  #print(x)  # print 'x' values were f() evaluated
  2 * x^3 + a * x^2 - b * x + 2
}

myfun3 <- function(x, a, b, t) {
 -t * (x - a)^3 + b
}



# --------------------
# stats::optimize() function
# --------------------
par(mfrow = c(1, 3))
optimizer <- purrr::partial(stats::optimize, lower = -10, upper = 10,
                            a = 2, b = 12, t = 0.1)
curve(myfun(x, a = 2, b = 12, t = 0.1), from = -10, to = 10, col = "navy",
      lwd = 2, ylab = "myfun", main = "Correct solution")
opt <- sapply(c(TRUE, FALSE), function(.x) optimizer(myfun, maximum = .x))
apply(opt, 2, function(.x) points(.x[1L], .x[2L], pch = 13, col = "red", cex = 2.5))

opt <- sapply(c(TRUE, FALSE), function(.x) optimizer(myfun2, maximum = .x))
curve(myfun2(x, a = 2, b = 12, t = 0.1), from = -10, to = 10, col = "navy",
      lwd = 2, ylab = "myfun2", main = "Wrong solution! (local max/min)")
apply(opt, 2, function(.x) points(.x[1L], .x[2L], pch = 13, col = "red", cex = 2.5))

opt <- sapply(c(TRUE, FALSE), function(.x) optimizer(myfun3, maximum = .x))
curve(myfun3(x, a = 2, b = 12, t = 0.1), from = -10, to = 10, col = "navy",
      lwd = 2, ylab = "myfun3", main = "Correct solution")
apply(opt, 2, function(.x) points(.x[1L], .x[2L], pch = 13, col = "red", cex = 2.5))

# ---------------
# Using stats::optim() for
# 1-parameter example
# ---------------
# hard-code parameters a/b/c, then minimize function to find
# 'x' and 'y' where function is minimized.
y = (x - a)^3 + (x - b)^2 + c
.fn <- function(x) {
  (x - 3)^2 + (x - 2)^2 + 5.5
}
opt <- optim(par = c(x = -1), fn = .fn, method = "BFGS")
curve(.fn(x), from = -10, to = 10, col = "navy", lwd = 2)
points(opt$par, opt$value, pch = 13, col = "red", cex = 2.5)


# ---------------
# with mle2
# ---------------
# must tell it vector name if pars
# are not separate arguments
parnames(myfun) <- c("x", "y")
fit2 <- bbmle::mle2(myfun, start =  list(x = -20, y = 3), vecpar = TRUE,
                    method = "Nelder-Mead")

