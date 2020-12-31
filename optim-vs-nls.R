# -----------------------------
# stats::optim() 2-parameter example
# -----------------------------
# objective function: Least-squares
# ------------------------------
set.seed(1)
x <- seq(1, 10, by = 0.1)
a <- 1
b <- 2
y_det <- (a - b / x)
y  <- y_det + rnorm(length(x), mean = 0, sd = 0.25)   # add e_ij ~ N(0,0.1)
df <- data.frame(x = x, y = y)

sse <- function(pars, data) {
  a <- pars[1L]
  b <- pars[2L]
  x <- data$x
  y <- data$y
  y_det <- (a - b / x)
  sse   <- sum((y - y_det)^2)
  sse
}

pars <- c(a = 0.5, b = 2)
fit  <- optim(par = pars, fn = sse, data = df)
fit

plot(df)
lines(x, (fit$par["a"] - fit$par["b"] / x), col = 2)
lines(x, y_det, col = 4, lty = 2)

# --------------------
# fit using stats::nls()
# --------------------
fit2 <- nls(y ~ a - b / x, start= list(a = 1, b = 2))
summary(fit2)
a_hat <- coef(fit2)["a"]
b_hat <- coef(fit2)["b"]
ci95 <- confint(fit2)
ci95

plot(df)
lines(x, (a_hat - b_hat / x), col = "green")
lines(x, (fit$par["a"] - fit$par["b"] / x), col = "navy")
lines(x, (ci95["a", "2.5%"] - ci95["b", "2.5%"] / x), col = "red", lty = 2)
lines(x, (ci95["a", "97.5%"] - ci95["b", "97.5%"] / x), col = "red", lty = 2)
