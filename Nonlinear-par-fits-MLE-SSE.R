# -----------------------
# Nonlinear fitting with
# stats::nls()
# stats::nlm()
# stats::optim() -> ML & SSE
# -----------------------
x <- seq(1, 26, by = 0.5)
K <- 0.4
m <- 0.4
c <- 15
set.seed(125)
fn <- function(x, K, m, c) K / (1 + exp(m * (c - x)))
df <- data.frame(x = x,
                 y = fn(x, K, m, c) + rnorm(length(x), mean = 0, sd = 0.03))
plot(df, main = "Nonlinear Least Squares", xlab = "time",
     ylab = "Proportion Infected")

# --------------------
# check for normality
# --------------------
ks.test(df$y, pnorm)   # nope

# ------------------
# fit using nls()
# Non-linear Least Squares
# ------------------
fit <- stats::nls(y ~ K / (1 + exp(m * (c - x))), data = df,
                  start = list(K = 1, m = 1, c = 5))
summary(fit)
nlspars <- coef(fit)
nlspars
ci95 <- confint(fit)

plot(df, main = "Nonlinear Least Squares", pch = 19,
     xlab = "time", ylab = "Proportion Infected")
curve(fn(x, K = nlspars["K"], m = nlspars["m"], c = nlspars["c"]), from = 0,
      to = max(x), lty = 1, col = 4, lwd = 2, add = TRUE)
curve(fn(x, K = ci95["K", "2.5%"], m = ci95["m", "97.5%"], c = ci95["c", "97.5%"]),
      from = 0, to = max(x), lty = 2, col = 2, lwd = 2, add = TRUE)
curve(fn(x, K = ci95["K", "97.5%"], m = ci95["m", "2.5%"], c = ci95["c", "2.5%"]),
      from = 0, to = max(x), lty = 2, col = 2, lwd = 2, add = TRUE)

# ------------------------------
# Now with stats::optim()
# but with SSE
# This is VERY close to stats::nls()!
# ------------------------------
sse <- function(pars, data) {
  Y <- fn(data$x, pars[1L], pars[2L], pars[3L])
  sum((data$y - Y)^2)
}
opt <- optim(c(K = 1, m = 1, c = 5), fn = sse, data = df, method = "BFGS")

# ---------------------------
# Nonlinear Fitting via
# Maximum Likelihood and stats::nlm()
# ---------------------------
# Create function to maximize
LL <- function(pars, data) {
  Y <- fn(data$x, pars[1L], pars[2L], pars[3L])
  n <- nrow(data)
  -sum(dnorm(data$y, mean = mean(Y), sd = sd(Y), log = TRUE))
}

ML <- stats::nlm(LL, p = c(K = 1, m = 1, c = 5), data = df, steptol = 1e-05)
ML
nlmpars <- ML$estimate

plot(df, main = "MLE 1", pch = 19, xlab = "time", ylab = "Proportion Infected")
curve(fn(x, nlmpars[1L], nlmpars[2L], nlmpars[3L]), from = 0.1, to = max(x),
      col = "navy", lwd = 2, add = TRUE)

# ------------------------------
# Now with stats::optim()
# ------------------------------
ML <- optim(c(K = 1, m = 1, c = 5), fn = LL, data = df, method = "Nelder-Mead")
opars <- ML$par

plot(df, main = "MLE", pch = 19, xlab = "time", ylab = "Proportion Infected")
curve(fn(x, opars["K"], opars["m"], opars["c"]), from = 0.1, to = max(x),
      col = 4, lwd = 2, add = TRUE)



# -----------------------
# Three-way comparison
# -----------------------
kurve <- purrr::partial(curve, from = 0, to = max(df$x), lwd = 2, add = TRUE)
plot(df, main = "Comparison of Various Fitting Procedures", pch = 19, col = "gray70",
     xlab = "time", ylab = "Proportion Infected")
kurve(fn(x, nlspars["K"], nlspars["m"], nlspars["c"]), col = 2)
kurve(fn(x, nlmpars[1L], nlmpars[2L], nlmpars[3L]), col = 3)
kurve(fn(x, opars["K"], opars["m"], opars["c"]), col = 4)
legend("topleft", bg = "gray95",
       legend = c("Non-linear L-S (nls)", "MLE (nlm)", "MLE (optim)"),
       col = c(2, 3, 4), lty = 4)
