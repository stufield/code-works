# --------------------------------
# Solving ODEs with lsoda in R
# --------------------------------
library(magrittr)
library(deSolve)
# Logistic growth
ODEfun <- function(t, y, parms) {
  N <- y[1L]
  with(parms, {
    dN <- r * N * (1 - N/K)
    list(dN)
  })
}

logistic <- deSolve::lsoda(
  y = c(N = 0.1),
  times = seq(0, 10, by = 0.1),
  func = ODEfun,
  parms = list(r = 0.9, K = 5)) %>% data.frame()

head(logistic)
plot(N ~ time, data = logistic, ylab = "N", xlab = "time", col = "navy",
     main = "Logistic Growth")


# ---------------
# The SI model
# ---------------
# assumes births & deaths equal and deaths
# from disease occur on timescale longer than we're interested in
SIfun <- function(t, y, parms) {
  S <- y[1L]
  I <- y[2L]
  with(parms, {
    dS <- -beta * S * (I / (S + I))
    dI <- beta * S * (I / (S + I))
    list(c(dS, dI))
  })
}

pars  <- list(beta = 0.75)
init  <- c(S = 4999, I = 1)
SIout <- data.frame(
  lsoda(y = init, times = seq(0, 25, by = 0.5), func = SIfun, parms = pars)
  )
head(SIout)

par(mfrow = c(1, 2))
plot(S ~ time, data = SIout, ylab = "Susceptible= blue",
     xlab = "time", col = "navy")
points(I ~ time, data = SIout, col = "darkred")
plot(I ~ S, data = SIout, ylab = "Susceptible", xlab = "Infected")


# -----------
# SIR model
# -----------
SIRfun <- function(t, y, parms) {
  S <- y[1L]
  I <- y[2L]
  R <- y[3L]
  with(parms, {
    dS <- -beta * S * (I / (S + I))
    dI <- beta * S * (I / (S + I)) - (gamma * I)
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

# beta = transmission
# gamma = recovery
pars <- list(beta = 0.6, gamma = 0.5)
init <- c(S = 4999, I = 1, R = 0)
SIRout <- data.frame(
  deSolve::lsoda(y = init, times = seq(0, 100, by = 1/2), func = SIRfun, parms = pars)
)

SIRout %>%
  tidyr::pivot_longer(cols = c("S", "I", "R"), names_to = "status",
                      values_to = "n") %>%
  dplyr::mutate(status = factor(status, levels = c("S", "I", "R"))) %>%
  ggplot2::ggplot(ggplot2::aes(x = time, y = n, colour = status)) +
  ggplot2::geom_point(size = 2) +
  NULL

plot(S ~ time, data = SIRout, ylab = "Susceptible", xlab = "time", col = "navy")
points(R ~ time, data = SIRout, col = "darkgreen")
points(I ~ time, data = SIRout, col = "darkred")
par(mfrow = c(2, 2))

plot(S ~ time, data = SIRout, ylab = "Susceptible", xlab = "time", col = "navy")
plot(I ~ time, data = SIRout, ylab = "Infected", xlab = "time", col = "darkred")
plot(R ~ time, data = SIRout, ylab = "Recovered", xlab = "time", col = "darkgreen")
plot(I ~ S, data = SIRout, ylab = "Infected", xlab = "Susceptible")


# -------------------
# Ottar's example
# SEIR model; from EEID 2007
# --------------------------
SEIRmod <- function(t, y, parms) {
  S <- y[1L]
  E <- y[2L]
  I <- y[3L]
  R <- y[4L]
  with(parms, {
    dS <- mu * (N - S) - beta * S * I/N
    dE <- beta * S * I/N - (mu + sigma) * E
    dI <- sigma * E - (mu + gamma) * I
    dR <- gamma * I - mu * R
    list(c(dS, dE, dI, dR))
  })
}

parameters <- list(mu = 1 / 50, N = 1, beta = 1000, sigma = 365 / 8, gamma = 365 / 5)
init       <- c(S = 0.06, E = 0, I = 0.001, R = 0.939)
SEIRout <- data.frame(
  lsoda(init, times = seq(0, 10, by = 1 / 120), func = SEIRmod, parms = parameters)
  )
head(SEIRout)

par(mfrow = c(2, 2))
plot(S ~ time, data = SEIRout, ylab = "Susceptible", xlab = "time")
plot(I ~ time, data = SEIRout, ylab = "Infected", xlab = "time", col = "darkred")
plot(R ~ time, data = SEIRout, ylab = "Recovered", xlab = "time", col = "green")
plot(I ~ S, data = SEIRout, ylab = "Infected", xlab = "Susceptible")


# ---------------------------------------------
# Exercises Evolutionary response (a) & (b)
# ---------------------------------------------
EvolFun <- function(t, y, parms) {
  x <- y[1L]
  a <- y[2L]
  with(parms, {
    dx <- x * (r - (a - aopt)^2)
    da <- E * (-2 * (a - aopt))
    list(c(dx, da))
  })
}

pars <- list(r = 0.2, aopt = 10, E = 0.03)
Evol <- data.frame(
  lsoda(c(x = 2, a = 9.15), times = 0:100, func = EvolFun, parms = pars)
)
head(Evol)

par(mfrow = c(1, 2))
plot(x ~ time, data = Evol, ylab = "Population Size", xlab = "time", col = "navy")
plot(a ~ time, data = Evol, ylab = "Trait Value (mean activation temp.)",
     xlab = "time", col = "darkred")
