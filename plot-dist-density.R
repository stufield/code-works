# -------------------
# Plot Distributions
# -------------------
set.seed(1)
z1 <- rexp(100)
z2 <- rnorm(100)
z3 <- rpois(100, lambda = 2)
z4 <- rbinom(100, prob = 0.4, size = 100)
###################################
# oma goes c(bottom, left, top, right)
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0), bg = "darkslateblue")
#######################################
hist(z1, main = "rexp", col = "gray50")     # plot1
hist(z2, main = "rnorm", col = "gray50")     # plot2
hist(z3, main = "rpois", col = "gray50")    # plot3
hist(z4, main = "rbinom", col = "gray50")     # plot4
mtext("Densities", outer = TRUE, cex = 1.5)
