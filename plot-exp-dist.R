##########################
x <- seq(0, 1, by = 0.01)
c <- 5
M <- matrix(0, nrow = length(x), ncol = c)
for (i in 1:c) {
  M[, i] <- 1 + exp(-x * i)
}

# Plot Matrix of exp() values
matplot(M, type = "l", lwd = 2)
scatter.smooth(1:length(x), M[, 2], col = 4)
