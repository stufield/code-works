##################################
# Eigenvector visualization
# for projection matrices
##################################
plot_vec <- function(v, ...) {
  arrows(0, 0, v[1L], v[2L], length = 0.1, ...)
}

eigen_ex <- function(A, x = 0:1, n = 5) {
  mat <- t(matrix(x))
  #plot(c(0,0), x, type="n")
  for (i in 1:n) {
    new <- A %*% mat[i, ]
    mat <- rbind(mat, t(new))
  }
  out <- list(Projection = mat,
             eigenvalues = eigen(A)$values,
            eigenvectors = eigen(A)$vectors, 
                  popbio = pop.projection(A,x,n)$stable.stage)
  plot(1, type = "n", ylim = c(-2, 2), xlim = c(-2, 2))
  abline(a = 0, b = 1, col = 2, lty = 3)
  sapply(1:2, function(i) plot_vec(out$eigenvectors[, i]))
  x11()
  plot(1, type = "n", xlim = c(-1, max(mat[, 1L])), ylim = c(-1, max(mat[, 2L])))
  sapply(1:nrow(mat), function(x) plot_vec(mat[x, ], col = x))
  abline(a = 0, b = 1, col = 2, lty = 3)
  out
}

#A <- matrix(c(1.25, 0.75, 0.6, 0.3), ncol=2)
