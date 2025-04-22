
my_SVD <- function(A) {
  mat_decomp <- eigen(t(A) %*% A)
  S <- diag(sqrt(mat_decomp$values))
  if ( !identical(dim(A), dim(S)) ) {
    S <- cbind(S, matrix(0, nrow(S), ncol(A) - ncol(S)))
  }
  S_1 <- diag(1 / diag(S)) # inverse of S
  V <- mat_decomp$vectors
  V_t <- t(V)
  U <- A %*% V %*% S_1
  list(Matrix = A,
       S   = S,
       S_1 = S_1,
       V   = V,
       V_t = V_t,
       U   = U,
       check = round(A - (U %*% S %*% V_t), 1L)
  )
}
