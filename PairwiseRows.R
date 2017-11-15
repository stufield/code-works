
mat <- matrix(round(runif(20, 25, 74)), nrow=4)

PairwiseRows <- function(x) {
  ret <- list()
  L   <- nrow(x)
  l   <- L - 1
  for ( n in 1:l ) {
    m <- n + 1
    ret <- c(ret, lapply(m:L, function(i) rbind(x[n,], x[i,])))
  }
  return(ret)
}

