# ----------------
# rescale vector
# range (-1,1)
# ----------------
rescale_11 <- function(x) {
  subfun <- function(x) (x - min(x)) / max((x - min(x))) * 2 - 1
  if ( is.null(dim(x)) ) {
    subfun(x)
  } else if ( length(dim(x)) == 2L ) {
    apply(x, 2, subfun)
  } else {
    stop("Check dimensions of 'x', must be a data frame, matrix, or vector.", 
         call. = FALSE
    )
  }
}
