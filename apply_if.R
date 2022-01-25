
apply_if <- function(.data, .predicate, .f, ...) {
  idx <- which(vapply(.data, .predicate, NA))
  for ( i in idx ) .data[[i]] <- .f(.data[[i]], ...)
  .data
}

bench::mark(
  apply_if = apply_if(iris, is.numeric, log, base = 10),
  mutate_if = dplyr::mutate_if(iris, is.numeric, log, base = 10)
)
