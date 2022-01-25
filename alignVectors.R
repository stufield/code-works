#' Align Character Vectors
#'
#' @param x A list of character strings to match and align.
#' @return A tibble of aligned strings.
#' @examples
#' x <- setNames(
#'   lapply(c(4, 5, 8, 7, 3), function(.x) sample(LETTERS[1:10], .x)),
#'   letters[1:5]
#' )
#' alignVectors(x)
#' x <- setNames(as.list(LETTERS), letters)
#' alignVectors(x)
#' @export
alignVectors <- function(x) {
  stopifnot(inherits(x, "list"))
  jagged <- function(.x) tibble::as_tibble(lapply(.x, "length<-", max(lengths(.x))))
  all <- unique(unlist(x))
  x   <- c(all = list(all), x)
  y   <- jagged(lapply(x, sort))
  out <- dplyr::mutate_at(y,  -1L, ~ .[match(all, .)])
  out$all <- NULL
  out
}
