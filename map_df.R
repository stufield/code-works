
map_df <- function(.x, .f, ..., .id = NULL) {
  if ( is.null(names(.x)) ) {
    names(.x) <- seq_along(.x)
  }
  ret <- do.call(rbind, c(make.row.names = FALSE, lapply(.x, .f, ...)))
  if ( !is.null(.id) ) {
    ret <- cbind(tmp = names(.x), ret)
    names(ret)[1L] <- .id
  }
  ret
}

map_df2 <- function(.x, .f, ..., .id = NULL) {
  if ( is.null(names(.x)) ) {
    names(.x) <- seq_along(.x)
  }
  dplyr::bind_rows(lapply(.x, .f, ...), .id = .id)
}

bench::mark(
  base  = map_df(letters, function(.x) data.frame(x = .x, y = 99), .id = "foo"),
  base_dplyr = map_df2(letters, function(.x) data.frame(x = .x, y = 99), .id = "foo"),
  purrr = purrr::map_df(letters, function(.x) data.frame(x = .x, y = 99), .id = "foo")
)
