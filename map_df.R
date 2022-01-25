
map_df <- function(.x, .f, ..., .id = NULL) {
  if ( is.null(names(.x)) ) {
    names(.x) <- seq_along(.x)
  }
  dplyr::bind_rows(lapply(.x, .f, ...), .id = .id)
}
bench::mark(
  base  = map_df(letters, function(.x) data.frame(x = .x, y = 99), .id = "foo"),
  purrr = purrr::map_df(letters, function(.x) data.frame(x = .x, y = 99), .id = "foo")
)
