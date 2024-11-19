
# https://github.com/r-lib/testthat/issues/859
library(testthat)
library(tibble)

# create nesting structure with tibbles
x <- list(
  id = c("A", "B"),
  tbl = tibble(a = 1:2, b = list(tibble(col1 = 99, col2 = 1L),
                                 tibble(col1 = 99, col2 = 1L)))
)
y <- list(
  id = c("A", "B"),
  tbl = tibble(a = 1:2, b = list(tibble(col1 = 99, col2 = 1L),
                                 tibble(col1 = 98, col2 = 1L)))
)

compare(x, x)
# dplyr is the devil causing problems here
head(dplyr::select(mtcars, hp))

# now fails
compare(x, x)
expect_equal(x, x)

# recursive unit tests:
# because lists may contain nested tibbles, or tibbles with lists of tibbles
# recurse the list tree until at leaves; then run expectation
# works because tibbles/data.frames are actually special lists
expect_equal_rcrv_tbl <- function(object, expected, label = NULL) {
  purrr::iwalk(object, ~ {
    label <- paste(c(label, .y), collapse = "$")
    exp   <- expected[[.y]]
    if ( inherits(.x, c("list", "tbl_df")) ) {
      expect_equal_rcrv_tbl(.x, exp, label)
    } else {
      expect_equal(.x, exp, label = label, expected.label = "the expected value")
    }
  })
}
# same
expect_equal_rcrv_tbl(x, x)
# different
expect_equal_rcrv_tbl(x, y)
