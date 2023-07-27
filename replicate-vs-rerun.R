
bench::mark(
  base  = withr::with_seed(1, replicate(1000, mean(rnorm(10)), simplify = FALSE)),
  rerun = withr::with_seed(1, purrr::rerun(1000, mean(rnorm(10)))),
  map   = withr::with_seed(1, purrr::map(1:1000, ~ mean(rnorm(10))))
)
