
reprex::reprex({
  library(magrittr)
  library(SomaPlyr)
  library(SomaSurvival)
  library(usethis)
  library(dplyr)

  apts <- attributes(wranglr::simdata)$sig_feats$surv
  frm  <- SomaSurvival:::createSurvFormula(apts)

  log_data <- select(sim_adat, time, status, all_of(apts)) |>
    log10() |> centerScaleData()

  model <- survival::survreg(frm, data = log_data)

  set.seed(101)
  new <- log_data[sample(1:nrow(log_data), 10), ]

  # Should now work correctly
  SomaSurvival:::getLHS(model, new)

  # Fails with better explanation
  SomaSurvival:::getLHS(model, rename(new, tyme = "time"))
})



reprex::reprex({
  f <- function(mean = NA_real_) mean(mean)
  identical(f(), mean(NA_real_))    # NA
  identical(f(1:10), mean(1:10))    # 5.5
})

reprex::reprex({
  l <- list()
  l$x <- matrix(1:1000^2, ncol = 1000)
  lobstr::obj_size(l)
  l$y <- l$x    # copy x -> y
  lobstr::obj_size(l)
})

reprex::reprex({
  f <- function(x) {
    cat("inside f global\n")
    print(lobstr::obj_size(env = .GlobalEnv))
    cat("inside f local\n")
    print(lobstr::obj_size())
    y <- x
    cat("inside f local after copy y -> x\n")
    print(lobstr::obj_size())
  }
  lobstr::obj_size(env = .GlobalEnv)
  x <- matrix(1:1000^2, ncol = 1000)
  lobstr::obj_size(f(x), env = .GlobalEnv)
  lobstr::obj_size(env = .GlobalEnv)
})


foo <- replicate(5, list(a = "value", b = 5), simplify = FALSE)
bench::mark(
  purrr = purrr::map(foo, purrr::pluck, "b"),
  base  = lapply(foo, `[[`, i = "b")
)




