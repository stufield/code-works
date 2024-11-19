
capture_snapshot <- function(code, make_reproducible = FALSE) {
  file <- tempfile("tmp", fileext = ".out")
  if ( make_reproducible ) testthat::local_reproducible_output()
  sink(file)
  withr::defer(unlink(file, force = TRUE))
  withCallingHandlers(
    eval(code),
    message = function(e) {
      cat(e$message, sep = "\n")
      invokeRestart("muffleMessage")
    }
  )
  lines <- readLines(file, encoding = "UTF-8")
  sink(NULL)
  writeLines(lines)
  invisible()
}

capture_snapshot(diffAdats(sim_adat, sim_adat[, -10L]))
capture_snapshot(diffAdats(sim_adat, sim_adat[, -10L]), TRUE)

capture_snapshot2 <- function(code, make_reproducible = FALSE) {
  if ( make_reproducible ) testthat::local_reproducible_output()
  withCallingHandlers(
    eval(code),
    message = function(e) {
      writeLines(e$message)
      invokeRestart("muffleMessage")
    }
  )
  invisible()
}
capture_snapshot2(diff_adats(sim_adat, sim_adat[, -10L]))
capture_snapshot2(diff_adats(sim_adat, sim_adat[, -10L]), TRUE)
