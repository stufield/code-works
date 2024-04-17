
# this captures messages and error messages (stdout), but not warnings
local_message_suppress <- function(.local_envir = parent.frame()) {
  file <- tempfile("local-msg-suppress-", fileext = ".out")
  info <- sink_setup(file)
  withr::defer({
    unlink(file, force = TRUE)
    sink_teardown(info)
  }, envir = .local_envir)
  invisible()
}

# helper to setup sink
sink_setup <- function(file) {
  con <- file(file, "w")
  sink(con, type = "message")
  list(con = con, n = sink.number(type = "message"))
}

# helper to clean up sink
sink_teardown <- function(sink_info) {
  if ( !is.null(sink_info$con) ) {
    on.exit(close(sink_info$con))
  }
  if ( sink_info$n == sink.number(type = "message") ) {
    sink(type = "message")
  }
  invisible()
}

# sample function using the local_*
with_message_suppress <- function(code) {
  local_message_suppress()
  eval(code)
}

f <- function() {
  message("Pie is good!")   # message trapped
  SomaGlobals::signal_oops("darn!")
  log(-1)  # warning not trapped
  pi
}

f()
with_message_suppress(f())

#showConnections()
#closeAllConnections()
