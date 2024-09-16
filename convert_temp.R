#' Convert Temperatures
#'
#' Converts temperatures from Celcius -> Fahrenheit
#' and Fahrenheit -> Celcius.
#'
#' @param x Input degree(s), either in C or F.
#' @param C2F Logical. Should Celcius units be converted to Fahrenheit?
#' @example
#' x <- seq(-50, 50, 0.5)
#' plot(convert_temp(x) ~ x, xlab = "Celcius", ylab = "Fahrenheit",
#'      type = "l", lwd = 2, col = "navy")
#' abline(h = 0, lty = 2)
#' abline(v = 0, lty = 2)
#' @export
convert_temp <- function(x, C2F = TRUE) {
  if ( C2F ) {
    9 / 5 * x + 32
  } else {
    5 / 9 * (x - 32)
  }
}
