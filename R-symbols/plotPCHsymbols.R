
function(f, h = 7, w = 10, s = 1) \{
  pdf(file = f, height = h * s, width = w * s, useDingbats = FALSE,
      title = basename(f))
  on.exit(dev.off())
  b <- 25L
  for (i in 1:b) \{
    if ( i == 1L ) \{
      plot(i, i, ylim = c(1, b), xlim = c(1, b), xlab = "", ylab = "", type = "n")
      grid(lty = 1, col = "gray90")
      title("Plotting Symbol, Line Type, & Color Codes in R")
      legend("topleft", legend = 1:6, lty = 1:6, lwd = 1.5, ncol = 2, bg = "gray95")
      legend("bottomright", legend = 1:8, col = 1:8, ncol = 3, pch = 19, bg = "gray95")
    \}
    points(i, i, pch = i, col = i, cex = 2)
  \}
\}
