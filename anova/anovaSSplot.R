# ---------------- #
# Stu Field
# Visualizing ANOVA via plotting
# of various variance components &
# Sum of Squares
# ---------------- #
plotANOVA <- function(x = NULL) {

  if ( is.null(x) ) {
    x <- list(
      sand  =  c(6, 10, 8, 6, 14, 17, 9, 11, 7, 11),
      clay  =  c(17, 15, 3, 11, 14, 12, 12, 8, 10, 13),
      loam  =  c(13, 16, 9, 12, 15, 16, 17, 13, 18, 14)
      )
    print("* Using Crawley's soil example")
  }

  L <- length(x)

  plotSS <- function(vec, start = 1L, pch, ...) {
    mu    <- mean(vec)
    l     <- length(vec)
    range <- start:(start + l - 1)
    points(range, vec, ylim = range(vec), pch = pch, ...)
    for ( i in range ) {
      lines(c(i, i), c(vec[i - start + 1L], mean(vec)), lty = 2, col = 8)
    }
    lines(c(range[1L], range[length(range)]), c(mean(vec), mean(vec)))
  }
  calcSS    <- function(x) sum((x - mean(x))^2)
  y         <- unlist(x)
  x_lab     <- bquote("sample y"[i])
  y_lab     <- "Residuals"
  symbols   <- c(15:25, 1:14L) |> head(L)
  n_vec     <- vapply(x, length, 1L)
  pch_vec   <- rep(symbols, times = n_vec)
  range_mat <- sapply(symbols, function(.x) range(which(pch_vec == .x))) |> t()
  plot(seq(y), y, type = "n", ylab = y_lab, main = "Total SS", xlab = x_lab)
  mtext(bquote(SS[total] == .(round(calcSS(y), 2L))), cex = 0.5, adj = 1)
  plotSS(y, pch = pch_vec, cex = 1.5)
  plot(seq(y), y, type = "n", ylab = y_lab, main = "SS Error", xlab = x_lab)
  ss_vec <- numeric(0)

  for ( i in 1:length(x) ) {
    range <- range_mat[i, ]
    plotSS(x[[i]], start = range[1L], pch = pch_vec[range], cex = 1.5)
    ss_vec[i] <- calcSS(x[[i]])
    text(median(range), par("usr")[3L], bquote(SS[i] == .(round(ss_vec[i], 1L))),
         pos = 3, cex = 0.9, col = "red")
  }
  mtext(bquote(SS[error] == .(round(sum(ss_vec), 2L))), cex = 0.5, adj = 1)
  means <- vapply(x, mean, 0.1) # group means
  plot(seq(means), means, type = "n", ylab = y_lab,
       main = "SS Group", xlab = x_lab, ylim = range(x))
  plotSS(means, pch = unique(pch_vec), cex = 1.5)
  ss_group <- sum(n_vec * (means - mean(means))^2)
  mtext(bquote(SS[group] == .(round(ss_group, 2L))), cex = 0.5, adj = 1)
}


# create sim data
withr::with_seed(1004, {
  normData <- lapply(rep(10, 6), rnorm, mean = 20, sd = 2)
  simData <- lapply(1:length(normData), function(.x)
    normData[[.x]] + runif(1, 1, 6) * 2)
})

height <- 480
SomaPlotr::figure("anova_ss.pdf", width = 1.5 * height, height = height,
                  scale = 1.5)
withr::with_par(list(mgp = c(2.00, 0.75, 0.00),
                     mar = c(3, 4, 3, 1) + 0.1,
                     mfrow = 2:3L), {
  plotANOVA(normData)
  plotANOVA(simData)
})
SomaPlotr::close_figure("anova_ss.pdf")
