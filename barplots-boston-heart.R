
ldl_data <- withr::with_seed(101,
  list(Baseline = rnorm(100, 125, 25), FollowUp = rnorm(100, 90, 25))
)

mike_barplot <- function(dat, interval = c("CI95", "SEM"),
                         cols = ggplot2::alpha("dodgerblue", 0.4),
                         bar_w = 25, bar_col = 1, bar_type = 1, labs_cex = 1.25,
                         plot_pars = c(7, 7, 1), filename = NULL, ...) {

  SomaPlotr::figure(filename,
                    height = plot_pars[1L],
                    width  = plot_pars[2L],
                    scale  = plot_pars[3L])
  on.exit(SomaPlotr::close_figure(filename))
  par_def <- list(mgp = c(2.00, 0.75, 0.00), mar = c(3, 4, 3, 1))
  par(par_def)
  mu <- sapply(dat, mean)
  y  <- barplot(mu, ylim = c(0,300), col = NULL, axes = FALSE, axisnames = FALSE,
                ylab = "LDL Cholesterol (mg/dL)", main = "Test Figure")

  dims        <- par("usr")
  left        <- dims[1L]
  right       <- dims[2L]
  bottom      <- dims[3L]
  top         <- dims[4L]
  color_alpha <- ggplot2::alpha

  rect(left, bottom, right, 100, col = color_alpha("green",0.5), border = NA)
  rect(left, 100, right, 160, col = color_alpha("yellow",0.5), border = NA)
  rect(left, 160, right, top, col = color_alpha("red",0.5), border = NA)

  barplot(mu, col = cols, add = TRUE, border = NA, cex.lab = labs_cex,
          cex.axis = labs_cex, cex.main = labs_cex, cex.names = labs_cex, ...)
  box()
  sem      <- sapply(dat, function(x) sd(x) / sqrt(length(x)))  # SE of the mean
  interval <- match.arg(interval)
  l.bar    <- ifelse(interval == "CI95", 1.96 * sem, sem)    # bar length
  up       <- mu + l.bar
  lo       <- mu - l.bar

  g <- (max(y) - min(y)) / bar_w
  purrr::walk(seq_along(y), function(i) {
    lines(c(y[i], y[i]), c(up[i], lo[i]), col = bar_col, lty = bar_type)         # vert bar
    lines(c(y[i] - g, y[i] + g), c(up[i], up[i]), col = bar_col, lty = bar_type) # up hat
    lines(c(y[i] - g, y[i] + g), c(lo[i], lo[i]), col = bar_col, lty = bar_type) # lo hat
  })
}

mike_barplot(ldl_data, filename = "plots/mike_barplots.pdf")
