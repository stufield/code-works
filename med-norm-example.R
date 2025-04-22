
create_MedNorm_data <- function(samples = 84, feats = 1000) {

  feat_names <- sprintf("feat%02i", seq(feats))
  sample_names <- sprintf("S%02i", seq(samples))

  df <- data.frame(sapply(feat_names, function(x)
                   rnorm(samples, mean = rgamma(1, shape = 0.1, rate = 1) * 1e+05,
                         sd = 3500)),
                   row.names = sprintf("S%02i", 1:samples)) %>%
    setNames(feat_names)

  get_neg_cols <- function(x) unique(which(x <= 0, arr.ind = TRUE)[, 2L]) # which cols < 0
  neg_cols <- get_neg_cols(df)
  while ( length(neg_cols) > 0L ) {
    cat("*  features with negative values ...", length(neg_cols), "\n")
    for ( i in neg_cols ) {
      df[, i] <- rnorm(samples, mean = rgamma(1, shape = 0.1, rate = 1) * 1e+05,
                       sd = 3500) # re-draw rexp()
    }
    neg_cols <- get_neg_cols(df)
  }

  # add noise to samples
  df <- data.frame(t(apply(df, 1, function(i) {
                   scale_noise = -1
                   while ( scale_noise <= 0 ) {
                     scale_noise <- rnorm(1, mean = 1, sd = 0.25)
                   }
                   as.numeric(i) * scale_noise
                })),
                row.names = sample_names) %>%
    setNames(feat_names)


  # The Median Normalization
  feat_medians <- apply(df, 2, median)
  median_sf <- sapply(seq(nrow(df)), function(i)
                median(feat_medians / as.numeric(df[i, ]))) %>%
    setNames(sample_names)

  med_norm_df <- as.data.frame(df * median_sf) # multiply each row by its scale factor
  rownames(med_norm_df) <- sample_names
  ret <- list(orig = df, med_norm = med_norm_df,
              quantile_norm = quantile.normalize(df), sf = median_sf)
  class(ret) <- "med_norm"
  ret
}


plot_MedNorm <- function(x) {

  if ( !all(c("orig", "med_norm") %in% names(x)) ) {
    stop("`x` must be a named list with, `orig`, `med_norm`, and `quantile_norm`",
         call. = FALSE)
  }

  par_def <- list(mgp = c(2.00, 0.75, 0.00), mar = c(3, 4, 3, 1))
  par(par_def)
  ylims <- lapply(x[1:3L], function(i)
             apply(apply(log10(i), 1, quantile, probs = c(0.25, 0.75)), 1, mean))
  m <- matrix(c(rep(1, 3), rep(2, 3), rep(3, 3), c(4, 4, 5)), byrow = T, ncol = 3)
  layout(m)

  boxplot(log10(t(x$orig)), col = rainbow(nrow(x$orig)),
          notch = TRUE, ylim = ylims$orig, xaxs = "i",
          las = 2, main = "Un-Normalized Data", ylab = "log10(RFU)")
  boxplot(log10(t(x$quantile_norm)), col = rainbow(nrow(x$quantile_norm)),
          notch = TRUE, ylim = ylims$quantile_norm, xaxs = "i",
          las = 2, main = "Quantile-Normalized Data", ylab = "log10(RFU)")
  boxplot(log10(t(x$med_norm)), col = rainbow(nrow(x$med_norm)),
          notch = TRUE, ylim = ylims$med_norm, xaxs = "i",
          las = 2, main = "Median-Normalized Data", ylab = "log10(RFU)")

  barplot(x$sf, col = "navy", xaxs = "i", las = 2, names = names(x$sf),
          ylab = "Med Scale Factor", main = "Median Scale Factors by Sample",
          xlab = "", plot = TRUE, xpd = FALSE)
  my.ecdf(log2(x$sf), col = "navy", lwd = 2, xlab = "log2(Median Scale Factors)",
          main = "CDF of Median Scale Factors", xlim = c(-1.320, 1.32))
  abline(v = c(-1.32, 0, 1.32), col = c(2, 3, 2), lty = c(1, 2, 1))
}
