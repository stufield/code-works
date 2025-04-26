#' Median Normalization Plots
#'
#' A quick plotting routine for the visualization of ADAT median
#' normalization scale factors. Produces boxplots of all 3 dilution mixes
#' by the `group =` argument.
#'
#' @param data A `data.frame` object containing columns
#'   of median normalization scale factors to plot.
#' @param group Column name by which to split the boxplots.
#' @param drop_hyb `logical(1)`. Should hybridization scale factors also be plotted?
#' @param do_cdf `logical(1)`. Should CDFs be plotted instead? Many boxplot specific
#'   arguments will be ignored.
#' @param notch Logical. Passed to [geom_boxplot()].
#' @examples
#' adat <- dplyr::filter(plasma20_hyb_med, SampleType == "Sample")
#' plotMedNorm(adat)   # defaults
#'
#' plotMedNorm(adat, group = "TimePoint")
#'
#' plotMedNorm(adat, notch = FALSE)  # silence notch warning
#'
#' plotMedNorm(adat, do.cdf = TRUE)
#' @importFrom SomaPlot theme_soma
#' @importFrom ggplot2 ggplot geom_boxplot geom_hline
#' @importFrom ggplot2 geom_vline aes geom_jitter scale_color_manual
#' @importFrom ggplot2 scale_fill_manual scale_x_discrete
#' @importFrom ggplot2 ylab facet_wrap ggtitle stat_ecdf
#' @export
plot_scale_factors <- function(data, group = "SampleGroup", drop_hyb = TRUE,
                               do_cdf = FALSE, notch = TRUE) {

  medsf <- sort(get_norms(data, drop_hyb = drop_hyb))

  if ( length(medsf) == 0L ) {
    stop(
      "No median scale factors found in adat. ",
      "Are you sure this data has been median normalized?",
      call. = FALSE
    )
  }

  if ( do_cdf ) {
    gg <- refactor_data(data) |>
      dplyr::select(all_of(medsf), group) |>
      tidyr::gather(key = Mix, value = value, -!!group) |>
      dplyr::mutate(Mix = factor(Mix, levels = medsf)) |>
      ggplot(aes(x = value, colour = !!dplyr::sym(group))) +
      stat_ecdf(size = 0.7) +
      scale_color_manual(values = col_string) +
      labs(title = "eCDF Median Normalization Scale Factors",
           x = "Scale Factor",
           y = bquote(italic(P) ~ (X < x))) +
      geom_vline(xintercept = c(0.4, 2.5), linetype = "dashed") +
      facet_wrap(~Mix, scales = "free_y") +
      theme_soma() +
      NULL
  } else {
    gg <- refactor_data(data) |>
      dplyr::select(all_of(medsf), group) |>
      tidyr::gather(key = Mix, value = value, -!!group) |>
      dplyr::mutate(Mix = factor(Mix, levels = medsf)) |>
      ggplot(aes(y = value, x = !!dplyr::sym(group),
                 fill = !!dplyr::sym(group))) +
      geom_boxplot(color = "#1F3552", alpha = 0.7,
                   outlier.color = NA,   # no outliers, jitter instead
                   notch = notch, size = 0.5) +
      scale_fill_manual(values = col_string) +
      geom_jitter(width = 0.05, alpha = 0.5, size = 2.5) +
      scale_x_discrete(name = group) +
      geom_hline(yintercept = c(0.4, 2.5), linetype = "dashed") +
      ylab("Scale Factor") +
      ggtitle("Median Normalization Scale Factors") +
      facet_wrap(~Mix) +
      theme_soma() +
      NULL
  }
  gg
}
