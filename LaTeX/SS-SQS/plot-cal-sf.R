#' Plot Calibration Scale Factors
#'
#' Empirical cumulative distribution functions (CDFs) of calibration
#' scale factors, grouped either by plate(s) or by plate and dilution mix.
#'
#' @param apt_data Optionally provide the "Col.Meta".
#' @param do_log `logical(1)`. Should the plot be on a log2-scale.
#' @param by_dilution `logical(1)`. Should CDFs be drawn by analyte dilution?
#' @param xlim Limits for the x-axis of the CDF. If not supplied, the x-axis
#'   is centered and extreme values are trimmed.
#' @param x_lab `character(1)`. Optional x-axis label.
#' @param ablines Positions of the vertical lines indicating the
#'   acceptance SOP limits for calibration scale factors.
#'   Must be `length == 3`, including the center line (0 or 1).
#' @param median_pt `logical(1)`. Should a symbol be placed at the CDF median?
#' @param ... Additional arguments passed to [SomaPlotr::plotCDFlist()].
#' @importFrom stats quantile
#' @importFrom ggplot2 coord_cartesian geom_vline scale_color_manual annotate
#' @importFrom utils head
#' @importFrom dplyr any_of
#' @importFrom purrr map_if
plot_cal_sf <- function(adat = NULL, apt_data = NULL, do_log = FALSE,
                        by_dilution = FALSE, x_lab = NULL, xlim = NULL,
                        main = "Calibration Scale Factor CDFs",
                        cols = col_string, ablines = NULL, median_pt = TRUE, ...) {

  cal_data <- get_cal_sf(adat, apt_data)
  names(cal_data) <- cleanNames(names(cal_data))

  # this grep may need fixing; fragile
  plates <- sort(grep("^Cal[.]S", names(cal_data), value = TRUE))
  L      <- length(plates)
  cols   <- unname(head(cols, L))

  if ( L == 0L ) {
    stop(
      "No calibration scale factors found in column meta data.",
      call. = FALSE
    )
  }

  signal_done("Plotting", value(L), "plates:")
  lapply(plates, function(.x) signal_todo("  ", .x))

  if ( do_log ) {
    cal_data <- map_if(cal_data, is.numeric, log2) |> data.frame()
  }

  if ( is.null(xlim) ) {
    # pass through -> plotCDFbyGroup()
    trim     <- 0.001
    xlim     <- unlist(cal_data[, plates]) |> stats::quantile(c(trim, 1 - trim))
    center   <- ifelse(do_log, 0, 1)
    max_dist <- abs(xlim - center) |> max()
    xlim     <- center + max_dist * c(-1, 1)
  }

  if ( is.null(x.lab) ) {
    x.lab <- if ( do_log )
      bquote(italic(log)[2] ~ (Cal~Scale~Factor)) else "Cal Scale Factor"
  }

  line_vec <- c(0.6, 0.8, 1.2, 1.4)

  if ( do_log ) {
    line_vec <- log2(line_vec)
  }

  if ( by.dilution ) {

    if ( !"Dilution" %in% names(cal_data) ) {
      stop(
        "The 'Dilution' field is absent from `cal_data`.\n",
        "It is required if `by.dilution = TRUE`.", call. = FALSE
      )
    }

    cal_data <- cal_data |>
      tidyr::pivot_longer(cols = c(-AptName, -Dilution),
                          names_to = "Plate", values_to = "sf") |>
      dplyr::mutate(Dilution = paste0(Dilution, "% Mix"))

    n_cols <- length(unique(cal_data$Dilution))
    dil_cols <- head(unlist(soma_colors, use.names = FALSE), n_cols)

    p <- cal_data |>
      ggplot(aes(x = sf, colour = Dilution)) +
      stat_ecdf(size = 0.75) +
      coord_cartesian() +
      geom_vline(xintercept = line_vec, colour = "grey80",
                 linetype = "dashed", size = 0.5) +
      geom_hline(yintercept = 0.5, colour = "grey80",
                 linetype = "dashed", size = 0.5) +
      scale_color_manual(values = dil_cols) +
      labs(x = "Calibration Scale Factor", y = bquote(italic(P) ~ (X < x))) +
      facet_wrap(~ Plate) +
      theme_soma(hjust = 0) +
      NULL

  } else { # all plates on one figure

    if ( is.null(ablines) ) {
      # this won't be symmetric!
      ablines <- line_vec
    }

    cal_data <- dplyr::select(cal_data, -any_of(c("AptName", "Dilution"))) |>
      as.list()
    p <- SomaPlotr::plotCDFlist(cal_data, x.lab = x_lab, main = main,
                                cols = cols, ablines = ablines,
                                xlim = xlim, ...)

    if ( median_pt ) {
      p <- p + annotate("point",
                        x = vapply(cal_data, median, 1.0),
                        y = rep(0.5, length(cal_data)),
                        fill = cols, size = 3.5,
                        alpha = 0.5, pch = 23)
    }
    p <- p + geom_hline(yintercept = 0.5,
                        linetype = "dashed",
                        color = "grey")
  }
  p
}
