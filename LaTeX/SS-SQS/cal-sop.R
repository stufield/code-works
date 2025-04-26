#' Calibration SOP
#'
#' Calculate SOP calibration acceptance criteria for
#' all analytes in a `soma_adat` object.
#'
#' @inheritParams hybNormalize
#' @param adat A `soma_adat` object with RFU values to be
#'   tested for SOP acceptance.
#' @param cals An optional data frame of calibration scale factors. If
#'   `NULL`, `cals` is created.
#' @param apt_data An `apt_data` object which contains calibration scale factor
#'   data and dilution data. Usually from a call to [getAnalyteInfo()].
#' @param do_plot `logical(1)`. Include scale factors and acceptance criteria plots?
#' @return An invisibly returned list of calibration SOP metrics:
#'   \item{cal.table}{A table of calibration statistics for each plate.}
#'   \item{tail.fails}{A list of the failed SOMAmers by plate.}
#'   \item{tail.tbl}{A tibble of the failed SOMAmers by plate with scale factors.}
#'   \item{plots}{A list of 2 `ggplots` for the median test and for the centered
#'                tail test.}
#' @author Stu Field
#' @examples
#' out <- calibrationSOP(plasma20_hyb_med_cal)
#'
#' # `ggplot2` print method performs plotting
#' out
#' @importFrom dplyr select filter all_of
#' @export
sop_calibration <- function(adat = NULL, cals = NULL, apt_data = NULL,
                            do_plot = TRUE, verbose = interactive()) {

  if ( is.null(cals) ) {
    cals <- get_cal_sf(adat, apt_data)
  }

  hyb_seqs  <- SomaDataIO::seqid2apt(seq_HybControlElution)
  cals      <- dplyr::filter(cals, !AptName %in% hyb_seqs) # remove HybControls
  cal_names <- grep("AptName|Dilution", names(cals), value = TRUE, invert = TRUE)
  medians   <- dplyr::select(cals, all_of(cal_names)) |>
    vapply(median, double(1))  # calc median cal sf by plate

  # Median Test
  # median must be between 0.8 - 1.2
  median_fails <- cal_names[abs(medians - 1) > 0.2]

  # Tail test
  tails <- setNames(cal_names, cal_names) |>
    lapply(function(.plate) {
      cal_sfs        <- cals[[.plate]]                  # Cal scale factors
      names(cal_sfs) <- cals$AptName
      right.tail     <- cal_sfs[cal_sfs > medians[.plate] + 0.4]
      left.tail      <- cal_sfs[cal_sfs < medians[.plate] - 0.4]
      c(left.tail, right.tail)
      # ------- ALTERNATIVE WAY OF SETTING TAIL WEIGHT CRITERION #
      # nolint start
      # cal_sfs <- log2(cal_sfs)
      # cal_sfs <- cal_sfs - median(cal_sfs)
      # cal_sfs[abs(cal_sfs) > log2(1.4)]
      # nolint end
    })

  # which plates > 5%
  lgl <- vapply(tails, function(.x) length(.x) / nrow(cals) > 0.05, NA)
  tail_fails <- cal_names[lgl]

  if ( do_plot ) {
    plural <- ifelse(length(cal_names) > 1L, "s", "")
    p <- list()
    p$median_test <- plot_cal_sf(
      apt_data    = cals,
      ablines     = c(0.8, 1.2),
      by_dilution = FALSE,
      main        = sprintf("Median Test: Median%s within (0.8-1.2)", plural)
    )

    # Now center the distribution on 1 and look at tails
    center_cals <- cals |>
      purrr::map_if(is.numeric, function(.x) .x - median(.x) + 1) |>
      tibble::as_tibble()
    p$tail_test <- plot_cal_sf(center_cals, apt_data = center_cals,
                               ablines = c(0.6, 1.4), by_dilution = FALSE,
                               main = "Tail Test: 95% median +/- 0.4")
  } else {
    p <- NULL
  }

  # not sure why these are here, they don't seem to do anything; sgf
  if ( length(median_fails) > 0L ) {
    lapply(median_fails, function(fail) {
      sprintf("\t%s: %0.4f", fail, medians[fail])
    })
  }
  if ( length(tail_fails) > 0L ) {
    lapply(cal_names, function(.plate) {
      sprintf("\t%s: %0.4f", .plate, length(tails[[.plate]]) / nrow(cals))
    })
  }

  counts     <- lengths(tails)
  percs      <- counts / nrow(cals) * 100
  tail_percs <- rbind(counts, percs) |> t() |> data.frame() |>
    setNames(c("Proteins in Tails", "Percent in Tails"))

  tail_percs[["Median Scale Factor"]] <- medians
  tail_percs[["Median Shift"]] <- 1 - medians
  tail_percs <- tail_percs[, sort(names(tail_percs)) ]

  fails <- setNames(cal_names, cal_names) |>
    lapply(function(.plate) names(tails[[.plate]]))

  if ( verbose ) {
    pad <- 45
    msg <- pad("No. plates failing the median shift criterion (0.8-1.2)", pad)
    signal_todo(msg, value(length(median_fails)))
    msg <- pad("No. plates failing the tail criterion (95%% median +/- 0.4)", pad)
    signal_todo(msg, value(length(tail_fails)))
    msg <- pad("No. SOMAmers failing tail criterion", pad)
    signal_todo(msg, value(length(unique(unlist(fails)))))
  }

  # subset all failed apts
  tail_tbl <- rearrange(cals, "AptName", unique(unlist(fails)))
  # sorting variable
  tail_tbl$sort <- apply(tail_tbl[, cal_names], 1, function(.x) sum(abs(log(.x))))
  tail_tbl      <- dplyr::arrange(tail_tbl, dplyr::desc(sort)) # sort by new variables
  tail_tbl$fail.rank <- seq_len(nrow(tail_tbl))                # create ranks column

  invisible(
    list(cal.table  = tail_percs,
         tail.fails = fails,
         tail.tbl   = tail_tbl,
         plots      = p)
  )
}
