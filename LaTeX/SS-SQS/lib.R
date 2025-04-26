# ----- sqs_wrapper documention ----- #
# Figure sizes:
#   Proportional scaling of figure size. Default=1
# @param hyb_fig_scale
# @param med_fig_scale
# @param cal_fig_scale
#
# Legend sizes:
#   Proportional scaling of legend size. Default=1
# @param med.legend.scale
# @param cal.legend.scale
#
# Figure widths:
#   Proportional scaling of figure width. Used for studies with
# many plates to provide room for boxes
# @param hyb_fig_width
# @param med_fig_width
#
# Boxplot x-axis:
#   Jitter labels on x-axis so that they don't overlap
# @param hyb_jitter_x
# @param med_jitter_x
#
# @param skip_sat_check should a saturation check be skipped? (T)
# @param rfu_thresh the threshold for saturation if a check is performed
# @param skip_cal should calibration be skipped? (F)
# @param skip_med should med.norm be skipped? (F)
# @param use_log_scale should plots be made with log2-transform or linear space?
# ----------------------------------- #
sqs_wrapper <- function(hyb_fig_scale = 1, med_fig_scale = 1,
                        cal_fig_scale = 1, med_legend_scale = 1,
                        cal_legend_scale = 1, hyb_fig_width = 1,
                        med_fig_width = 1, hyb_jitter_x = FALSE,
                        med_jitter_x = FALSE, skip_cal = FALSE,
                        skip_med = FALSE, use_log_scale = TRUE,
                        rfu_thresh = 80000, skip_sat_check = TRUE) {

  signal_info("Loading template data ...")
  template_pairs <- parse_template_pairs()

  if ( !"Adat" %in% names(template_pairs) ) {
    stop("Adat missing from `sqs-data.txt`", call. = FALSE)
  } else if ( template_pairs$Adat == "NULL" ) {
    stop("You forgot to enter an ADAT file path!", call. = FALSE)
  }

  signal_info("Loading adat ...")
  adat <- SomaDataIO::read_adat(template_pairs$Adat)
  sample_type_table <- table(adat$SampleType)
  adat <- dplyr::filter(adat, SampleType == "Sample")
  template_pairs$apts <- SomaDataIO::match_seqIds(template_pairs$apts, names(adat))

  if ( nrow(adat) == 0L || !"SampleType" %in% names(adat) ) {
    stop("Error parsing adat on `SampleType` columns", call. = FALSE)
  }
  signal_done("done")

  signal_info("Processing Adat meta data ...")
  adat_pairs <- load_adat_pairs(adat)

  signal_info("Writing sqs-data.tex ...")
  write_sqs_data_tex(template_pairs, adat_pairs, adat, sample_type_table)

  if ( use_log_scale ) {
    cat(sprintf("\\useLogScaletrue\n"), file = "sqs-data.tex", append = TRUE)
  } else {
    cat(sprintf("\\useLogScalefalse\n"), file = "sqs-data.tex", append = TRUE)
  }
  signal_done("done")

  signal_info("Making hyb norm plots ...")
  hyb_fails <- create_hyb_norm_data(adat, hyb_fig_scale,
                                    hyb_fig_width, jitter_x = hyb_jitter_x,
                                    use_log_scale = use_log_scale)
  signal_done("done")

  # check for (cell sample) saturation
  if ( !skip_sat_check ) {
    signal_info("Checking for saturation ...")
    create_saturation_data(adat, threshold = rfu_thresh)
    signal_done("done")
  } else {
    signal_info("Skipping saturation check ...")
  }

  if ( !skip_med ) {
    signal_info("Making med norm plots ...")
    med_fails <- create_med_norm_data(adat, med_fig_scale,
                                      med_legend_scale, med_fig_width,
                                      jitter_x = med_jitter_x,
                                      use_log_scale = use_log_scale)
    signal_done("done")
  }

  if ( !skip_cal ) {
    signal_info("Testing Cal SOP criteria ...")
    create_cal_sop_data(adat, template_pairs, cal_fig_scale, cal_legend_scale)
    signal_done("done")
  }

  if ( "SampleNotes" %in% names(adat) ) {
    signal_info("Writing sample notes table ...")
    sample_notes_table(adat)
    signal_done("done")
  } else {
    signal_info("Skipping sample notes table, no SampleNotes field ...")
  }

  if ( !skip_med ) {
    invisible(c(hyb_fails, med_fails))
  } else {
    invisible(hyb_fails)
  }
}


parse_template_pairs <- function() {
  tokens <- read_text("sqs-params.txt")
  tokens <- gsub("^(.*?)#.*", "\\1", tokens) |>
    keep_it(function(x) x != "") |>
    trimws() |>
    strsplit(":[\t ]*")

  stopifnot(
    "`sqs-params.txt` incorrect format. Should contain `key: value` pairs." =
      all(lengths(tokens) == 2L)
  )

  template_pairs <- lapply(tokens, `[`, x = 2L) |>
    setNames(vapply(tokens, `[`, x = 1L, ""))

  if ( "AptMenu" %in% names(template_pairs) ) {
    if ( template_pairs$AptMenu %in% c("Premium", "450-plex") ) {
      column <- c(Premium = "Premium", "450-plex" = "On450")
      column <- column[template_pairs$AptMenu]
      ss_menu <- readRDS("SSmenu.rds")
      apts <- ss_menu[[column]]
      template_pairs$apts <- apts
    } else {
      stop("`AptMenu` must be either Premium or 450-plex. ",
           "Please check `sqs-params.txt`.", call. = FALSE)
    }
  } else {
    stop("`AptMenu` missing from `sqs-params.txt`", call. = FALSE)
  }
  template_pairs
}

load_adat_pairs <- function(adat) {
  adat_pairs <- list()
  meta_names <- SomaDataIO::getMeta(adat)
  if ( "SampleMatrix" %in% meta_names ) {
    adat_pairs$MatrixName <- tolower(names(sort(table(adat$SampleMatrix),
                                                decreasing = TRUE))[1L])
  }
  adat_pairs$NumSamples <- nrow(adat)
  adat_pairs$NumPlates  <- length(table(adat$PlateId))

  if ( adat_pairs$NumPlates > 1L ) {
    adat_pairs$Sets <- sprintf("Sets A-%s", LETTERS[adat_pairs$NumPlates])
  } else {
    adat_pairs$Sets <- names(table(adat$PlateId))
  }

  adat_pairs
}


write_sqs_data_tex <- function(template_pairs, adat_pairs,
                               adat, sample_type_table) {

  out_file <- file("sqs-data.tex", open = "w")
  on.exit(close(out_file))

  process_pair <- function(template_pairs, adat_pairs, key) {
    cat(sprintf("\\renewcommand{\\%s}{", key), file = out_file)
    if ( key %in% names(template_pairs) ) {
      if ( template_pairs[[key]] == "NULL" ) { # Template is NULL?
        if ( key %in% names(adat_pairs) ) { # Present in Adat?
          cat(sprintf("%s}\n", adat_pairs[[key]]), file = out_file)  # Use Adat
        } else {                       # Not in Adat
          cat(sprintf("%s}\n", key), file = out_file) # Use generic
        }
      } else {   # Template over-ride
        cat(sprintf("%s}\n", gsub("&", "\\\\&", template_pairs[[key]])),
            file = out_file)  # Use template value
      }
    } else { # Not in template
      if ( key %in% names(adat_pairs) ) {
        cat(sprintf("%s}\n", adat_pairs[[key]]), file = out_file)  # Use Adat
      } else {
        cat(sprintf("%s}\n", "MISSING"), file = out_file)  # Use Adat
      }
    }
  }

  c("ClientName", "MatrixName", "Sets",
    "AptMenu", "NumPlates", "NumSamples",
    "SponsorName") |>
    lapply(process_pair, template_pairs = template_pairs,
           adat_pairs = adat_pairs)

  cat(sprintf("\\renewcommand{\\NumApts}{%i}\n",
              length(template_pairs$apts)), file = out_file)

  if ( length(unique(adat$PlateId)) > 1L ) {
    cat("\\renewcommand{\\Plural}{s}\n", file = out_file)
    cat("\\renewcommand{\\IsAre}{are}\n", file = out_file)
  } else {
    cat("\\renewcommand{\\IsAre}{is}\n", file = out_file)
  }

  cat("\\renewcommand{\\NumCalibrators}{", file = out_file)

  if ( "Calibrator" %in% names(sample_type_table) ) {
    cat(sprintf("%i}\n", sample_type_table["Calibrator"]), file = out_file)
  } else {
    cat(sprintf("%i}\n", 0), file = out_file)
  }

  cat("\\renewcommand{\\NumBuffers}{", file = out_file)

  if ( "Buffer" %in% names(sample_type_table) ) {
    cat(sprintf("%i buffer (no protein) control sample%s}\n",
                sample_type_table["Buffer"],
                ifelse(sample_type_table["Buffer"] != 1, "s", "")),
        file = out_file)
  } else {
    cat("0 buffer (no protein) control samples}\n", file = out_file)
  }

  cat("\\renewcommand{\\NumQC}{", file = out_file)

  qcs <- grep("QC", names(sample_type_table), value = TRUE)

  if ( length(qcs) > 1L ) {
    stop(sprintf("Multiple QC sample types: %s", value(qcs)), call. = FALSE)
  } else if ( length(qcs) == 1L ) {
    cat(sprintf("%i}\n", sample_type_table[qcs]), file = out_file)
  } else {
    cat(sprintf("%i}\n", 0), file = out_file)
  }
}


create_hyb_norm_data <- function(adat, fig_scale, fig_width,
                                 jitter_x, use_log_scale) {

  # This is now a ggplot; save with ggsave()
  SomaPlotr::figure("plots/hyb-norm.pdf", 4.4, 5 * fig_width, scale = fig_scale)
  on.exit(SomaPlotr::close_figure("plots/hyb-norm.pdf"))
  tab <- plot_scale_factors(adat, drop_hyb = FALSE, do_cdf = FALSE)
  write_latex(tab, row.names = "Run", file = "tables/hyb-norm.tex")

  hyb_fails <- adat[abs(log(adat$HybControlNormScale, base = 2)) > log(2.5, base = 2), ]

  were_was <- ifelse(nrow(hyb_fails) == 1L, "was", "were")
  plural   <- ifelse(nrow(hyb_fails) == 1L, "", "s")

  cat(sprintf("\\renewcommand{\\NumHybFail}{%s %i sample%s}\n",
              were_was, nrow(hyb_fails), plural),
      file = "sqs-data.tex", append = TRUE)

  if ( nrow(hyb_fails) > 0L ) {
    cat(sprintf("\\HybFailstrue\n"), file = "sqs-data.tex", append = TRUE)
    temp_hyb_fails <- hyb_fails
    row.names(temp_hyb_fails) <- temp_hyb_fails$ExtIdentifier
    write_latex(temp_hyb_fails[, c("PlateId", "SampleId", "HybControlNormScale")],
                file = "tables/hyb-fail.tex", row.names = "ExtIdentifier")
  } else {
    cat(sprintf("\\HybFailsfalse\n"), file = "sqs-data.tex", append = TRUE)
  }

  list(hyb_fails = hyb_fails)
}


create_saturation_data <- function(adat, threshold) {

  # keep only analytes from premium menu
  ss_menu <- readRDS("SSmenu.rds")
  seq_ids <- ss_menu$Premium
  apts  <- SomaDataIO::matchSeqIds(seq_ids, names(adat), order.by.x = FALSE)
  nadat <- adat[, c(SomaDataIO::getMeta(adat), apts)]  # subset only premium menu

  # undo calibration first
  aptdata <- SomaDataIO::getAnalyteInfo(adat)
  aptdata <- aptdata[apts, ] # subset for 1129
  calname <- grep("^Cal", names(aptdata), value = TRUE)

  if ( length(calname) > 0L ) {
    cat(sprintf("  Undoing calibration with factors from %s\n", calname[1L]))
    aptnames <- SomaDataIO::getAnalytes(adat)
    for ( i in aptnames ) {
      nadat[, i] <- adat[, i] / as.numeric(aptdata[calname[1L]][i, 1L])
    }
  }

  # undo median normalization
  # get normalization factor colnames
  # typically 1 dil = lysate
  scales <- sort(grep("^NormScale", SomaDataIO::getMeta(adat), value = TRUE))

  # get a list of aptamers by dilutions
  by_dil <- split(apts, aptdata$Dilution)  # same order as above due to sorting

  if ( length(scales) > 0L ) {
    signal_info("Undoing med norm (by dil)")
    for ( i in seq_along(by_dil) ) {
      nadat[, by_dil[[i]]] <- nadat[, by_dil[[i]]] / nadat[, scales[i]]
    }
  }

  # finally, we can seek those analytes for which
  # *all* measures are above threshold
  apt_mins <- apply(nadat[, apts], 2, min)
  saturated <- names(apt_mins)[apt_mins > threshold]

  # write out variables
  if ( length(saturated) > 0L ) {
    cat(sprintf("\\Saturationtrue\n"), file = "sqs-data.tex", append = TRUE)
    if ( length(saturated) > 1L ) {
      cat(sprintf("\\renewcommand{\\NumSaturation}{were %i SOMAmers}\n",
                  length(saturated)), file = "sqs-data.tex", append = TRUE)
    } else {
      cat("\\renewcommand{\\NumSaturation}{was 1 SOMAmer}\n",
          file = "sqs-data.tex", append = TRUE)
    }

    # write out table
    tbldata <- aptdata[saturated, c("SomaId", "Target", "UniProt", "EntrezGeneSymbol")]
    # some UniProts are comma separated lists, much better to have spaces
    tbldata$UniProt <- gsub(",", " ", tbldata$UniProt)

    # add the minimum RFU values
    min_rfu  <- as.matrix(apply(nadat[, saturated], 2, min))
    colnames(min_rfu) <- "Minimum RFU"
    tbldata <- cbind(tbldata, min_rfu)
    write_latex(tbldata, file = "tables/saturation.tex", write.rownames = FALSE)
  } else {
    cat(sprintf("\\Saturationfalse\n"), file = "sqs-data.tex", append = TRUE)
  }
}


create_med_norm_data <- function(adat, fig_scale, legend_scale,
                                 fig_width, jitter_x, use_log_scale) {

  med_names <- grep("^NormScale", names(adat), value = TRUE)

  if ( length(med_names) == 0L ) {
    stop("No Med Norm Scale factors found")
  }

  # this is now a ggplot; save with ggsave()
  SomaPlotr::figure("plots/med-norm.pdf", 5, 5 * fig_width, scale = fig_scale)
  on.exit(SomaPlotr::close_figure("plots/med-norm.pdf"))
  tab <- plot_scale_factors(adat, drop_hyb = TRUE, do_cdf = FALSE)
  write_latex(tab, row.names = "Run: Dilution", file = "tables/med-norm.tex")

  med_fails <- adat[, c("SampleId", med_names)]

  if ( any(is.na(adat$ExtIdentifier)) ) {
    row.names(med_fails) <- seq_len(nrow(med_fails))
    rn_name <- ""
  } else {
    if ( any(table(adat$ExtIdentifier) > 1L) ) { # if duplicates can't assign rn
      row.names(med_fails) <- as.character(adat$SampleId)
    } else {
      row.names(med_fails) <- as.character(adat$ExtIdentifier)
    }
    rn_name <- "ExtIdentifier"
  }

  med_fails[, 2:ncol(med_fails)] <- apply(as.data.frame(med_fails[, 2:ncol(med_fails)]),
                                          2, function(sf) {
            if ( use_log_scale ) {
              scale_factors_str <- sprintf("%0.3f", log(sf, base = 2))
            } else {
              scale_factors_str <- sprintf("%0.3f", sf)
            }
            scale_factors_str[abs(log(sf, base = 2)) < log(2.5, base = 2)] <- ""
            scale_factors_str
  })

  med_fails <- as.data.frame(med_fails)  # added sgf
  med_fails <- med_fails[apply(med_fails[, 2:ncol(med_fails)], 1,
                               function(row) any(row != "")), ]

  were_was <- ifelse(nrow(med_fails) == 1L, "was", "were")
  plural   <- ifelse(nrow(med_fails) == 1L, "", "s")

  cat(sprintf("\\renewcommand{\\NumMedFail}{%s %i sample%s}\n", were_was,
              nrow(med_fails), plural), file = "sqs-data.tex", append = TRUE)
  cat(sprintf("\\renewcommand{\\NumMedFails}{%i sample%s}\n", nrow(med_fails),
              plural), file = "sqs-data.tex", append = TRUE)

  med_fails <- med_fails[order(med_fails$SampleId), ]

  if ( nrow(med_fails) > 0L ) {
    cat(sprintf("\\MedFailstrue\n"), file = "sqs-data.tex", append = TRUE)
    names(med_fails)[2:ncol(med_fails)] <- sapply(names(med_fails)[2:ncol(med_fails)],
                                                  function(nm) gsub("[.]", " ", nm))
    names(med_fails)[2:ncol(med_fails)] <- sapply(names(med_fails)[2:ncol(med_fails)],
                                                 function(nm) gsub("$", "\\\\%", nm))
    names(med_fails)[2:ncol(med_fails)] <- sapply(names(med_fails)[2:ncol(med_fails)],
                                                 function(nm) gsub(" 005", " 0.005", nm))
    names(med_fails)[2:ncol(med_fails)] <- sapply(names(med_fails)[2:ncol(med_fails)],
                                                 function(nm) gsub(" 05", " 0.05", nm))
    names(med_fails)[2:ncol(med_fails)] <- sapply(names(med_fails)[2:ncol(med_fails)],
                                                 function(nm) gsub("2.5", "2.5", nm))
    names(med_fails)[2:ncol(med_fails)] <- sapply(names(med_fails)[2:ncol(med_fails)],
                                                 function(nm) gsub("^NormScale ", "", nm))
    names(med_fails)[2:ncol(med_fails)] <- sprintf("%s Dilution",
                                                   names(med_fails)[2:ncol(med_fails)])
    med_fails <- med_fails[, c("SampleId", sort(names(med_fails)[2:ncol(med_fails)]))]

    if ( all(rownames(med_fails) == med_fails$SampleId) && ncol(med_fails) > 2L ) {
       med_fails <- med_fails[, setdiff(names(med_fails), "SampleId")]
    } else {
      med_fails <- as.data.frame(med_fails[, -grep("SampleId", names(med_fails))],
                                 row.names = rownames(med_fails)) |>
        setNames(setdiff(names(med_fails), "SampleId"))
    }

    write_latex(med_fails, file = "tables/med_fail.tex", row.names = rn_name)

  } else {
    cat(sprintf("\\MedFailsfalse\n"), file = "sqs-data.tex", append = TRUE)
  }

  list(med_fails = med_fails)

}


create_cal_sop_data <- function(adat, template_pairs, fig_scale, legend_scale) {

  seqid_matches <- SomaDataIO::getSeqIdMatches(SomaDataIO::getAnalytes(adat),
                                               template_pairs$apts)
  apts  <- seqid_matches[, 1L]
  adat2 <- adat[, c(SomaDataIO::getMeta(adat), apts)]
  apt_data <- SomaDataIO::getAnalyteInfo(adat)
  apt_data <- apt_data[apts, ]

  sop_data <- sop_calibration(adat2, apt.data = apt_data)
  # this is now a ggplot; save with ggsave()
  # save plots here with: sop_data$plots
  SomaPlotr::figure("plots/cal-sop.pdf", 2.9 * 1.6, 5.8 * 1.6, scale = fig_scale)

  tail_apts <- unique(unlist(sop_data$tail_fails))
  new_rn <- vapply(row.names(sop_data$cal_table), function(name) {
    q <- 0
    q <- regexpr("Set.[A-Z]", name)
    if ( length(q[[1L]]) != 1L ) {
      name
    } else {
      gsub("[_.]", " ", substr(name, q, attributes(q)$match.length + q - 1))
    }
  }, "")

  sop_data$cal_table <- set_rn(sop_data$cal_table, new_rn)

  tail_data <- apt_data[tail_apts, c("SomaId", "Target", "UniProt", "EntrezGeneSymbol")]
  tail_data <- set_rn(tail_data, tail_data$SomaId)
  tail_data <- tail_data[, setdiff(names(tail_data), "SomaId")]

  write_latex(sop_data$cal_table, file = "tables/cal-sop.tex", row.names = "Run")
  write_latex(tail_data, file = "tables/tail-apts.tex", row.names = "SomaId")
  were_was <- ifelse(nrow(tail_data) == 1L, "was", "were")
  plural   <- ifelse(nrow(tail_data) == 1L, "", "s")

  cat(sprintf("\\renewcommand{\\NumCalFail}{%s %i analyte%s}\n",
              were_was, nrow(tail_data), plural),
      file = "sqs-data.tex", append = TRUE)
  if ( nrow(tail_data) > 0L ) {
    cat(sprintf("\\CalFailstrue\n"), file = "sqs-data.tex", append = TRUE)
  }
}


sample_notes_table <- function(adat) {

  if ( all(is.na(adat$SampleNotes)) ) {
    cat("\\showSampleNotesfalse", file = "sqs-data.tex", append = TRUE)
    return()
  }

  tab <- adat[adat$SampleNotes != "", c("ExtIdentifier", "SampleId", "SampleNotes")]
  tab <- tab[order(tab$SampleNotes), ]

  if ( nrow(tab) > 0L ) {
    tab <- tab[order(tab$SampleId), ]
    row.names(tab) <- seq_len(nrow(tab))

    if ( all(as.character(tab$ExtIdentifier) == as.character(tab$SampleId)) ) {
      tab <- tab[, -2L]
    }

    names(tab)[names(tab) == "SampleNotes"] <- "SampleAppearance"
    write_latex(tab, row.names = "", file = "tables/sample-notes.tex")
    cat("\\showSampleNotestrue", file = "sqs-data.tex", append = TRUE)
  } else {
    cat("\\showSampleNotesfalse", file = "sqs-data.tex", append = TRUE)
  }
}


norm_scale_factors <- function(adat, group, file = NULL) {

  pars <- c(par_def, list(mfrow = c(1L, 3L)))
  withr::local_par(list(pars))

  if ( all(group %in% names(adat)) ) {
    group <- do.call(paste, as.list(adat[, group]))
  } else if ( length(group) != nrow(adat) ) {
    stop("Bad group", call. = FALSE)
  }

  SomaPlotr::figure(file, 3, 9)
  withr::defer(SomaPlotr::close_figure(file))

  boxplot(split(log2(adat$NormScale.005), group),
          main = "Median Normalization: 0.005%",
          ylab = expression(log[2](Normalization ~ Scale ~ Factor)),
          xlab = "Study", ylim = ylim)
  boxplot(split(log2(adat$NormScale.1), group),
          main = "Median Normalization: 1%",
          ylab = expression(log[2](Normalization ~ Scale ~ Factor)),
          xlab = "Study", ylim = ylim)
  boxplot(split(log(adat$NormScale.40), group),
          main = "Median Normalization: 40%",
          ylab = expression(log[2](Normalization ~ Scale ~ Factor)),
          xlab = "Study", ylim = ylim)
}


write_latex <- function(data, file, row_names = "Feature",
                        append = FALSE, long = FALSE,
                        caption = NULL, ...) {
  if ( long ) {
    table <- "longtable"
  } else {
    table <- "tabular"
  }

  cols <- c("l", rep("r", ncol(data)))

  data <- data |>
    set_rn(sub("_", "\\_", rownames(mtcars), fixed = TRUE))

  header <- sprintf("\\hline\n\\textbf{%s} & \\textbf{%s} \\\\\n\\hline\n",
                    row_names, paste(colnames(data), collapse = "} & \\textbf{"))

  cat(sprintf("\\begin{%s}{", table), cols, "}\n", sep = "",
      file = file, append = append)

  if ( !is.null(caption) ) {
    sprintf("\\multicolumn{%i}{c}\n{\\small \\textbf{\\tablename\\ \\thetable{} -- %s}} \\\\\n",
            length(cols), caption) |>
      cat(append = TRUE, file = file)
  }

  cat(header, append = TRUE, file = file)

  if ( long ) {
    cat("\\endfirsthead\n\n", append = TRUE, file = file)
    sprintf("\\multicolumn{%i}{c}\n{{\\tablename\\ \\thetable{} -- continued from previous page}} \\\\\n",
            length(cols)) |>
      cat(append = TRUE, file = file)
    cat(header, append = TRUE, file = file)
    cat("\\endhead\n\n", append = TRUE, file = file)
  }
  for ( i in seq_len(ncol(data)) ) {
    if ( is.numeric(data[, i]) ) {
      data[, i] <- format(data[, i], nsmall = 2L, digits = 2L,
                          scientific = abs(min(data[, i])) < 0.01)
    }
  }
  write.table(data, file = file, sep = " & ",
              quote = FALSE, append = TRUE,
              col.names = FALSE, row.names = TRUE, eol = "\\\\\n", ...)
  cat(sprintf("\\hline\n\\end{%s}\n", table), append = TRUE, file = file)
}


get_norms <- function(adat, drop_hyb = TRUE) {
  nms <- grep("^NormScale|^Med\\.Scale\\.|^HybControlNorm",
              names(adat), value = TRUE)
  if ( drop_hyb ) {
    nms <- grep("HybControlNorm", nms,
                value = TRUE, invert = TRUE)
  }
  nms
}


par_def <- list(mgp = c(2.00, 0.75, 0.00), mar = c(3L, 4L, 3L, 1L))


col_string <- c("steelblue", "red", "darkgreen", "darkorchid4",
                "cyan", "orange", "black", "grey", "#990066", "green",
                "#24135F")


seq_HybControlElution <- c("2171-12", "2178-55", "2194-91",
                           "2229-54", "2249-25", "2273-34",
                           "2288-7", "2305-52", "2312-13",
                           "2359-65", "2430-52", "2513-7")


seq_Spuriomer <- c("2052-1", "2053-2", "2054-3", "2055-4",
                   "2056-5", "2057-6", "2058-7", "2060-9",
                   "2061-10", "4666-193", "4666-194", "4666-195",
                   "4666-199", "4666-200", "4666-202", "4666-205",
                   "4666-206", "4666-212", "4666-213", "4666-214")

