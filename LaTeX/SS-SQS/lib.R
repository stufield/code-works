# ----- sqs_wrapper documention ----- #
# Figure sizes:
#   Proportional scaling of figure size. Default=1
# @param hyb.fig.scale
# @param med.fig.scale
# @param cal.fig.scale
#
# Legend sizes:
#   Proportional scaling of legend size. Default=1
# @param med.legend.scale
# @param cal.legend.scale
#
# Figure widths:
#   Proportional scaling of figure width. Used for studies with
#   many plates to provide room for boxes
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
sqs_wrapper <- function(hyb.fig.scale=1, med.fig.scale=1, cal.fig.scale=1,
                        med.legend.scale=1, cal.legend.scale=1,
                        hyb.fig.width=1, med.fig.width=1,
                        hyb.jitter.x=FALSE, med.jitter.x=FALSE, skip_cal=FALSE, skip.med=FALSE,
                        use.log.scale=TRUE, rfu.thresh=80000, skip.sat.check=TRUE) {

  cat("Loading template data ...")
  template.pairs <- load.template.pairs()
  cat("      done\n")

  if (! "Adat" %in% names(template.pairs))
    stop("Adat missing from sqs-data.txt")
  else if (template.pairs[["Adat"]] == "NULL")
    stop("You forgot to enter an Adat filename")

  cat("Loading adat ...")
  adat = load.adat(template.pairs[["Adat"]], remove.buffer=FALSE)
  sample.type.table = table(adat$SampleType)
  adat = adat[ adat$SampleType == "Sample", ]
  template.pairs[['apts']] = match.seq.ids(template.pairs[['apts']], names(adat))



  if ( nrow(adat) == 0L | !"SampleType"  %in% names(adat) ) {
    stop("Error parsing adat on SampleType columns")
  }
  cat("               done\n")
  cat("Processing Adat meta data ...")
  adat.pairs = load.adat.pairs(adat)
  cat("  done\n")
  cat("Writing sqs-data.tex ...")
  write.sqs.data.tex(template.pairs, adat.pairs, adat, sample.type.table)

  if (use.log.scale)
    cat(sprintf("\\useLogScaletrue\n"), file="sqs-data.tex", append=TRUE)
  else
    cat(sprintf("\\useLogScalefalse\n"), file="sqs-data.tex", append=TRUE)
  cat("       done\n")

  cat("Making hyb norm plots ...")
  hyb.fails = create.hyb.norm.data(adat, hyb.fig.scale, hyb.fig.width,
                        jitter.x=hyb.jitter.x, use.log.scale=use.log.scale)
  cat("      done\n")

  # check for (cell sample) saturation
  if (!skip.sat.check) {
    cat("Checking for saturation ...\n")
    create.saturation.data(adat, threshold=rfu.thresh)
    cat("                               done\n")
  } else
    cat("Skipping saturation check .........\n")

  if ( !skip.med ) {
    cat("Making med norm plots ...")
    med.fails = create.med.norm.data(adat, med.fig.scale, med.legend.scale, med.fig.width,
                          jitter.x=med.jitter.x, use.log.scale=use.log.scale)
    cat("      done\n")
  }


  if ( !skip_cal ) {
    cat("Testing Cal SOP criteria ...\n")
    create.cal.sop.data(adat, template.pairs, cal.fig.scale, cal.legend.scale)
    cat("                              done\n")
  }


  if ("SampleNotes" %in% names(adat)) {
    cat("Writing sample notes table ...")
    sample.notes.table(adat)
    cat(" done\n")
  }
  else
    cat("Skipping sample notes table, no SampleNotes field\n")

  if ( !skip.med )
    invisible(c(hyb.fails, med.fails))
  else
    invisible(hyb.fails)

}


load.template.pairs <- function() {
  tokens <- readLines("sqs-data.txt")
  tokens <- sapply(tokens, function(x) gsub("#.*$", "", x))    # Remove comments
  tokens <- strsplit(tokens, ":")
  tokens <- tokens[ sapply(tokens, function(x) length(x) > 0) ]    # Remove empty strings at end of file
  tokens <- tokens[ !sapply(tokens, function(x) length(x) == 1 && x[1] == "") ]      # Remove empty stings
  template.pairs.temp = lapply(tokens, function(pair) {
    if (length(pair) != 2)
      stop(sprintf("sqs-data.txt is in an incorrect format. Should contain 'key: value' pairs.\nFound '%s'", paste(pair, collapse=", ")))
    key = gsub("^[\t ]+|[\t ]+$", "", pair[1])
    value= gsub("^[\t ]+|[\t ]+$", "", pair[2])
    c(key, value)
  })

  template.pairs <- lapply(template.pairs.temp, function(pair) pair[2])
  names(template.pairs) <- lapply(template.pairs.temp, function(pair) pair[1])
  if ("AptMenu" %in% names(template.pairs)) {
    if (template.pairs[['AptMenu']] %in% c("Premium", "450plex")) {
      column = list(Premium="Premium", "450plex"="On450")[[template.pairs[['AptMenu']]]]
      apts = SSmenu.df[ SSmenu.df[,column] == 'Y',]$SeqId
      template.pairs[["apts"]] = apts
    }
    else
      stop("AptMenu must be either Premium or 450plex. Please check sqs-data.txt")
  }
  else
    stop("AptMenu missing from sqs-data.txt")
  template.pairs

}






load.adat.pairs <- function(adat) {
  adat.pairs <- list()
  meta.names <- get.meta(adat)
  if ("SampleMatrix" %in% meta.names)
    adat.pairs[["MatrixName"]] = tolower(names(sort(table(adat$SampleMatrix), decreasing=TRUE))[1])
  adat.pairs[["NumSamples"]] = nrow(adat)
  adat.pairs[["NumPlates"]] = length(table(adat$PlateId))
  if (adat.pairs[["NumPlates"]] > 1)
    adat.pairs[["Sets"]] = sprintf("Sets A-%s", LETTERS[adat.pairs[["NumPlates"]]])
  else
    adat.pairs[["Sets"]] = names(table(adat$PlateId))

  adat.pairs


}





write.sqs.data.tex <- function(template.pairs, adat.pairs, adat, sample.type.table) {
  out.file <- file("sqs-data.tex", open="w")
  on.exit(close(out.file))
  process.pair = function(template.pairs, adat.pairs, key) {
    cat(sprintf("\\renewcommand{\\%s}{", key), file=out.file)
    if (key %in% names(template.pairs)) {
      if (template.pairs[[key]] == "NULL") {              # Template is NULL?
        if (key %in% names(adat.pairs))                # Present in Adat?
          cat(sprintf("%s}\n", adat.pairs[[key]]), file=out.file)  # Use Adat
        else                            # Not in Adat
          cat(sprintf("%s}\n", key), file=out.file)         # Use generic
      }
      else {                                                           # Template over-ride
        cat(sprintf("%s}\n", gsub("&", "\\\\&", template.pairs[[key]])), file=out.file)  # Use template value
      }
    }
    else {                                # Not in template
      if (key %in% names(adat.pairs))
          cat(sprintf("%s}\n", adat.pairs[[key]]), file=out.file)  # Use Adat
      else
        cat(sprintf("%s}\n", "MISSING"), file=out.file)  # Use Adat
    }

  }

  lapply(c("ClientName", "MatrixName", "Sets", "AptMenu", "NumPlates", "NumSamples", "SponsorName"), process.pair, template.pairs=template.pairs, adat.pairs=adat.pairs)

  cat(sprintf("\\renewcommand{\\NumApts}{%i}\n", length(template.pairs[["apts"]])), file=out.file)
  if (length(unique(adat$PlateId)) > 1) {
    cat("\\renewcommand{\\Plural}{s}\n", file=out.file)
    cat("\\renewcommand{\\IsAre}{are}\n", file=out.file)
  }
  else
    cat("\\renewcommand{\\IsAre}{is}\n", file=out.file)


  cat("\\renewcommand{\\NumCalibrators}{", file=out.file)
  if ("Calibrator" %in% names(sample.type.table))
    cat(sprintf("%i}\n", sample.type.table["Calibrator"]), file=out.file)
  else
    cat(sprintf("%i}\n", 0), file=out.file)


  cat("\\renewcommand{\\NumBuffers}{", file=out.file)
  if ("Buffer" %in% names(sample.type.table))
    cat(sprintf("%i buffer (no protein) control sample%s}\n", sample.type.table["Buffer"], if (sample.type.table["Buffer"] != 1) "s" else ""), file=out.file)
  else
    cat(sprintf("0 buffer (no protein) control samples}\n", 0), file=out.file)

  cat("\\renewcommand{\\NumQC}{", file=out.file)
  qcs = grep("QC", names(sample.type.table), value=T)
  if (length(qcs) > 1)
    stop(sprintf("Multiple QC sample types: %s", paste(qcs, collapse=",")))
  else if (length(qcs) == 1)
    cat(sprintf("%i}\n", sample.type.table[qcs]), file=out.file)
  else
    cat(sprintf("%i}\n", 0), file=out.file)

}





create.hyb.norm.data <- function(adat, fig.scale, fig.width, jitter.x, use.log.scale) {
  figure("plots/hyb-norm.pdf", 4.4, 5*fig.width, scale=fig.scale)
  par(mar=c(3.2, 4, 3, 2))
  tab <- plot.scale.factors(adat, hyb=TRUE, scale=fig.scale, as.boxplot=T, jitter.x=jitter.x, use.log.scale=use.log.scale)
  close_figure("plots/hyb-norm.pdf")
  write.latex(tab, row.names="Run", file='tables/hyb-norm.tex')

  hyb.fails <- adat[abs(log(adat$HybControlNormScale, base=2)) > log(2.5, base=2),]

  if (nrow(hyb.fails) == 1) {
    were.was = "was"
    plural = ""
  }
  else {
    were.was = "were"
    plural = "s"
  }

  cat(sprintf("\\renewcommand{\\NumHybFail}{%s %i sample%s}\n", were.was, nrow(hyb.fails), plural), file="sqs-data.tex", append=TRUE)
  #cat(sprintf("\\renewcommand{\\NumHybFail}{%i}\n", nrow(hyb.fails)), file="sqs-data.tex", append=TRUE)

  if (nrow(hyb.fails) > 0L) {
    cat(sprintf("\\HybFailstrue\n"), file="sqs-data.tex", append=TRUE)
    temp.hyb.fails = hyb.fails
    row.names(temp.hyb.fails) = temp.hyb.fails$ExtIdentifier
    write.latex(temp.hyb.fails[,c("PlateId", "SampleId", "HybControlNormScale")], file='tables/hyb-fail.tex', row.names="ExtIdentifier")
  }
  else
    cat(sprintf("\\HybFailsfalse\n"), file="sqs-data.tex", append=T)

  list(hyb.fails=hyb.fails)
}


create.saturation.data <- function(adat, threshold) {

  # keep only SOMAmers in the premium menu
  seq.ids <- SSmenu.df$SeqId[SSmenu.df$Premium == "Y"]
  apts <- match.seq.ids(seq.ids, names(adat), order.by.first=FALSE)
  nadat <- adat[, c(get.meta(adat), apts)]      # subset only premium menu

  # recreate hyb normalized (only) data
  #  **** FUTURE ITERATIONS WILL PROVIDE THE HYB DATA DIRECTLY ****

  # undo calibration first
  aptdata <- get.apt.data(adat)
  aptdata <- aptdata[ apts, ]        # subset for 1129
  calname <- grep("^Cal", names(aptdata), value=TRUE)

  if (length(calname) > 0) {
    cat(sprintf("  Undoing calibration with factors from %s\n",calname[1]))
    aptnames <- get.aptamers(adat)
    for (i in aptnames)
      nadat[,i] <- adat[,i] / as.numeric(aptdata[calname[1]][i,1])
  }

  # undo median normalization
  # get normalization factor colnames
  scales <- sort(grep("^NormScale", get.meta(adat), val=TRUE))   # typically 1 dil for cell lysate

  # get a list of aptamers by dilutions
  by.dil <- split(apts, aptdata$Dilution)                # same order as above due to sorting

  if (length(scales) > 0) {
    cat("  Undoing med norm (by dil)\n")
    for (i in 1:length(by.dil))
      nadat[,by.dil[[i]]] <- nadat[,by.dil[[i]]] / nadat[,scales[i]]
  }

  # finally, we can seek those SOMAmers for which *all* measures are above threshold
  apt.mins <- apply(nadat[,apts], 2, min)
  saturated <- names(apt.mins)[apt.mins > threshold]
  #print(saturated)

  # write out variables
  if (length(saturated) > 0) {
    cat(sprintf("\\Saturationtrue\n"), file="sqs-data.tex", append=TRUE)
    if (length(saturated)>1)
      cat(sprintf("\\renewcommand{\\NumSaturation}{were %i SOMAmers}\n", length(saturated)), file="sqs-data.tex", append=TRUE)
    else
      cat("\\renewcommand{\\NumSaturation}{was 1 SOMAmer}\n", file="sqs-data.tex", append=TRUE)

    # write out table
    tabledata <- aptdata[saturated,c("SomaId", "Target", "UniProt", "EntrezGeneSymbol")]
    # some UniProts are comma separated lists, much better to have spaces
    tabledata$UniProt <- gsub(","," ",tabledata$UniProt)
    # add the minimum RFU values
    min_rfu  <- as.matrix(apply(nadat[,saturated],2,min))
    colnames(min_rfu) <- "Minimum RFU"
    tabledata <- cbind(tabledata, min_rfu)
    write.latex(tabledata, file="tables/saturation.tex", write.rownames=FALSE)
  } else
    cat(sprintf("\\Saturationfalse\n"), file="sqs-data.tex", append=TRUE)
}


create.med.norm.data <- function(adat, fig.scale, legend.scale, fig.width, jitter.x, use.log.scale) {
  med.names <- grep("^NormScale", names(adat), value=T)
  if (length(med.names) == 0)
    stop("No Med Norm Scale factors found")
  figure("plots/med-norm.pdf", 5, 5*fig.width, scale=fig.scale)
  tab <- plot.scale.factors(adat, hyb=FALSE, as.boxplot=TRUE, scale=fig.scale, legend.cex=legend.scale, jitter.x=jitter.x, use.log.scale=use.log.scale)
  close_figure("plots/med-norm.pdf")
  write.latex(tab, row.names="Run: Dilution", file='tables/med-norm.tex')

  med.fails <- adat[,c("SampleId", med.names)]

  if (any(is.na(adat$ExtIdentifier))) {
    row.names(med.fails) = 1:nrow(med.fails)
    rn.name = ""
  }
  else {
    if ( any(table(adat$ExtIdentifier) > 1) )      # if duplicates; can't assign rownames
      row.names(med.fails) = as.character(adat$SampleId)
    else
      row.names(med.fails) = as.character(adat$ExtIdentifier)
    rn.name = "ExtIdentifier"
  }

  med.fails[,2:ncol(med.fails)] <- apply(as.data.frame(med.fails[,2:ncol(med.fails)]), 2, function(scale.factors){
    #c("", "X")[1+as.numeric(abs(log(scale.factors, base=2)) > log(2.5, base=2))]
    if (use.log.scale) {
      scale.factors.str = sprintf("%0.3f", log(scale.factors, base=2))
    } else {
      scale.factors.str = sprintf("%0.3f", scale.factors)
    }
    scale.factors.str[abs(log(scale.factors, base=2)) < log(2.5, base=2)] = ""
    scale.factors.str
  })

  med.fails <- as.data.frame(med.fails)  # added sgf
  med.fails <- med.fails[ apply(med.fails[,2:ncol(med.fails)], 1, function(row) any(row != "")), ]

  if (nrow(med.fails) == 1) {
    were.was = "was"
    plural = ""
  }
  else {
    were.was = "were"
    plural = "s"
  }

  cat(sprintf("\\renewcommand{\\NumMedFail}{%s %i sample%s}\n", were.was, nrow(med.fails), plural), file="sqs-data.tex", append=T)
  cat(sprintf("\\renewcommand{\\NumMedFails}{%i sample%s}\n", nrow(med.fails), plural), file="sqs-data.tex", append=TRUE)

  # order med.fails by sampleID (sgf)
  med.fails <- med.fails[ order(med.fails$SampleId), ]


  if ( nrow(med.fails) > 0 ){
    cat(sprintf("\\MedFailstrue\n"), file="sqs-data.tex", append=TRUE)
    names(med.fails)[2:ncol(med.fails)] = sapply(names(med.fails)[2:ncol(med.fails)], function(name) gsub("[.]", " ", name))
    names(med.fails)[2:ncol(med.fails)] = sapply(names(med.fails)[2:ncol(med.fails)], function(name) gsub("$", "\\\\%", name))
    names(med.fails)[2:ncol(med.fails)] = sapply(names(med.fails)[2:ncol(med.fails)], function(name) gsub(" 005", " 0.005", name))
    names(med.fails)[2:ncol(med.fails)] = sapply(names(med.fails)[2:ncol(med.fails)], function(name) gsub(" 05", " 0.05", name))
    names(med.fails)[2:ncol(med.fails)] = sapply(names(med.fails)[2:ncol(med.fails)], function(name) gsub("2.5", "2.5", name))
    names(med.fails)[2:ncol(med.fails)] = sapply(names(med.fails)[2:ncol(med.fails)], function(name) gsub("^NormScale ", "", name))
    names(med.fails)[2:ncol(med.fails)] = sprintf("%s Dilution", names(med.fails)[2:ncol(med.fails)])

    med.fails = med.fails[,c("SampleId", sort(names(med.fails)[2:ncol(med.fails)]))]   # reorder columns of dilutions: sgf

    if ( all(rn(med.fails) == med.fails$SampleId) && ncol(med.fails) > 2 )   # added sgf
       med.fails = med.fails[, setdiff(names(med.fails), "SampleId")]
    else   # added sgf
      med.fails = as.data.frame(med.fails[, -grep("SampleId", names(med.fails))], row.names=rn(med.fails)) %names% setdiff(names(med.fails), "SampleId")

    write.latex(med.fails, file="tables/med_fail.tex", row.names=rn.name)

  }
  else
    cat(sprintf("\\MedFailsfalse\n"), file="sqs-data.tex", append=T)

  list(med.fails=med.fails)

}


create.cal.sop.data <- function(adat, template.pairs, fig.scale, legend.scale) {

  seq.id.matches <- get.seq.ids.matches(get.aptamers(adat), template.pairs[['apts']])
  apts <- seq.id.matches[,1]
  adat2 <- adat[,c(get.meta(adat), apts)]
  apt.data <- get.apt.data(adat)
  apt.data <- apt.data[apts,]

  figure("plots/cal-sop.pdf", 2.9*1.6, 5.8*1.6, scale=fig.scale)
  sop.data <- sop.calibration(adat2, apt.data=apt.data, legend.cex=legend.scale)
  close_figure("plots/cal-sop.pdf")
  tail.apts <- unique(unlist(sop.data$tail.fails))
  new.rn <- sapply(row.names(sop.data$cal.table), function(name) {
        q=0
        q = regexpr("Set.[A-Z]", name)
        if (length(q[[1]]) != 1 )
            name
        else
            gsub("[_.]", " ", substr(name, q, attributes(q)$match.length+q-1))
      })

  row.names(sop.data$cal.table) = new.rn

  tail.data <- apt.data[tail.apts,c("SomaId", "Target", "UniProt", "EntrezGeneSymbol")]
  row.names(tail.data) <- tail.data$SomaId
  tail.data <- tail.data[,setdiff(names(tail.data), "SomaId")]

  write.latex(sop.data$cal.table, file='tables/cal-sop.tex', row.names="Run")
  write.latex(tail.data, file='tables/tail-apts.tex', row.names="SomaId")
  if (nrow(tail.data) == 1) {
    were.was = "was"
    plural = ""
  }
  else {
    were.was = "were"
    plural = "s"
  }

  cat(sprintf("\\renewcommand{\\NumCalFail}{%s %i analyte%s}\n", were.was, nrow(tail.data), plural), file='sqs-data.tex', append=T)
  if (nrow(tail.data) > 0)
    cat(sprintf("\\CalFailstrue\n"), file="sqs-data.tex", append=T)
}


sample.notes.table <- function(adat){

  if (all(is.na(adat$SampleNotes))) {
    cat("\\showSampleNotesfalse", file="sqs-data.tex", append=T)
    return()
  }

  tab <- adat[adat$SampleNotes != "", c("ExtIdentifier", "SampleId", "SampleNotes")]
  tab <- tab[order(tab$SampleNotes), ]

  if ( nrow(tab) > 0L ) {
    tab <- tab[order(tab$SampleId), ]
    row.names(tab) = 1:nrow(tab)

    if ( all(as.character(tab$ExtIdentifier) == as.character(tab$SampleId)) ) {
      tab <- tab[, -2L]
    }

    names(tab)[names(tab) == "SampleNotes"] = "SampleAppearance"
    write.latex(tab, row.names="", file='tables/sample-notes.tex')
    cat("\\showSampleNotestrue", file="sqs-data.tex", append=T)
  } else {
    cat("\\showSampleNotesfalse", file="sqs-data.tex", append=T)
  }
}


norm.scale.factors <- function(adat, group, filename=NULL) {
  if (all(group %in% names(adat)))
    group <- do.call(paste, as.list(adat[,group]))
  else if (length(group) != nrow(adat))
    stop("Bad group")
  #med.sfs = grep("^NormScale", names(adat))
  figure(filename, 3, 9)
  par(mfrow=c(1,3))
  par(par.def)
  boxplot(split(log2(adat$NormScale.005), group), main="Median Normalization: 0.005%",
        ylab=expression(log[2](Normalization~Scale~Factor)), xlab="Study", ylim=ylim)
  boxplot(split(log2(adat$NormScale.1), group), main="Median Normalization: 1%",
        ylab=expression(log[2](Normalization~Scale~Factor)), xlab="Study", ylim=ylim)
  boxplot(split(log(adat$NormScale.40), group), main="Median Normalization: 40%",
        ylab=expression(log[2](Normalization~Scale~Factor)), xlab="Study", ylim=ylim)
  close_figure(filename)
}


