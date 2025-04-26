#' Get Calibration Scale Factors
#'
#' This function is a utility used to generate a table (`tibble`)
#' of the calibration scale factors for each plate/run
#' for a given ADAT. Alternatively, an `apt_data` object
#' corresponding to the ADAT can be passed.
#'
#' @export
get_cal_sf <- function(adat = NULL, apt_data = NULL) {

  if ( is.null(apt_data) ) {
    if ( is.null(adat) ) {
      stop(
        "Must provide either `adat` or `apt_data` to `getCalSFs()`",
        call. = FALSE
      )
    }
    apt_data <- SomaDataIO::getAnalyteInfo(adat)
  }

  # this grep needs fixing for PX normalized adats
  plate_fields <- grep("^Cal.Set", names(apt_data), value = TRUE)

  if ( length(plate_fields) == 0L ) {
    stop(
      "No calibration scale factors found in `apt_data` and/or `Col.Meta`",
      call. = FALSE
    )
  }

  # must ungroup the Diltuion
  dplyr::ungroup(apt_data)[, c("AptName", plate_fields, "Dilution")]
}
