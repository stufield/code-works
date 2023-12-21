
get_deps <- function(path = ".") {
  dcf <- read.dcf(file = file.path(path, "DESCRIPTION"))
  if ( nrow(dcf) < 1L) {
    stop("DESCRIPTION file of package is corrupt.", call. = FALSE)
  }
  desc <- as.list(dcf[1L, ])[c("Depends", "Imports", "Suggests")]
  lapply(desc, .parse_deps) |>
    dplyr::bind_rows(.id = "type")
}


.parse_deps <- function(deps) {
  deps <- trimws(strsplit(deps, ",")[[1L]])
  deps <- lapply(strsplit(deps, "\\("), trimws)
  deps <- lapply(deps, sub, pattern = "\\)$", replacement = "")
  res <- data.frame(stringsAsFactors = FALSE,
    package = vapply(deps, "[", "", 1L),
    version = vapply(deps, "[", "", 2L)
  )
  res$version <- gsub("\\s+", " ", res$version)
  res$version[is.na(res$version)] <- "*"
  res
}
