f <- "~/SomaDataIO/inst/example/example_data.adat"
system.time(x <- SomaReadr::read_adat(f))
skip <- attr(x, "file.specs")$data.begin
f1 <- function() {
  x <- data.frame(
    readr::read_delim(f, delim = "\t", progress = FALSE,
                      col_types = paste0(strrep("?", 35), strrep("d", 5284)),
                      col_names = FALSE, skip = skip)
  )
  names(x) <- sub("X", "rfu", names(x))
  x[[2]] <- as.character(x[[2]])
  x
}
f2 <- function() {
  x <- data.frame(
    utils::read.delim(f, sep = "\t", row.names = NULL,
                      as.is = TRUE, skip = skip,
                      colClasses = c(rep(NA, 35), rep("numeric", 5284)),
                      encoding = "UTF-8", header = FALSE)
  )
  names(x) <- sub("V", "rfu", names(x))
  x[[10]][x[[10]]==""] <- NA
  x[[16]][x[[16]]==""] <- NA
  x[[34]][x[[34]]==""] <- NA
  x
}
bench::mark(
  readr_read_delim = f1(),
  utils_read.delim = f2()
)
