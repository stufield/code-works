
CheckLazyDataCompression <- function(pkg) {
  pkg_name <- sub("_.*", "", pkg) |> basename()
  lib <- tempfile(); dir.create(lib)
  zs  <- c("gzip", "bzip2", "xz")
  res <- integer(3L); names(res) <- zs
  for (z in zs) {
    opts <- c(paste0("--data-compress=", z),
              "--no-libs", "--no-help", "--no-demo", "--no-exec", "--no-test-load")
    install.packages(pkg, lib, INSTALL_opts = opts, repos = NULL, type = "source",
                     quiet = TRUE)
    res[z] <- file.size(file.path(lib, pkg_name, "data", "Rdata.rdb"))
  }
  res / 1024^2
}
size <- CheckLazyDataCompression("~/bitbucket/SomaNormalization/")
size / max(size)
