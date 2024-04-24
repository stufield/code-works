# --------------------------------
# Generate depenedency graphs for
# each of the `somaverse` packages.
# The 'somaverse' itself is run as
# one-off pkg via Makefile.
# MUST set $R_SOMAVERSE variable
# usage:
#   make all
#   make networks
#   make somaverse
# --------------------------------
dev_lib <- tempfile("lib-")
dir.create(dev_lib)                 # create tmp library location
.libPaths(c(dev_lib, .libPaths()))  # push dev_lib to top of stack

pkgs <- c(
  "SomaReadr",               # core
  "SomaGlobals"             # core
  "SomaPlyr",                # core
  "SomaPlot",                # core
  "SomaClassify",            # core
  "SomaSurvival",            # core
  "palantir",                # core
  "SomaNormalization",
  "SomaStabilitySelection",
  "SomaMixedEffects",
  "SomaPCA",
  "SomaPipeline",
  "SomaModelAssessment"
)

rootdir <- gsub("/+$", "", Sys.getenv("R_SOMAVERSE"))

invisible(
  lapply(pkgs, function(.pkg) { 
    pkg_path <- file.path(rootdir, .pkg)
    base::system2(
      command = "R",
      args    = c("CMD", "INSTALL", "--use-vanilla", paste0("--library=", dev_lib),
                  "--resave-data", "--with-keep.source", pkg_path, "> .tmp 2>&1"),

      stdout  = NULL
    )
    pkgnet::CreatePackageReport(
      pkg_name    = .pkg,
      pkg_path    = pkg_path,
      report_path = paste0(.pkg, ".html")
    )
  })
)

unlink(c(dev_lib, ".tmp"), force = TRUE)
