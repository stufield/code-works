
pkg_version <- function(pkg) {
  .f <- function(...) tryCatch(as.character(utils::packageVersion(...)),
                               error = function(e) NA_character_)
  vapply(.libPaths(), .f, pkg = pkg, "") |>
    package_version(strict = FALSE) |>
    tibble::enframe(name = "lib", value = "version")
}
pkg_version("helpr")
