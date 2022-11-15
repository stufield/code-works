check_libmasks <- function() {
  libs <- .libPaths()
  lib1 <- libs[1L]
  lib2 <- libs[2L]
  newlibs <- dir(lib1)
  is_lib <- vapply(newlibs, function(.x)
    #all(c("NAMESPACE", "DESCRIPTION", "Meta") %in% dir(file.path(lib1, .x))),
    length(dir(file.path(lib1, .x), pattern = "Meta")) > 0,
    NA)
  newlibs <- newlibs[is_lib]
  if ( length(newlibs) > 0 ) {
    message(
      "Attention! You have over-installed packages!\n",
      "Potentially dangerous installations:"
    )
    .f <- function(...) tryCatch(as.character(utils::packageVersion(...)),
                                 error = function(e) NA_character_)
    v1 <- vapply(newlibs, .f, lib.loc = lib1, "", USE.NAMES = FALSE)
    v2 <- vapply(newlibs, .f, lib.loc = lib2, "", USE.NAMES = FALSE)
    df <- data.frame(p = newlibs, l1 = package_version(v1),
                     x = "\u2026", l2 = package_version(v2, strict = FALSE))
    df$y <- ifelse(df$l1 > df$l2, "\u2713",  "\u2716")
    names(df) <- c("package", lib1, " ", lib2, "newer")
    str <- structure("<pkg_vrs>", class = c("package_version", "numeric_version"))
    r1  <- data.frame("<chr>", str, "", str, "<chr>", check.names = FALSE)
    names(r1) <- names(df)
    print(rbind(r1, df), row.names = FALSE, right = FALSE)
  }
  invisible()
}
check_libmasks()
