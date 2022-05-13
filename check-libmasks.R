check_libmasks <- function() {
  libs <- .libPaths()
  stopifnot(length(libs) >= 2L)
  l1 <- list.dirs(libs[1L], full.names = FALSE, recursive = FALSE)
  l2 <- list.dirs(libs[2L], full.names = FALSE, recursive = FALSE)
  masks <- intersect(l1, l2)
  if ( length(masks) > 0 ) {
    message(
      "You have over-installed packages!\n",
      "Potentially dangerous installations:"
    )
    v1 <- lapply(masks, packageVersion, lib.loc = libs[1L]) |> sapply(as.character)
    v2 <- lapply(masks, packageVersion, lib.loc = libs[2L]) |> sapply(as.character)
    df <- data.frame(p = masks, l1 = package_version(v1),
                         x = "\u2026", l2 = package_version(v2, strict = FALSE))
    df$y <- ifelse(df$l1 > df$l2, "\u2713",  "\u2716")
    names(df) <- c("package", libs[1L], " ", libs[2L], "newer")
    str <- structure("<pkg_vrs>", class = c("package_version", "numeric_version"))
    r1  <- data.frame("<chr>", str, "", str, "<chr>", check.names = FALSE)
    names(r1) <- names(df)
    print(rbind(r1, df), row.names = FALSE, right = FALSE)
  }
  invisible()
}
check_libmasks()
