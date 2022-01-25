
set_Names <- function(x, nms = x, ...) {
  if ( is.function(nms) ) {
    nms <- nms(names(x) %||% x, ...)
  } else if ( !is.null(nms) ) {
    nms <- as.character(nms)
  }
  structure(x, names = nms)
}
set_Names(head(mtcars), toupper)   # apply fn
set_Names(head(letters))           # names self
set_Names(head(letters), toupper)  # apply fn non-existing
set_Names(set_Names(head(letters)), NULL) # NULL removes names
