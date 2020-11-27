

# partial matching of arguments can be problematic when
# wrapping functions
wrapper <- function(x, y, main.cex = 2, ...) {
	plot(x, y)
	print(match.call(expand.dots = TRUE))
	title(..., cex.main = main.cex)
}

wrapper(1, 2, main = "hello")
