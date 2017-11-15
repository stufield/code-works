
options(error = function() {

	msg <- geterrmessage()
	missing <- grepl("could not find function", msg)   # if missing function error
   q <- gregexpr("\"", msg)[[1]]
	missing_fun <- substr(msg, start=q[1] + 1, stop=q[2] - 1)
	if ( missing && missing_fun%in%names(map) ) {
		cat("This function has changed names as of v2.0.0 ...\n")
		cat("Old name:", missing_fun, "\n")
		cat("New name:", map$hotdog, "\n")
		#v2functionMap(missing_fun)
	} else {
		NULL
	}
	
})


map <- list(hotdog="plotHotDog")

errorTest <- function() {
	
	m <- mean(1:10)
	print(m)
	hotdog(m)
	return("OK")

}


