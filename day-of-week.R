

dow <- function(yr1, yr2, day.yr1) {

	yr.diff <- yr2-yr1
	wvec <- c("Sun","Mon","Tue","Wed","Thr","Fri","Sat")

	if ( !day.yr1 %in% wvec )
		stop("Invalid 'day' value")
	
	which <- which(wvec==day.yr1)
	yr.vec <- seq(yr1+1,yr2)

	if ( which!=7 )
		wvec <- c(wvec[(which+1):7],wvec[1:which])

	d <- 0
	for (i in 1:yr.diff) d <- if (yr.vec[i]%%4==0) d=d+2 else d=d+1

	if ( d > 7 )
		final.vec <- rep(wvec, yr.diff%/%7*2)
	else
		final.vec <- wvec

	return(final.vec[d])

}
