#################
#	Funny Plot
########################
x <- c(You=1000, Me=10)
par(mar=c(3,3.5,5,1), mgp=c(2,.75,0))
barplot(x, ylim=c(0, 1100), col=c("darkred", "navy"), yaxt="n", cex.names=1.5, density=NA, angle=10, border=1)
box()
mtext("Interest level", side=2, line=2, font=2, cex=1.5)
mtext("Interest in Seeing Pics \n of Your Kids", side=3, line=2, font=4, cex=1.5)
