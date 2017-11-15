##Sexual Conflict Kokko Chapter 1
x=seq(0,1,length=101)
m1=1.5
m2=5
m3=10
y1=(m1*x)/((m1*x)+(1-x))
y2=(m2*x)/((m2*x)+(1-x))
y3=(m3*x)/((m3*x)+(1-x))
plot(x, y1, 'l', xlab="Proportion of A males in the Population", ylab="Probability that a male has allele A", col='dark blue', lwd=2)
lines(x, y2, col='dark green', lwd=2)
lines(x, y3, col='dark red', lwd=2)
text(0.5, 0.45, "m = 1.5", cex=1.3)
text(0.35, 0.6, "m = 5", cex=1.3)
text(0.1, 0.8, "m = 10", cex=1.3)

##par(mfrow=c(2,2))## Makes output window have graphs in rows & columns
##ylim = range(y1,y2,y3)## Sets the max/min values of y-axis according to all y-values, not just y1
