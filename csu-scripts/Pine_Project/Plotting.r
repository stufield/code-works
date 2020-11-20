#####################
#### Plotting
##################
Output = pine36(ipop=I.pop, Gen=5, plot=F)$Popn
y <- colSums(Output[c(5,6,11,12),3,]) / colSums(Output[c(2,3,4,8,9,10),3,])
plot(1:length(y), y, type="b", main="MAIN TITLE HERE", xlab="xlabel", ylab="ylabel", pch=19, ylim=c(0,5), col="navy")

#################################
### Beta vs r.site
### vs. Final Infected Adults
#################################
b <- seq(0, 0.1, by=0.01/3)
Lb = length(b)
site <- seq(0, 1, by=0.05)
Ls = length(site)
out <- matrix(NA, nrow=Lb*Ls, ncol=4, dimnames=list(NULL, c("n","Beta","r.site","Final_Adults_i")))

for (i in 1:Lb){
  for (j in 1:Ls){
    out[(Ls*(i-1))+j,1] = Ls*(i-1)+j
    out[(Ls*(i-1))+j,2] = b[i]
    out[(Ls*(i-1))+j,3] = site[j]
    out[(Ls*(i-1))+j,4] = tail(pine12(ipop=I.pop, Beta=b[i], r.site=site[j], plot=F)$Pop_Totals,1)
  }
}
out
  
# Reorganize
require(akima)
plot3d <- interp(out[,"Beta"], out[,"r.site"], out[,"Final_Adults_i"])
# 3D plot
persp(plot3d, ylab="r.site", xlab="Beta", zlab="Final Adults", theta=40, shade=0.7, ltheta=0, phi=20, col="darkorchid4", ticktype="detailed", main="TITLE HERE", cex.lab=1.5, cex.axis=1.2, tcl=0.01)




####################
#### Beta vs Time
####################
source("/Users/sfield/Dropbox/CSU/R-scripts/5NP R Code/pine12/pine12.R")
b <- seq(0, 0.2, by=0.01/3)
Lb = length(b)
Gen=500
pop <- NA; beta <- NA; time=NA

for (i in 1:Lb){
	pop = c(pop, pine12(ipop=I.pop, Gen=Gen, Beta=b[i], plot=F)$Pop_Totals)
	beta = c(beta, rep(b[i],Gen))
	time = c(time, 1:Gen)
}

out <- na.omit(cbind(pop, beta, time)); colnames(out) <- c("Pop","Beta","time")

# Reorganize
require(akima)
plot3d <- interp(out[,"time"], out[,"Beta"], out[,"Pop"])

# 3D plot
persp(plot3d, ylab="Beta", xlab="time", zlab="Pop", theta=40, shade=0.7, ltheta=0, phi=20, col="grey55", ticktype="detailed", main="Population Projection vs. Beta", cex.lab=1.00, cex.axis=0.75, tcl=0.1)


#########
###
############
curve3d(pine12(a=x, b=y), from=c(4.4,0.18), to=c(6.5,0.22), xlab="a",ylab="b", main="2D Likelihood Surface", col="navy", zlab= "log-Likelihood")