####################
#### R Tutorial ####
##### Dataset ######
### "Tree Data" ####
####################
#############################################
#mydata <- read.csv("TreeData.csv")
################ OR #########################
wooddata <- data.frame(
	year = c(1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983),
	age = c(rep(c("Juveniles"),9), rep("Adults",9)),
	N = c(59, 22, 43, 42, 1, 48, 39, 8, 25, 46, 46, 40, 51, 52, 32, 46, 49, 35),
	s = c(33, 14, 13, 17, 0, 18, 7, 2, 11, 24, 31, 28, 19, 28, 22, 30, 24, 21),
	p = c(0.569, 0.636, 0.302, 0.405, 0.000, 0.375, 0.179, 0.250, 0.440, 0.522, 0.674, 0.700, 0.372, 0.538, 0.688, 0.652, 0.490, 0.600),
	p.1_p = c(0.2465, 0.2315, 0.2108, 0.2410, 0.0000, 0.2344, 0.1470, 0.1875, 0.2464, 0.2495, 0.2197, 0.2100, 0.2339, 0.2486, 0.2147, 0.2269, 0.2499, 0.2400)
)

wooddata; summary(wooddata)

wooddata <- wooddata[wooddata$age == "Juveniles",] # only work with Juveniles
wooddata <- wooddata[wooddata$age == "Adults",] # only work with Adults
wooddata <- wooddata[wooddata$s > 0 & wooddata$age=="Juveniles",] # remove year where only one individual followed

mean(wooddata$p)

################################
### Define Likelihood function
#################################
require(bbmle)
nLogLik <- function(Sprob, k, N){
	-sum(dbinom(k, prob=Sprob, size=N, log=TRUE))
}

SurvMLE <- mle2(minuslogl= nLogLik, start= list(Sprob= 0.5), data= list(N=wooddata$N, k=wooddata$s), method="L-BFGS-B", lower=0.001, upper=0.999)

CI <- confint(SurvMLE)
Surv <- c(CI[1], coef(SurvMLE), CI[2]); Surv
logLik(SurvMLE); exp(logLik(SurvMLE))
mean(wooddata$p)



