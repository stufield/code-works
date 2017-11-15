########################################
###	Stu Field ##########################
###	Department of Biology ##############
###	Colorado State University ##########
###	Fort Collins, CO 80523-1878 ########
###	sgf@colostate.edu ##################
########################################
##############################
### optimizer() Function
### Finds Max or Min to a
### specified function over
### a given range of values
### then graphs it
##############################
myfun <- function(x,a,b,t){
	y = -t * (x-a)^2 + b
	y}

myfun2 <- function(x,a,b,t){
	y = 2*x^3 + a*x^2 - b*x + 2
	y}

myfun3 <- function(x,a,b,t){
	y = -t * (x-a)^3 + b
	y}


####################
### optimizer call
####################
optimizer(fn=myfun, I=c(-10,10), max=TRUE, a=2, b=12, t=0.1)
optimizer(fn=myfun, I=c(-10,10), max=FALSE, a=2, b=12, t=0.1)
optimizer(fn=myfun2, I=c(-10,10), max=TRUE, a=2, b=12, t=0.1)
optimizer(fn=myfun2, I=c(-10,10), max=FALSE, a=2, b=12, t=0.1)
optimizer(fn=myfun3, I=c(-10,10), max=TRUE, a=2, b=12, t=0.1)
optimizer(fn=myfun3, I=c(-10,10), max=FALSE, a=2, b=12, t=0.1)


########################
### Lease Squares fit
########################
x = seq(1,10,by=0.1)
a = 1; b = 2
y.det <- (a-b/x)
#y <- jitter((a-b/x) + c, amount=0.1)
y <- rnorm(length(x), mean=y.det, sd=0.05)
mydata <- cbind(x,y)
plot(mydata)

myfun <- function(pars,data){
	x <- data[,1]
	y <- data[,2]
	a <- pars[1]
	b <- pars[2]
	y.det = (a-b/x)
	sse <- sum((y-y.det)^2)
	sse
}

pars = c(a=0.5, b=2)
fit <- optim(par=pars, fn=myfun, data=mydata)
fit
CI95 <- confint(fit)

plot(mydata)
lines(x, (fit$par[1] - fit$par[2]/x), col=2)
lines(x, y.det, col=4)

##########################
# fit using nls()
##########################
fit2 <- nls(y ~ a-b/x, start= list(a=1, b=2))
summary(fit2)
est.a <- coef(fit2)["a"]
est.b <- coef(fit2)["b"]
CI95.2 <- confint(fit2)
CI95.2

plot(mydata)
lines(x, (est.a - est.b/x), col=2)
lines(x, (fit$par[1] - fit$par[2]/x), col=3)
lines(x, y.det, col=4)

