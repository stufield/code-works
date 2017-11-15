##############################
# Optim() given a function
# that takes a vector of
# parameters and turns them
# into a number, then finds
# the parameter set that
# maximizes that value
##############################
require(emdbook)
######################
a=0.5; b=12; t=0.1
curve(-t*(x-a)^2+b, from=-10, to=10)
a=5; b=3; t=0.1; curve(-t*(x-b)^2-a, from=-10, to=10)
#####################################
myfun <- function(x,a,b,t){
	y = -t * (x-a)^2 + b
	y}

myfun2 <- function(x,a,b,t){
	y = 2*x^3 + a*x^2 - b*x + 2
	y}

myfun3 <- function(x,a,b,t){
	y = -t * (x-a)^3 + b
	y}



##########################
### optimizer function ###
##########################
optimizer(fn=myfun, I=c(-10,10), max=T, a=2, b=12, t=0.1)
optimizer(fn=myfun, I=c(-10,10), max=F, a=2, b=12, t=0.1)
optimizer(fn=myfun2, I=c(-10,10), max=T, a=2, b=12, t=0.1)
optimizer(fn=myfun2, I=c(-10,10), max=F, a=2, b=12, t=0.1)
optimizer(fn=myfun3, I=c(-10,10), max=T, a=2, b=12, t=0.1)
optimizer(fn=myfun3, I=c(-10,10), max=F, a=2, b=12, t=0.1)

# For one parameter (x)
optimize(f=myfun, interval=c(-10,10), maximum=T, a=a, b=b, t=t)
optimize(f=myfun2, interval=c(-10,10), maximum=T, a=a, b=b, t=t)

#######################
### Using optim()
### 2 par example
#######################
?optim
fun1 <- function(x){
 	y <- (x[1]-3)^2 + (x[2]-1)^2 + 5.5
 	y}

optim(par=c(0,0), fn=fun1, method = c("BFGS"), # or "Nelder-Mead" or "SANN"
	lower = -Inf, upper = Inf, control = list(), hessian = TRUE)

curve((x-3)^2 + (x-1)^2 + 5.5, from=-10, to=10)

######################
### with mle2
######################
require(bbmle)
parnames(myfun) = c("x","y") # must tell it vector name if pars are not separate arguments
fit2 = mle2(myfun, start= list(x=-20,y=3), vecpar=TRUE, method='Nelder-Mead')



