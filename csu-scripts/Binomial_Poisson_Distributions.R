##################
# The Binomial
#################
vec <- 1:25
plot(vec, dbinom(vec, prob=0.2, size= length(vec)), xlab="k successes", ylab="Probability", main="Binomial Probability Distribution", sub="prob = 0.2, size = 50")
barplot(dbinom(vec, prob=0.1, size= length(vec)), vec, space=10, xlab="k successes", ylab="Probability", main="Binomial Probability Distribution", sub="prob = 0.2, size = 50")


### Example #2
k=0:10
b1 <- dbinom(k, prob=0.1, size=length(k))
b2 <- dbinom(k, prob=0.5, size=length(k))
b3 <- dbinom(k, prob=0.9, size=length(k))

Prob <- numeric(3*length(b1))
Prob[seq(1,31,3)]=b1; Prob[seq(2,32,3)]=b2; Prob[seq(3,33,3)]=b3 # reorganize values to every fourth entry

xscale <- as.vector(barplot(Prob)) # only used to determine notches on xaxis
gap <- xscale[2]-xscale[1]
barplot(Prob, col=rep(c(0,1,8),10), ylim=c(0,max(Prob)+0.1), xlab="k successes", ylab="Probability", main="Binomial Probability Distribution")
axis(1, as.character(0:10), at=seq(head(xscale,1)+gap,tail(xscale,1)-gap,3*gap), tick=T)
legend("top", c("p = 0.1", "p = 0.5", "p = 0.9"), fill=c(0,1,8))


#################
### Poisson
#################
# when N = large & p = small, the binomial approaches the poisson
# Example #1

counts <- 0:20
p1 <- dpois(counts, lambda=0.8)
p2 <- dpois(counts, lambda=3)
p3 <- dpois(counts, lambda=12)

Prob <- numeric(3*length(p1))
Prob[seq(1,61,3)]=p1; Prob[seq(2,62,3)]=p2; Prob[seq(3,63,3)]=p3

xscale <- as.vector(barplot(Prob)) # used to determine notches on xaxis
gap <- xscale[2]-xscale[1]
barplot(Prob, col=rep(c(0,1,8),20), ylim=c(0,max(Prob)+0.1), xlab="# of events", ylab="Probability", main="Poisson Probability Distribution")
axis(1, as.character(seq(0,20,5)), at=seq(head(xscale,1)+gap,tail(xscale,1)-gap, 15*gap), tick=TRUE)
legend("top", c("lambda = 0.8", "lambda = 3", "lambda = 12"), fill=c(0,1,8))


