############################################################
# Formula if Beta jumps by 0.2 units per generation
############################################################
x   <- seq(1, 21, by=0.2)
y   <- Mod(0.64 * asin(sin((0.32 * x) - 0.32)))
int <- 0 * x
plot(x, y, "p")
lines(x, int)
range(y)

x[which(y == min(y))]
y[1]

############################################################
# Formula if Beta jumps by 0.1 units per generation
############################################################
x   <- seq(1,35, by=0.1)
y   <- Mod(0.635 * asin(sin((0.16 * x) - 0.16)))
int <- 0 * x
plot(x, y, "p")
lines(x, int)
range(y)

x[which(y == min(y))]
y[1]

#############################################
# Curve function of the tent function
#############################################
curve(Mod(0.635 * asin(sin((0.16 * x) - 0.16))), from=1, to=300)

#########################################
# Values for the actual runs within loop
#########################################
Gen  <- 30
Beta <- sapply(1:Gen, function(r) Mod(0.635 * asin(sin((0.16 * r) - 0.16))))
Beta
range(Beta)
plot(Beta, type="b")
