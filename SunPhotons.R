###########################
### Photons leaving Star ##
### The "random walk" to the corona
##############################
### Stu Field
### April 12th 2011
#############################################
# Function for drawing a circle of radius r
circle <- function(rad, add=TRUE, ...) {
   circ <- seq(0, 2*pi, l=100)
   if ( add ) {
      lines(rad*cos(circ), rad*sin(circ), ...)
   } else {
      plot(rad*cos(circ), rad*sin(circ), type="l", ylab="", xlab="", ...)
   }
}

###########################################
x <- 0; vecx <- 0
y <- 0; vecy <- 0
z <- 0; vecz <- 0
D <- 1391000       # diameter of sun (km)
r <- D/2           # radius of sun (km)
f <- 5000          # scale factor
r <- r/f           # scaled radius of sun
dist <- 0          # initial distance from core (0,0,0)
count <- 0         # counter for steps

### Run simulation ###
set.seed(101)
while (dist <= r) {
  x <- x + runif(1, -1, 1)
  y <- y + runif(1, -1, 1)
  z <- z + runif(1, -1, 1)
  dist  <- sqrt((x-0)^2 + (y-0)^2 + (z-0)^2)
  vecx  <- c(vecx, x)
  vecy  <- c(vecy, y)
  vecz  <- c(vecy, z)
  count <- count + 1
  if ( count%%1000==0 )
     cat("steps =", count, "\n")
}

### Plot simulation ###
plot(vecx, vecy, type="l", ylim=c(r*-1.2, r*1.2), 
     xlim=c(r*-1.2, r*1.2), xlab="", ylab="")
circle(rad=r, lwd=4, col="darkred")
abline(h=0, lty=3)
abline(v=0, lty=3)
count
dist
r

