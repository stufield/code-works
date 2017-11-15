######################################
### dxdparam 3-dim array/matrix
### for nonlinear analysis
### but only the linear parameters
######################################
rm(list=ls())
################################################################
# Setting up the dxdparam matrix using derivatives with respect
# to the 22 various LINEAR parameters. Each plane of the 3-dim matrix
# dxdparam equals dx/ with respect to the k^th parameter
# Theta is the parameter of interest. The k^th plane will be dxdparam
# corresponding to that parameter.
# The columns represent time: gen, gen+1, gen+2, etc.
##################################################################
# load parameters into memory; not necessary b/c called in dAdparam.R
# source("/Users/sfield/Documents/CSU/R-scripts/5NP R Code/Animal/SetParameters.R")

# load dAdparam
source("/Users/sfield/Documents/CSU/R-scripts/5NP R Code/Animal/dAdparam.R")

# load pine6 function to create population projection x
source("/Users/sfield/Documents/CSU/R-scripts/5NP R Code/Animal/5NP 6 Class (12x12) Animal 1.1.R")

# Set value for Beta separately because usually is an argument in pine6
b = c(0,rep(0.04,5))

# The theta vector with linear parameters contained
theta = c(S.par[2], S.par[3], S.par[4], S.par[5], S.par[6],
        T.par[2], T.par[3], T.par[4], T.par[5],
        b[2], b[3], b[4], b[5], b[6],
        C.par[2], C.par[3], C.par[4], C.par[5], C.par[6],
        dbh.v[4], dbh.v[5], dbh.v[6])

# Call the Population Projection used to calculate dxdparam
x <- pine6(Gen=100, Beta=0.04, plot=0)$Pop_Projection
A <- pine6(Gen=100, Beta=0.04, plot=0)$Projection_Matrix
A[1,]= 0; A[2,1]= 0 ### zero out the nonlinear parts of the projection matrix A

# Set dimensions used to set dxdparam
gen = nrow(x)
kdim = length(theta); # number of linear parameters to take derivatives w.r.t

# Set up the dxdparam matrix for storage (cols = time; plane v   s = params)
dxdparam = array(0, dim=c(stages, (gen+1), kdim)) # planes = theta parameter

for (loop1 in 1:gen){
    for (loop2 in 1:kdim){
        dxdparam[,loop1+1,loop2] = dAdparam[,,loop2] %*% x[loop1,] + A %*% dxdparam[,loop1,loop2]
    }
}

dxdparam
