######################################
### dAdparam 3-dim array/matrix
### for nonlinear analysis
### but only the linear parameters
######################################
rm(list=ls())
################################################################
# Setting up the dAdparam matrix using derivatives with respect
# to the 22 various LINEAR parameters. Each plane of the 3-dim matrix
# dAdparam equals dA/ with respect to the k^th parameter
# Theta is the parameter of interest. The k^th plane will be dAdparam
# corresponding to that parameter.
##################################################################
# load parameters into memory
source("/Users/sfield/Documents/CSU/R-scripts/5NP R Code/Animal/SetParameters.R")

# Define dimensions of dAdparam matrix using 'stages' from SetParameters.R
# stages = length(I.pop) from above; assume square matrix dimenstions
# Set dimensions used to set dAdparam

# Set value for Beta separately because usually is an argument in pine6
b = c(0,rep(0.04,5))

# The theta vector with linear parameters contained
theta = c(S.par[2], S.par[3], S.par[4], S.par[5], S.par[6],
        T.par[2], T.par[3], T.par[4], T.par[5],
        b[2], b[3], b[4], b[5], b[6],
        C.par[2], C.par[3], C.par[4], C.par[5], C.par[6],
        dbh.v[4], dbh.v[5], dbh.v[6])

kdim = length(theta); # number of linear parameters to take derivatives w.r.t

# Create dAdparam storage matrix full of zeros
dAdparam = array(0,dim=c(stages,stages,kdim)) # planes = theta parameter

#######################################################
### Populate the dAdparam matrix with the derivatives
#######################################################
# for s2; theta_1
dAdparam[2,2,1] = 1 - b[2]
dAdparam[8,2,1] = b[2]
dAdparam[8,8,1] = C.par[2]

# for s3; theta_2
dAdparam[3,3,2] = 1 - b[3]
dAdparam[9,3,2] = b[3]
dAdparam[9,9,2] = C.par[3]

# for s4; theta_3
dAdparam[4,4,3] = 1 - b[4]
dAdparam[10,4,3] = b[4]
dAdparam[10,10,3] = C.par[4]

# for s5; theta_4
dAdparam[5,5,4] = 1 - b[5]
dAdparam[11,5,4] = b[5]
dAdparam[11,11,4] = C.par[5]

# for s6; theta_5
dAdparam[6,6,5] = 1 - b[6]
dAdparam[12,6,5] = b[6]
dAdparam[12,12,5] = C.par[6]

# for t2; theta_6
dAdparam[3,2,6] = 1 - b[2]
dAdparam[9,2,6] = b[2]
dAdparam[9,8,6] = C.par[3]

# for t3; theta_7
dAdparam[4,3,7] = 1 - b[3]
dAdparam[10,3,7] = b[3]
dAdparam[10,9,7] = C.par[4]

# for t4; theta_8
dAdparam[5,4,8] = 1 - b[4]
dAdparam[11,4,8] = b[4]
dAdparam[11,10,8] = C.par[5]

# for t5; theta_9
dAdparam[6,5,9] = 1 - b[5]
dAdparam[12,5,9] = b[5]
dAdparam[12,11,9] = C.par[6]

# for b2; theta_10
dAdparam[2,2,10] = -S.par[2]
dAdparam[3,2,10] = -T.par[2]
dAdparam[8,2,10] = S.par[2]
dAdparam[9,2,10] = T.par[2]

# for b3; theta_11
dAdparam[3,3,11] = -S.par[3]
dAdparam[4,3,11] = -T.par[3]
dAdparam[9,3,11] = S.par[3]
dAdparam[10,3,11] = T.par[3]

# for b4; theta_12
dAdparam[4,4,12] = -S.par[4]
dAdparam[5,4,12] = -T.par[4]
dAdparam[10,4,12] = S.par[4]
dAdparam[11,4,12] = T.par[4]

# for b5; theta_13
dAdparam[5,5,13] = -S.par[5]
dAdparam[6,5,13] = -T.par[5]
dAdparam[11,5,13] = S.par[5]
dAdparam[12,5,13] = T.par[5]

# for b6; theta_14
dAdparam[6,6,14] = -S.par[6]
dAdparam[12,6,14] = T.par[6]

# for C.par2; theta_15
dAdparam[8,7,15] = T.par[1] # T.par[1] is actually zero because in nonlinear piece
dAdparam[8,8,15] = S.par[2]

# for C.par3; theta_16
dAdparam[9,8,16] = T.par[2]
dAdparam[9,9,16] = S.par[3]

# for C.par4; theta_17
dAdparam[10,9,17] = T.par[3]
dAdparam[10,10,17] = S.par[4]

# for C.par5; theta_18
dAdparam[11,10,18] = T.par[4]
dAdparam[11,11,18] = S.par[5]

# for C.par6; theta_19
dAdparam[12,11,19] = T.par[5]
dAdparam[12,12,19] = S.par[6]

# for d4; theta_20
dAdparam[10,9,20] = T.par[3]*0.15*exp(-0.15*dbh.v[4])
dAdparam[10,10,20] = S.par[4]*0.15*exp(-0.15*dbh.v[4])

# for d5; theta_21
dAdparam[11,10,21] = T.par[4]*0.15*exp(-0.15*dbh.v[5])
dAdparam[11,11,21] = S.par[5]*0.15*exp(-0.15*dbh.v[5])

# for d6; theta_22
dAdparam[12,11,22] = T.par[5]*0.15*exp(-0.15*dbh.v[6])
dAdparam[12,12,22] = S.par[6]*0.15*exp(-0.15*dbh.v[6])

dAdparam









