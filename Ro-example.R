# ----------------------------------
# Calculation of Ro 
# The Basic Reproductive Rate
# from de-Camino-Beck & Lewis 2007
# ----------------------------------
# See Fig. 3 in above paper for matrices
# a31=0.33; a21=0.2; a11=0.75; a32=0.4
# a13=3; a23=2; a33=1
# ----------------------------------
parS <- c(0.75, 0,   0,
          0.2,  0,   0,
          0.33, 0.4, 0)
parF <- c(0, 0, 3,
          0, 0, 2,
          0, 0, 1)

TM <- matrix(parS, 3, 3, byrow = TRUE)
TM
FM <- matrix(parF, 3, 3, byrow = TRUE)
FM
A <- TM + FM
A
eigen(A)$values[1L]   # Dom. eigenvalue of A


# Using the packaged function popmat
calcRo(TM, FM)


# -----------------------------
# Step by Step outside function
# Create Next Generation Matrix
# -----------------------------
I <- diag(3)     # Identity matrix
I         
N  <- solve((I - TM), I)   # Returns X where a %*% X = b; solve(a,b); N = The Fundamental matrix
R  <- FM %*% N           # Create Next Generation Matrix
Ro <- eigen(R)$values[1L]
Ro  # Get dominant eigenvalue of Next-Gen matrix which is Ro


# -----------------------------
# Double Check 
# Formula from paper Fig. 3.
# Life cycle Graph reduction method
# -----------------------------
Ro1 <- (((0.33 * 3) + (3 * 0.2 * 0.4)) / (1 - 0.75)) + (0.4 * 2) + 1
Ro1   # make sure Ro & Ro1 match
all.equal(Ro, Ro1)

# ---------------------
# Part II
# SIR disease model
# from Oli et al. 2006
# ---------------------
# Pick some parameter values
Beta  <- 0.2
Gamma <- 0.1
Ps <- 0.9
Pi <- 0.5
Pr <- 0.7

# T & F for disease (dis)
T.dis <- matrix(c(Ps * (1 - Beta), 0, 0,
                  Ps * Beta, Pi * (1 - Gamma), 0,
                  0, Pi * Gamma, Pr),
                nrow = 3, ncol = 3, 
                byrow = TRUE)

# use diag function to set up F for disease
F.dis <- diag(c(0, Ps * Beta, 0)) 

# Use above function to calculate Ro for the disease (Infection (I))
RoI.a <- calcRo(T.dis, F.dis)
RoI.a

# Double check from equation in Appendix I of Oli et al. 2006
RoI.b <- Ps * Beta / (1 - Pi + Pi * Gamma)
RoI.b

