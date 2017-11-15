##################################
# Calculating LAI from DBH
# Law et al.
####################################
data  <- read.table("LAI.csv", sep=",", header=TRUE)
a     <- data$a[2]
b     <- data$b[2]
ma    <- mean(data$a)
mb    <- mean(data$b)
dbh   <- 12.3
MSE   <- data$MSE[2]
L_Asw <- a + b*(log10(dbh))
Asw   <- 10^(L_Asw)  # Uncorrected Sapwood area
CF    <- exp(0.5*((2.303*(MSE^0.5))^2)) # Correction factor
Asw_c <- Asw * CF    # Asw corrected for logarithmic bias via CF correction factor
NT    <- 694 / 10000 # (No. trees per m^2)
LAI   <- NT * ((0.163 * Asw_c) - 2.594)
LAI
