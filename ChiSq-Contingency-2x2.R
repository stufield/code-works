#################
# http://math.hws.edu/javamath/ryan/ChiSquare.html
# ?chisq.test
#################
data <- matrix(c(36, 30, 14, 25), ncol=2,
               dimnames=list(c("Treated", "Not Treated"),
                             c("Dead", "Alive")))
data

chisq.test(data, correct=FALSE)
chisq.test(data, correct=TRUE)   # if df=1 must correct with Yates continuity


# Dairy 1
chisq.test(matrix(c(12, 19, 22, 88), ncol=2), correct=FALSE)
chisq.test(matrix(c(12, 19, 22, 88), ncol=2), correct=TRUE)

# Dairy 2
chisq.test(matrix(c(3, 21, 20, 35), ncol=2), correct=FALSE)
chisq.test(matrix(c(3, 21, 20, 35), ncol=2), correct=TRUE)


# Jen Example
embryo.data <- matrix(c(203, 131, 202, 206), ncol=2, 
               dimnames=list(c("w FCS", "w/o FCS"),
                             c("Uncleaved", "Cleaved")))
embryo.data
chisq.test(embryo.data, correct=FALSE)
chisq.test(embryo.data, correct=TRUE)  # if df=1 must correct with Yates continuity


embryo.data2 <- matrix(c(35, 59, 10, 13, 164, 133, 76, 86), 
                ncol=2, 
                dimnames=list(c("C-C", "C-B", "B-C", "B-B"),
                              c("Uncleaved", "Cleaved")))

embryo.data2
chisq.test(embryo.data2, correct=FALSE)
#chisq.test(embryo.data2, correct=TRUE)  # if df=1 must correct with Yates continuity

