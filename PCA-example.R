###############
# PCA Example
###############
x <- c(2.5,.5,2.2,1.9,3.1,2.3,2,1,1.5,1.1)
y <- c(2.4,.7,2.9,2.2,3,2.7,1.6,1.1,1.6,.9)
data <- data.frame(x = x, y = y)
rownames(data) <- head(LETTERS, 10L)

means <- apply(data, 2, mean)

data2 <- sapply(1:2, function(x) data[, x] - means[x])
colnames(data2) <- c("x", "y")

plot(y ~ x, data = data, xlim = c(-1, 4), ylim = c(-1, 4))
abline(h = 0)
abline(v = 0)

# calc covariance matrix
cov_mat <- var(data)
with(data, cov(x, y))

var(x) + var(y)
var(x + y)

vecs <- eigen(cov_mat, FALSE)$vectors

plot(y ~ x, data = data2)
abline(0, vecs[1, 1] / vecs[2, 1], lty = 2)
abline(0, vecs[1, 2] / vecs[2, 2], lty = 3)
abline(h = 0); abline(v = 0)

Final_data <- t(vecs) %*% t(data2)
Final_data
plot(t(Final_data), xlim = c(-2, 2), ylim=c(-1, 1), col = 1:ncol(data))
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)

###############
# require(HSAUR)
###############
data_pca <- prcomp(data)
data_pca
summary(data_pca)
x11()
plot(data_pca)
x11()
biplot(data_pca, col = c("black", "red"))
predict(data_pca, data)   # same as Final_data above

