###########################################
# PCA analysis                            #
# Handbook of Statistical Analyses in R   #
# pgs 218 - 223                           #
# Stu Field                               #
###########################################
pca_example <- function() {
  #require(HSAUR)
  #?prcomp
  data("heptathlon", package = "HSAUR")
  heptathlon$hurdles <- max(heptathlon$hurdles) - heptathlon$hurdles
  heptathlon$run200m <- max(heptathlon$run200m) - heptathlon$run200m
  heptathlon$run800m <- max(heptathlon$run800m) - heptathlon$run800m

  score <- which(colnames(heptathlon)=='score')

  plot(heptathlon[,-score])

  print(round(cor(heptathlon[,-score]), 3L))

  # PCA
  heptathlon_pca <- prcomp(heptathlon[, -score], scale = TRUE)

  ############################
  # linear rotation of PC1   #
  ############################
  a1 <- heptathlon_pca$rotation[, 1L]
  print(a1)

  ##############################
  # Center & scaling of PC1    #
  ##############################
  center <- heptathlon_pca$center
  scale <- heptathlon_pca$scale
  hm <- as.matrix(heptathlon[,-score])
  drop(scale(hm, center = center, scale = scale) %*% heptathlon_pca$rotation[, 1L])
  print(predict(heptathlon_pca)[, 1L])

  ################
  #    plots     #
  ################
  print(range(heptathlon_pca$x[,"PC1"])) 
  x11()
  plot(heptathlon_pca)
  x11()
  biplot(heptathlon_pca, col = c("black", "red"), 
         ylim= c(-0.6, 0.6), xlim= c(-0.7, 0.6), cex = 0.66) 
  abline(h=0, lty=3, col= 8)
  abline(v=0, lty=3, col= 8)
  df <- as.data.frame(heptathlon_pca$rotation[,1:2])
  colnames(df) <- c("PC1", "PC2")
  x11()
  plot(df, pch=19, main = "Rotation", xlim=c(-0.15,0.55))
  abline(h=0, lty=3, col=8)
  abline(v=0, lty=3, col=8)
  text(df$PC1, df$PC2, pos=2, labels=rownames(df))
  x11()
  plot(heptathlon$score, heptathlon_pca$x[,1], pch=19)
  print(cor(heptathlon$score, heptathlon_pca$x[,1]))

  #########################
  #     post-analysis     #
  #########################
  list(data       = heptathlon,
       Components = heptathlon_pca,
       summary    = summary(heptathlon_pca),
       names      = attributes(heptathlon_pca)$names,
       rotation   = heptathlon_pca$rotation,
       center     = heptathlon_pca$center,
       scale      = heptathlon_pca$scale,
       x          = heptathlon_pca$x)
}
