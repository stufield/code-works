# ESL figures

#library(ElemStatLearn)
#library(class)

fig2.1 <- function() {

  x <- mixture.example$x
  g <- mixture.example$y
  x.mod <- lm(g ~ x)
  plot(x, col = ifelse(g == 1, "coral", "cornflowerblue"), xlab = "", ylab = "", 
      main = "Linear Regression of 0/1 Response")
  print(coef(x.mod))
  abline( (0.5-coef(x.mod)[1])/coef(x.mod)[3], -coef(x.mod)[2]/coef(x.mod)[3])

  #prob <- predict(x.mod, mixture.example$xnew)

  ghat <- ifelse(fitted(x.mod)>0.5, 1, 0)
  if ( interactive() ) {
    print(length(ghat))
    print(sum(ghat == g))
    print(1 - sum(ghat==g)/length(g))
    xnew <- mixture.example$xnew # training misclassification rate
    print(dim(xnew))
    print(colnames(xnew))
  }
}



fig2.2 <- function(k = 15) {
  # doubles for fig2.3 with k=1
  x <- mixture.example$x
  g <- mixture.example$y
  xnew <- mixture.example$xnew
  mod15 <- knn(x, xnew, g, k=k, prob=TRUE)
  prob <- attr(mod15, "prob")
  prob <- ifelse(mod15=="1", prob, 1-prob)
  px1 <- mixture.example$px1
  px2 <- mixture.example$px2
  prob15 <- matrix(prob, length(px1), length(px2))
  par(par.def)
  contour(px1, px2, prob15, levels=0.5, xlab="", ylab="", lwd=2, lty=1, col=1,
        vfont=c("sans serif","bold"), axes=FALSE, labcex=1,
        main=sprintf("%i-Nearest Neighbour Classifier",k))
  points(x, col=map.binary.vector(g, 1, c("coral","cornflowerblue")))
  gd <- expand.grid(x=px1, y=px2)
  points(gd, pch=".", cex=1.2, col=ifelse(prob15>0.5, "coral", "cornflowerblue"))
  box()
}

fig2.4 <- function() {

  # Untested!!!!
  # Reproducing figure 2.4, page 17 of the book:
  # The data do not contain a test sample, so we make one,
  # using the description of the oracle page 17 of the book: The centers 
  # is in the means component of mixture.example, with green(0) first, 
  # so red(1).  For a test sample of size 10000 we simulate
  # 5000 observations of each class.
       
  set.seed(123)
  centers <- c(sample(1:10, 5000, replace=TRUE), 
           sample(11:20, 5000, replace=TRUE))
  means <- mixture.example$means
  means <- means[centers, ]
  mix.test <- mvrnorm(10000, c(0,0), 0.2*diag(2))
  mix.test <- mix.test + means
  cltest <- c(rep(0, 5000), rep(1, 5000))
  ks <- c(1,3,5,7,9,11,15,17,23,25,35,45,55,83,101,151 )
  # nearest neighbours to try
  # nearest neighbours to try
  nks <- length(ks)
  misclass.train <- numeric(length=nks)
  misclass.test  <- numeric(length=nks)
  names(misclass.train) <- names(misclass.test) <- ks
  for (i in seq(along=ks)) {
    mod.train <- knn(x,x,k=ks[i],cl=g)
    mod.test  <- knn(x, mix.test,k= ks[i],cl= g)
    misclass.train[i] <- 1 - sum(mod.train==factor(g))/200
    misclass.test[i] <- 1 - sum(mod.test==factor(cltest))/10000
  }
  print(cbind(misclass.train, misclass.test))

  plot(misclass.train, xlab="Number of NN", ylab="Test error", type="n", xaxt="n")
  axis(1, 1:length(ks), as.character(ks))
  lines(misclass.test, type="b", col="blue", pch=20)
  lines(misclass.train, type="b", col="red", pch=20)
  legend("bottomright", lty=1, col=c("red","blue"), legend=c("train ", "test "))

}


fig2.5 <- function() {
  x <- mixture.example$x
  g <- mixture.example$y
  px1 <- mixture.example$px1
  px2 <- mixture.example$px2
  prob <- mixture.example$prob
  prob.bayes <- matrix(prob, length(px1), length(px2))
  par(par.def)
  contour(px1, px2, prob.bayes, levels=0.5, xlab="", ylab="", lwd=2, lty=1, col=1,
        labcex=1, vfont=c("sans serif","bold"), axes=FALSE, 
        main="Bayes Optimal Classifier")
  points(x, col=ifelse(g==1,"coral","cornflowerblue"))
  gd <- expand.grid(x=px1, y=px2)
  points(gd, pch=".", cex=1.2, col=ifelse(prob.bayes>0.5,"coral","cornflowerblue"))
  box()
}

