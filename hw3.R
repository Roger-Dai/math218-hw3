sum.x2 <- function(data) {
  # Returns the sum of squares of sample mean variance.
  sum <- 0
  bar <- mean(data)
  for (value in data) {
    x2 <- (value-bar)^2
    sum <- sum + x2
  }
  return(sum)
}

sum.xy <- function(data1, data2) {
  # Returns the sum of the product of two sample mean variances.
  bar1 <- mean(data1)
  bar2 <- mean(data2)
  sum <- 0
  for (i in 1:length(data1)) {
    xy = (data1[i]-bar1)*(data2[i]-bar2)
    sum <- sum + xy
  }
  return(sum)
}

rss <- function(data1, data2, b0, b1) {
  # Returns the residual sum of squares of a regression.
  sum <- 0
  for (i in 1:length(data1)) {
    yhat <- b0 + b1*data2[i]
    e2 <- (data1[i] - yhat)^2
    sum <- sum + e2
  }
  return(sum)
}

linear.model <- function(dependent, independent) {
  # Runs an overall regression of two data sets. This function prints out important
  # statistics of the regression.
  sumx2 <- sum.x2(independent)
  sumy2 <- sum.x2(dependent)
  sumxy <- sum.xy(dependent,independent)
  b1 <- sumxy/sumx2
  b0 <- mean(dependent) - b1*mean(independent)
  rss1 <- rss(dependent,independent,b0,b1)
  stderr <- sqrt((rss1/(length(dependent)-2))/sumx2)
  r2 <- 1-rss1/sumy2
  cat("R-squared:  " , r2, "\n")
  cat("Estimated coefficient: " , b1, "\n")
  cat("Standard Error for beta1: ", stderr, "\n")
  cat("Estimated constant: ", b0, "\n")
}

# Testing against lm()
library(MASS)
linear.model(Boston$medv, Boston$crim)
lm.model <- lm(medv ~ crim, data=Boston)
summary(lm.mod)





