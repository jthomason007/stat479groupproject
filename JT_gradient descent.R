rm(list=ls())

#importing data
data <- read.csv('Data - Sheet1.csv')

#partitioning data
set.seed(117)
sample <- sample.split(data, SplitRatio = .75)
train <- subset(data, sample == T)
test <-  subset(data, sample == F)

#X_test format
drop <- c('League','Total.Wins')
X_train <- train[,!(names(train) %in% drop)]
X_train <- X_train[-11,]
X_train <- as.matrix(X_train)
drop <- c('Total.Wins')
y_train <- train[,(names(train) %in% drop)]
y_train <- y_train[-11]
y_train <- as.matrix(y_train)

#y_test format
drop <- c('League','Total.Wins')
X_test <- test[,!(names(test) %in% drop)]
X_test <- X_test[-11,]
X_test <- as.matrix(X_test)
drop <- c('Total.Wins')
y_test <- test[,(names(test) %in% drop)]
y_test <- y_test[-11]
y_test <- as.matrix(y_test)

#gradient descent
beta <- rep(1,18)
mu <- X_train %*% beta
D <- diag(1/seq(116))

GD_LS <- function(X,y,b0) {
  iter_max <- 1000
  alpha <- 1/norm(t(X)%*%X, '2')
  b <- b0
  for (i in 1:iter_max) {
    grad <- t(X) %*% (X %*% b-y)
    b <- b - alpha * grad
    if (sum(grad**2)<10**-8) {
      break
    }
  }
  return(b)
}

b0 <- rep(0,18)
b_LS={}

for (i in 116) {
  b_LS <- cbind(b_LS, GD_LS(X_train,y_train[i,],b0))
}

mean((X_test%*%b_LS[,1] - y_test)**2)

