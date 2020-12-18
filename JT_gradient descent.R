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

#using gradDescent package in R
library("gradDescent")
gd.data =cbind(X_train, y_train)
gd.model.delta = ADADELTA(gd.data, seed=117)
gd.model.delta

i=0
results=matrix(nrow=35, ncol=1)
for (i in 1:35){
  tempval = as.numeric(gd.model.delta[1]) + as.numeric(gd.model.delta[2]*X_test[i,1]) + 
  as.numeric(gd.model.delta[3]*X_test[i,2]) + as.numeric(gd.model.delta[4]*X_test[i,3]) + 
  as.numeric(gd.model.delta[5]*X_test[i,4]) + as.numeric(gd.model.delta[6]*X_test[i,5]) + 
  as.numeric(gd.model.delta[7]*X_test[i,6]) + as.numeric(gd.model.delta[8]*X_test[i,7]) + 
  as.numeric(gd.model.delta[9]*X_test[i,8]) + as.numeric(gd.model.delta[10]*X_test[i,9]) + 
  as.numeric(gd.model.delta[11]*X_test[i,10]) + as.numeric(gd.model.delta[12]*X_test[i,11]) + 
  as.numeric(gd.model.delta[13]*X_test[i,12]) + as.numeric(gd.model.delta[14]*X_test[i,13]) + 
  as.numeric(gd.model.delta[15]*X_test[i,14]) + as.numeric(gd.model.delta[16]*X_test[i,15]) + 
  as.numeric(gd.model.delta[17]*X_test[i,16]) + as.numeric(gd.model.delta[18]*X_test[i,17]) + 
  as.numeric(gd.model.delta[19]*X_test[i,18]) 
  results[i,1] = tempval
  }

mean((results[,1] - y_test)**2)


