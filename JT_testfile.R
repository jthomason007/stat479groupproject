#James Thomason
rm(list = ls())
library(glmnet)
library(caTools)
library(RColorBrewer)

data <- read.csv('Data - Sheet1.csv')

#setting colors for graph
mypalette <- brewer.pal(6,"Set1")
mypalette


model <- lm(Total.Wins ~ ., data = data)
summary(model)

model.glm <- glm(Total.Wins ~., data = data)
summary(model.glm)

drop <- c('League','Total.Wins')
X <- data[,!(names(data) %in% drop)]
X <- X[-12,]
X <- as.matrix(X)
y <- data[,(names(data) %in% drop)]
dropper <- c('League')
y <- y[,!(names(y) %in% dropper)]
y <- y[-12]
y <- as.matrix(y)


#testing making  of model
XtX <- t(X)%*%X
lambda <- 10

mse_R <- {}

beta <- rep(0,18)
mu <-  X%*% beta
b_R <- {}

for (i in 1000) {
  b_R <- cbind(b_R, solve(XtX + lambda*diag(18), t(X)%*%y))
  
}
mse_R=c(mse_R, mean(colSums((b_R-beta)**2)))

#Ridge plot
model.net <- glmnet(X,y, alpha = 0)
plot(model.net, xvar = "lambda", label=TRUE, lwd=3, col=mypalette, main="Ridge Penalty Regression"); abline(h=0, lwd=1, lty=2, col="grey")
legend("topright", lwd=3, lty=1, legend=colnames(Xmat), col=mypalette)


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

#training
XtX <- t(X_train)%*%X_train
lambda <- 100

mse_R <- {}

beta <- rep(0,18)
mu <-  X_train%*% beta
b_R <- {}

for (i in 1000) {
  b_R <- cbind(b_R, solve(XtX + lambda*diag(18), t(X_train)%*%y_train))
  
}
mse_R=c(mse_R, mean(colSums((b_R-beta)**2)))


#mse
mse_test <- sum((y_test-(X_test%*%b_R))**2)
mse_test


#finding lambda
magic.ridge.cv <- cv.glmnet(X, y, alpha=0, nfold=5)
plot(magic.ridge.cv)
election.ridge.cv$lambda.min
