library(moments)
library(glmnet)
library(caTools)
library(tidyverse)
library(broom)
library(MASS)
library(ridge)

data = read.csv("479-data.csv", header = T, sep=",")
data = data[-12,]


# EDA
par(mfrow=c(3,2))
hist(data$Total.Cost, col = "paleturquoise2", 
     main = "Histogram of Total Cost per League vs. Frequency", 
     xlab = "Total Cost per League")
hist(data$Avg.Cost, col = "paleturquoise2", 
     main = "Histogram of Avg. Cost per League vs. Frequency", 
     xlab = "Avg. Cost per League")
hist(data$Avg.CMC, col = "paleturquoise2", 
     main = "Histogram of Avg. CMC per League vs. Frequency", 
     xlab = "Avg. CMC per League")
hist(data$Avg.Power, col = "paleturquoise2", 
     main = "Histogram of Avg. Power per League vs. Frequency", 
     xlab = "Avg. Power per League")
hist(data$Avg.Toughness, col = "paleturquoise2", 
     main = "Histogram of Avg. Toughness per League vs. Frequency", 
     xlab = "Avg. Toughness per League")

# checking scatterplots for avg. cost & cmc
par(mfrow = c(2,1))
plot(x=data$Avg.Cost, y=data$Total.Wins, 
     main = "Avg. Cost per Card vs. Total Wins per League", 
     xlab = "Avg. Cost per Card", ylab = "Total Wins per League")
plot(x=data$Avg.CMC, y=data$Total.Wins, 
     main = "Avg. CMC per League vs. Total Wins per League", 
     xlab = "Avg. CMC per League", ylab = "Total Wins per League")


# checking skewness before transformation
skewness(data$Total.Cost)
skewness(data$Avg.Cost)
skewness(data$Avg.CMC)
skewness(data$Avg.Power)
skewness(data$Avg.Toughness)

# data transformation
data$Total.Cost = log(data$Total.Cost)
data$Avg.Cost = log(data$Avg.Cost)
data$Avg.CMC = log(data$Avg.CMC)
data$Avg.Power = log(data$Avg.Power)
data$Avg.Toughness= log(data$Avg.Toughness)


# checking skewness after transformation & new histograms
par(mfrow=c(3,2))
hist(data$Total.Cost, col = "palevioletred2", 
     main = "Histogram of Total Cost per League vs. Frequency", 
     xlab = "Total Cost per League")
hist(data$Avg.Cost, col = "palevioletred2", 
     main = "Histogram of Avg. Cost per League vs. Frequency", 
     xlab = "Avg. Cost per League")
hist(data$Avg.CMC, col = "palevioletred2", 
     main = "Histogram of Avg. CMC per League vs. Frequency", 
     xlab = "Avg. CMC per League")
hist(data$Avg.Power, col = "palevioletred2", 
     main = "Histogram of Avg. Power per League vs. Frequency", 
     xlab = "Avg. Power per League")
hist(data$Avg.Toughness, col = "palevioletred2", 
     main = "Histogram of Avg. Toughness per League vs. Frequency", 
     xlab = "Avg. Toughness per League")

skewness(data$Total.Cost)
skewness(data$Avg.Cost)
skewness(data$Avg.CMC)
skewness(data$Avg.Power)
skewness(data$Avg.Toughness)



# convert to factors
data$Number.Lands = factor(data[,8])
data$Number.Types.of.Basic.Lands = factor(data[,9])
data$Artifact = factor(data[,10])
data$Creature = factor(data[,11])
data$Sorcery = factor(data[,12])
data$Planeswalker = factor(data[,13])
data$Enchantment = factor(data[,14])
data$Instant = factor(data[,15])
data$White = factor(data[,16])
data$Blue = factor(data[,17])
data$Black = factor(data[,18])
data$Red = factor(data[,19])
data$Green = factor(data[,20])
str(data)

#partitioning data
set.seed(117)
sample <- sample.split(data, SplitRatio = .75)
train <- subset(data, sample == T)
test <-  subset(data, sample == F)

#X_test format
drop <- c('League','Total.Wins')
X_train <- train[,!(names(train) %in% drop)]
X_train <- data.matrix(X_train)
drop <- c('Total.Wins')
y_train <- train[,(names(train) %in% drop)]
y_train <- data.matrix(y_train)

#y_test format
drop <- c('League','Total.Wins')
X_test <- test[,!(names(test) %in% drop)]
X_test <- data.matrix(X_test)
drop <- c('Total.Wins')
y_test <- test[,(names(test) %in% drop)]
y_test <- data.matrix(y_test)


# ridge regression for train set
lambdas <- 10^seq(3, -2,by = -.1)
fit <- glmnet(X_train, y_train, alpha = 0, lambda = lambdas)
summary(fit)

# finding lambda
cv_fit <- cv.glmnet(X_train, y_train, alpha = 0, lambda = lambdas)
par(mfrow=c(1,1))
plot(cv_fit)
opt_lambda <- cv_fit$lambda.min
opt_lambda
fit <- cv_fit$glmnet.fit
summary(fit)
y_predicted <- predict(fit, s = opt_lambda, newx = X_train)

# Sum of Squares Total and Error
sst <- sum((y_train - mean(y_train))^2)
sse <- sum((y_predicted - y_train)^2)

# R squared
rsq <- 1 - sse / sst
rsq
