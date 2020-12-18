rm(list=ls())


library(moments)
library(glmnet)
library(caTools)
library(RColorBrewer)
library(gradDescent)


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


# checking scatterplots
par(mfrow = c(3,2))
plot(x=data$Total.Cost, y=data$Total.Wins, 
     main = "Total Cost per League vs. Total Wins per League", 
     xlab = "Total Cost per League", ylab = "Total Wins per League")
plot(x=data$Avg.Cost, y=data$Total.Wins, 
     main = "Avg. Cost per Card vs. Total Wins per League", 
     xlab = "Avg. Cost per Card", ylab = "Total Wins per League")
plot(x=data$Avg.CMC, y=data$Total.Wins, 
     main = "Avg. CMC per League vs. Total Wins per League", 
     xlab = "Avg. CMC per League", ylab = "Total Wins per League")
plot(x=data$Avg.Power, y=data$Total.Wins, 
     main = "Avg. Power per League vs. Total Wins per League", 
     xlab = "Avg. Power per League", ylab = "Total Wins per League")
plot(x=data$Avg.Toughness, y=data$Total.Wins, 
     main = "Avg. Toughness per League vs. Total Wins per League", 
     xlab = "Avg. Toughness per League", ylab = "Total Wins per League")
     

# checking skewness before transformation
skewness = matrix(nrow = 5, ncol = 2, 
                  dimnames = list( c("Total Cost", "Avg. Cost", "Avg. CMC", 
                                     "Avg. Power",  "Avg. Toughness"), 
                                   c("Skewness Before", "Skewness After")))

# note: bs means before skewness
bs1 = skewness(data$Total.Cost)
bs2 = skewness(data$Avg.Cost)
bs3 = skewness(data$Avg.CMC)
bs4 = skewness(data$Avg.Power)
bs5 = skewness(data$Avg.Toughness)

skewness[1] = bs1
skewness[2] = bs2
skewness[3] = bs3
skewness[4] = bs4
skewness[5] = bs5


# checking kurtosis before transformation
kurtosis = matrix(nrow = 5, ncol = 2, 
                  dimnames = list( c("Total Cost", "Avg. Cost", "Avg. CMC", 
                                     "Avg. Power",  "Avg. Toughness"), 
                                   c("Kurtosis Before", "Kurtosis After")))

# note: bk means before kurtosis
bk1 = kurtosis(data$Total.Cost)
bk2 = kurtosis(data$Avg.Cost)
bk3 = kurtosis(data$Avg.CMC)
bk4 = kurtosis(data$Avg.Power)
bk5 = kurtosis(data$Avg.Toughness)

kurtosis[1] = bk1
kurtosis[2] = bk2
kurtosis[3] = bk3
kurtosis[4] = bk4
kurtosis[5] = bk5


# Data transformation
data$Total.Cost = log(data$Total.Cost)
data$Avg.Cost = log(data$Avg.Cost)
data$Avg.CMC = log(data$Avg.CMC)
data$Avg.Power = log(data$Avg.Power)
data$Avg.Toughness= log(data$Avg.Toughness)


# New histograms after transformation
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


# New scatterplots after transformation
par(mfrow = c(3,2))
plot(x=data$Total.Cost, y=data$Total.Wins, 
     main = "Total Cost per League vs. Total Wins per League", 
     xlab = "Total Cost per League", ylab = "Total Wins per League")
plot(x=data$Avg.Cost, y=data$Total.Wins, 
     main = "Avg. Cost per Card vs. Total Wins per League", 
     xlab = "Avg. Cost per Card", ylab = "Total Wins per League")
plot(x=data$Avg.CMC, y=data$Total.Wins, 
     main = "Avg. CMC per League vs. Total Wins per League", 
     xlab = "Avg. CMC per League", ylab = "Total Wins per League")
plot(x=data$Avg.Power, y=data$Total.Wins, 
     main = "Avg. Power per League vs. Total Wins per League", 
     xlab = "Avg. Power per League", ylab = "Total Wins per League")
plot(x=data$Avg.Toughness, y=data$Total.Wins, 
     main = "Avg. Toughness per League vs. Total Wins per League", 
     xlab = "Avg. Toughness per League", ylab = "Total Wins per League")


# Skewness After Transformation

# note: as means after skewness
as1 = skewness(data$Total.Cost)
as2 = skewness(data$Avg.Cost)
as3 = skewness(data$Avg.CMC)
as4 = skewness(data$Avg.Power)
as5 = skewness(data$Avg.Toughness)

skewness[6] = as1
skewness[7] = as2
skewness[8] = as3
skewness[9] = as4
skewness[10] = as5

skewness


# Kurtosis after transformation

# note: ak means after kurtosis
ak1 = kurtosis(data$Total.Cost)
ak2 = kurtosis(data$Avg.Cost)
ak3 = kurtosis(data$Avg.CMC)
ak4 = kurtosis(data$Avg.Power)
ak5 = kurtosis(data$Avg.Toughness)

kurtosis[6] = ak1
kurtosis[7] = ak2
kurtosis[8] = ak3
kurtosis[9] = ak4
kurtosis[10] = ak5

kurtosis


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
sample = sample.split(data, SplitRatio = .75)
train = subset(data, sample == T)
test =  subset(data, sample == F)


# Train format
drop = c('League','Total.Wins')
X_train = train[,!(names(train) %in% drop)]
X_train = data.matrix(X_train)
drop = c('Total.Wins')
y_train = train[,(names(train) %in% drop)]
y_train = data.matrix(y_train)


# Test format
drop = c('League','Total.Wins')
X_test = test[,!(names(test) %in% drop)]
X_test = data.matrix(X_test)
drop = c('Total.Wins')
y_test = test[,(names(test) %in% drop)]
y_test = data.matrix(y_test)


# Gradient descent
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
gd.data =cbind(X_train, y_train)
gd.model.delta = ADADELTA(gd.data, seed=117)
gd.model.delta
i=0
results=matrix(nrow=36, ncol=1)
for (i in 1:36){
        tempval = as.numeric(gd.model.delta[1]) + 
                as.numeric(gd.model.delta[2]*X_test[i,1]) + 
                as.numeric(gd.model.delta[3]*X_test[i,2]) + 
                as.numeric(gd.model.delta[4]*X_test[i,3]) + 
                as.numeric(gd.model.delta[5]*X_test[i,4]) + 
                as.numeric(gd.model.delta[6]*X_test[i,5]) + 
                as.numeric(gd.model.delta[7]*X_test[i,6]) + 
                as.numeric(gd.model.delta[8]*X_test[i,7]) + 
                as.numeric(gd.model.delta[9]*X_test[i,8]) + 
                as.numeric(gd.model.delta[10]*X_test[i,9]) + 
                as.numeric(gd.model.delta[11]*X_test[i,10]) + 
                as.numeric(gd.model.delta[12]*X_test[i,11]) + 
                as.numeric(gd.model.delta[13]*X_test[i,12]) + 
                as.numeric(gd.model.delta[14]*X_test[i,13]) + 
                as.numeric(gd.model.delta[15]*X_test[i,14]) + 
                as.numeric(gd.model.delta[16]*X_test[i,15]) + 
                as.numeric(gd.model.delta[17]*X_test[i,16]) + 
                as.numeric(gd.model.delta[18]*X_test[i,17]) + 
                as.numeric(gd.model.delta[19]*X_test[i,18]) 
        results[i,1] = tempval
}

mean((results[,1] - y_test)**2)



# ridge regression for train set
lambdas = 10^seq(3, -2,by = -.1)
fit1 = glmnet(X_train, y_train, alpha = 0, lambda = lambdas)
summary(fit1)


# finding lambda
cv.fit = cv.glmnet(X_train, y_train, alpha = 0, lambda = lambdas)
par(mfrow=c(1,1))
plot(cv.fit)
opt_lambda = cv.fit$lambda.min
opt_lambda
coef(cv.fit, opt_lambda)
fit2 = cv.fit$glmnet.fit
y_predicted = predict(fit1, s = opt_lambda, newx = X_train)


# Sum of Squares Total and Error
sst = sum((y_train - mean(y_train))^2)
sse = sum((y_train - y_predicted)^2)


# R squared
rsq = 1 - (sse / sst)
rsq


# Graphs for Ridge

mypalette <- brewer.pal(12,"Set3")
mypalette

# need to have viewing window big enough or legend can cause issues
par(mar=c(5, 5, 5, 14), xpd=TRUE)
plot(fit1, xvar = "lambda", label=TRUE, lwd=3, col=mypalette, 
     main="Ridge Penalty Regression")
abline(h=0, lwd=1, lty=2, col="grey")
legend("topright", inset=c(-.55,0), lwd=3, lty=1, legend=colnames(X_train), 
       col=mypalette)

par(mfrow=c(1,1), xpd = F)
hist(predict(fit1, newx = X_train, s = opt_lambda, 
             type="response"), xlab="Predicted Number of Wins", 
     main="Histogram of Predicted Number of Wins") #prediction

