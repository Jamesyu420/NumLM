load("demo/LMMdata.RData")
data <- as.matrix(data)
library(caret)

set.seed(42)
index <- createDataPartition(data[,1], p = 0.7, list = FALSE)
m <- 2000 * 0.3
data.train <- data[index, ]
data.test <- data[-index, ]
y.train <- data.train[,1]
Z.train <- data.train[,2:11]
y.test <- data.test[,1]
Z.test <- data.test[,2:11]

# QR decomposition
system.time(
  for(i in 1:100){
    model.QR.coef <- lm.QR(Z.train, y.train)
  }
)
y.test.QR.pred <- Z.test %*% model.QR.coef[2:11] + model.QR.coef[1]
test_error.QR <- sum((y.test.QR.pred - y.test)^2)/m
cat("Test error of LM-QR:", test_error.QR, "\n")
model.QR <- lm.test(Z.train, y.train, model.QR.coef)
model.QR$t.test


# System solution
system.time(
  for(i in 1:100){
    model <- lm(y.train~Z.train)
  }
)
model.coef <- model$coefficients
y.test.sys.pred <- Z.test %*% model.coef[2:11] + model.coef[1]
test_error.sys <- sum((y.test.sys.pred - y.test)^2)/m
cat("Test error of LM-sys:", test_error.sys, "\n")

# Gradient descent
system.time(
  for(i in 1:100){
    model.GD.coef <- lm.GD(Z.train, y.train)
  }
)
y.test.GD.pred <- Z.test %*% model.GD.coef[2:11] + model.GD.coef[1]
test_error.GD <- sum((y.test.GD.pred - y.test)^2)/m
cat("Test error of LM-GD:", test_error.GD, "\n")

model.GD <- lm.test(Z.train, y.train, model.GD.coef)
model.GD$t.test

# Online update

