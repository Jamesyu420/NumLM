load("demo/LMMdata.RData")
data <- as.matrix(data)
library(caret)

# Data splitting
set.seed(42)
index <- createDataPartition(data[,1], p = 0.7, list = FALSE)
data.train <- data[index, ]
data.test <- data[-index, ]
n <- 2000 * 0.7
m <- 2000 * 0.3
y.train <- data.train[,1]
Z.train <- data.train[,2:11]
X.train <- data.train[,12:5011]
y.test <- data.test[,1]
Z.test <- data.test[,2:11]
X.test <- data.test[,12:5011]

model.train <- lmm.EM(y.train, Z.train, X.train, maxit = 2e3, tol = 1e-2)

sbeta2 <- model.train$var.rand.eff
se2 <- model.train$var.resid
like.hist <- model.train$likelihood
omega.hat <- model.train$coef.fix.eff
mu.hat <- model.train$post
y.hat <- Z.test %*% omega.hat + X.test %*% mu.hat
train_error <- sum((Z.train %*% omega.hat +
                      X.train %*% mu.hat - data.train[,1])^2)/n
test_error <- sum((y.hat - y.test)^2)/m
cat("Training error of LMM:", train_error, "\n")
cat("Test error of LMM:", test_error, "\n")
cat("LMM+EM:sbeta2:",sbeta2,",se2:",se2,"\n")
cat("Mean of posterior mean:", mean(model.train$post),"\n")
plot(like.hist, type ='l')
