lm.RecursiveLS <- function(X, y){
  n <- dim(X)[1]
  X <- cbind(rep(1,n),X)
  colnames(X)[1] <- "(intercept)"
  p <- dim(X)[2]
  beta <- lm(y[1:p]~X[1:p,]-1)$coefficients
  A <- solve(t(X[1:p,])%*%X[1:p,])
  for(i in (p+1):n){
    Ax <- A %*% X[i,]
    xAx <- as.numeric(1 + t(X[i,]) %*% Ax)
    A <- A - (Ax %*% t(Ax)) / xAx
    b <- Ax / xAx
    beta <- beta + (y[i] - as.numeric(t(beta) %*% X[i,])) * b
  }
  names(beta) <- colnames(X)
  beta
}
