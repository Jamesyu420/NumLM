lm.GD <- function(X, y, alpha = 0.001, tol = 1e-7){
  n <- dim(X)[1]
  X <- cbind(rep(1,n),X)
  colnames(X)[1] <- "(intercept)"
  p <- dim(X)[2]
  beta <- rep(0, p)
  for(i in 1:1e4){
    v <- alpha * as.numeric(t(X %*% beta - y) %*% X)
    if(sum(v^2) < tol){
      break
    }
    else{
      beta <- beta - v
    }
  }
  names(beta) <- colnames(X)
  beta
}
