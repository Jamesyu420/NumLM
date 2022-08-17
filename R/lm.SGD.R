lm.SGD <- function(X, y, alpha = 0.001, tol = 1e-12, seed = 42){
  set.seed(seed)
  n <- dim(X)[1]
  X <- cbind(rep(1,n),X)
  colnames(X)[1] <- "(intercept)"
  p <- dim(X)[2]
  beta <- rep(0, p)
  for(i in 1:1e5){
    sample_num <- sample(n, 1)
    XX <- X[sample_num,]
    yy <- y[sample_num]
    v <- alpha * c(XX %*% beta - yy) * XX
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
