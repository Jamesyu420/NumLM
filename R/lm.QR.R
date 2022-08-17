lm.QR <- function(X, y){
  n <- dim(X)[1]
  p <- dim(X)[2]
  # Design matrix with intercept
  X <- cbind(rep(1,n),X)
  colnames(X)[1] <- "(intercept)"
  # QR decomposition
  QR <- qr(X)
  Q <- qr.Q(QR)
  R <- qr.R(QR)
  # Estimate beta using Gram-Schmidt
  beta.hat <- as.numeric(backsolve(r = R, x = crossprod(Q, y)))
  names(beta.hat) <- colnames(X)
  beta.hat
}
