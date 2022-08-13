lm.QR <- function(X, y){
  X <- as.matrix(iris[,1:4])
  # Design matrix with intercept
  X <- cbind(rep(1,150),X)
  colnames(X)[1] <- "(intercept)"
  n <- dim(X)[1]
  p <- dim(X)[2]
  # QR decomposition
  QR <- qr(X)
  Q <- qr.Q(QR)
  R <- qr.R(QR)
  # Estimate beta using Gram-Schmidt
  beta.hat <- as.numeric(backsolve(r = R, x = crossprod(Q, y)))
  names(beta.hat) <- colnames(X)
  # Hypothesis testing results
  infer <- hypothesis.test(X, y, beta.hat)
  output <- list(
    "coefficients" = beta.hat,
    "fitted.values" = infer$fitted.values,
    "residuals" = infer$residuals,
    "df" = infer$df

  )
}
