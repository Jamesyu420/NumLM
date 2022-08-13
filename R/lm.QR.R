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
  # Hypothesis testing
  # Fitted values
  fitted <- as.numeric(X %*% beta.hat)
  names(fitted) <- as.character(1:length(y))
  # Residuals
  res <- as.numeric(y - fitted)
  names(res) <- as.character(1:length(y))
  df <- n - p
  sigma.hat <- sum(res^2) / df
  # t-test
  beta.SE <- sqrt(diag(sigma.hat * chol2inv(R)))
  t.value <- beta.hat / beta.SE
  p.value <- 2 * pt(-abs(t.value), df)
  output <- list(
    "coefficients" = beta.hat,
    "fitted.values" = fitted,
    "residuals" = res,
    "t.test" = cbind(beta.hat, beta.SE, t.value, p.value),
    "p.value" = p.value
  )
}
