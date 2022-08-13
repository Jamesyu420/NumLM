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
  # Hypothesis test
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
