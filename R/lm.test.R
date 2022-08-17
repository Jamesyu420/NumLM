lm.test <- function(X, y, beta.hat){
  n <- dim(X)[1]
  p <- dim(X)[2]
  X <- cbind(rep(1,n),X)
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
  beta.SE <- sqrt(diag(sigma.hat * solve(t(X)%*%X)))
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
