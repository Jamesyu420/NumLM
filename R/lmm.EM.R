# For a comparison, we hereby show the result of LMM solved by EM algorithm.
lmm.EM <- function(y, Z, X, maxit = 1e3, tol = 1e-3){
  c <- ncol(Z)
  p <- ncol(X)
  n <- length(y)

  # initial estimates
  lm.fit <- lm(y~Z-1)
  omega <- coef(lm.fit)
  se2 <- summary(lm.fit)$sigma^2
  sbeta2 <- 1
  likelihood <- NA
  likelihood.hist <- c()
  XXt <- X%*%t(X)
  XXt.eigen <- eigen(XXt)
  XXt.eigen.values <- XXt.eigen$values
  XXt.eigen.vectors <- XXt.eigen$vectors

  for(i in 1:maxit){
    # E-step
    r <- y - Z %*% omega
    ratio <- se2/sbeta2
    Diag <- (XXt.eigen.values + ratio) / se2
    mu <- 1/se2 * t(X) %*% (XXt.eigen.vectors %*%
                              (t(XXt.eigen.vectors)%*%r / Diag))
    # mu <- solve(XtX+ratio*diag(rep(1,p)),t(X)) %*% r Problem solved!
    Sigma.XtX.tr <- sum(1/(1+ratio/XXt.eigen.values)) * se2
    Sigma.tr <- (sum(1/(XXt.eigen.values + ratio))+(p-n)/ratio) * se2
    mutmu <- t(mu) %*% mu
    X.mu <- X %*% mu
    ## expectation
    # Marginal likelihood
    curr.likelihood <- -sum(log(Diag))/2 - n/2*log(sbeta2) - n/2*log(se2) -
      n/2*log(2*pi) - 1/2*sum(r^2/Diag)
    if(!is.na(likelihood) & abs(curr.likelihood - likelihood) < tol)
      break
    likelihood <- curr.likelihood
    likelihood.hist <- c(likelihood.hist, curr.likelihood)
    cat("Iter:",i,",marginal likelihood:",likelihood,
        ",sbeta2:",sbeta2,",se2:",se2,"\n")
    # M step
    omega <- solve(t(Z)%*%Z,t(Z)) %*% (y-X.mu)
    se2 <- as.numeric((t(r-X.mu) %*% (r-X.mu) + Sigma.XtX.tr) / n)
    sbeta2 <- as.numeric((mutmu + Sigma.tr) / p)
  }

  list(coef.fix.eff = omega,
       var.rand.eff = sbeta2,
       var.resid = se2,
       post = mu,
       likelihood = likelihood.hist)
}
