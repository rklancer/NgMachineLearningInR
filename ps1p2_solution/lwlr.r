logistic <- function (x) {
  1/(1+exp(-1*x))
}

lwlr <- function(X_train, y_train, x, tau) {
  # x = c(x1, x2)
  lambda <- 0.0001
  norm_criterion <- 1e-4
  last_grad_norm <- 1e10
  
  tau2sq <- 2 * tau * tau
  
  xs <- kronecker(t(x), matrix(1, dim(X)[1]))
  w <-  exp( -1 * as.matrix( rowSums( (xs - X_train)^2 ) )^(1/2) / tau2sq )
  theta <- c(1,1)
  
  while (TRUE) {
    h_train <- logistic(X %*% theta)
    z <- w * (y_train - h_train)
    grad <- t(X) %*% z - lambda * theta
    
    grad_norm <- sum( (grad^2)^(1/2) )
    if ( (grad_norm > last_grad_norm) || (grad_norm < norm_criterion) ) {
      break;
    }
    last_grad_norm <- grad_norm
    
    D_iis <- as.vector(w * h_train * (1 - h_train))
    D <- diag(D_iis, nrow = length(D_iis))
    H <- t(X) %*% D %*% X - lambda * diag(2)
    delta_theta <- as.vector(solve(H, grad))
    theta <- theta + delta_theta
  }

  logistic(as.numeric(theta %*% x))
}
