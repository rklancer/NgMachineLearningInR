logistic <- function (x) {
  1/(1+exp(-1*x))
}

lwlr <- function(X_train, y_train, x, tau) {
  # x = c(x1, x2)
  lambda <- 0.0001
  min <- 1e-4
  tau2sq <- 2 * tau * tau
  
  xs <- kronecker(t(x), matrix(1, dim(X)[1]))
  w <-  exp( -1 * as.matrix( rowSums( (xs - X)^2 ) )^(1/2) / tau2sq )
  theta <- c(1,1)
  
  while (TRUE) {
    h_train <- logistic(X %*% theta)
    z = w * (y - h_train)
    grad <- t(X) %*% z - lambda * theta
    
    print(as.vector(grad))
    
    if ( sum( (grad^2)^(1/2) ) < min ) {
      break;
    }
    
    D_iis <- as.vector(w * h_train * (1 - h_train))
    D <- diag(D_iis, nrow = dim(diis)[1])
    H <- t(X) %*% D %*% X - lambda * diag(2)
    delta_theta <- as.vector(solve(H, grad))
    theta <- theta + delta_theta
  }
  print("theta = ")
  print(theta)
  as.numeric(logistic(theta %*% x))
}
