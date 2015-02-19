##' Logistical regression data simulation
##' 
##' @title sim_dat_LR
##' @aliases LR_simulate
##' @description A data simulation function which is used for glmnet benchmark. The data set size for L2-regularized 
##' @param N number of observations
##' @param p number of predictors
##' @param rho correlation of X_j and X_{j'}. Default is 0.95
##' @param snr signal-noise ratio threahold. Default is 3.0
##' @param nmu noise distribution's mean. Default is 0.0
##' @param nsd noise distribution's sd. Default is 1.0
##' @param control Optional(TRUE/FALSE), whether control whiten noise and signal's SNR
##' @importFrom MASS mvrnorm
##' @return x p predictors' matrix for N observation.
##' @return y N observation (1 or 0) which is based on real value of y0 and noise.
##' @return beta p real beta value
##' @author Eric xin zhou \url{xxz220@@miami.edu}
sim_dat_LR <- function(N, p, rho = 0.95, snr = 3.0, nmu = 0.0, nsd = 1.0, control = TRUE){
  # multinormal matrix generation subject to cor(xi,xj) == 0.95
  ## sigma build
  sigma <- as.vector(diag(N))
  sigma[which(sigma == 0)] <- rho
  ## x N x p
  x <- t(mvrnorm(n = p, mu = rep(0, N), Sigma = matrix(sigma, nrow = N)))
  ## noise generation
  z <- rnorm(N)
  ## beta generation
  beta <- genBeta(p)
  # control snr or not
  k <- 1
  if(control){
    k <- sum(x %*% beta) / (snr * sum(z))
  }
  # generation response r
  r <- x %*% beta + k * z
  
  # logistic assignment probability p = Pr(G=0|x)
  p <- 1 / (1 + exp(-r))
  ## generate y == G by binomial distribution
  if(length(p) == N){
    y <- rbinom(N, 1, prob = p)
  }else{
    stop("Dimension error")
  }
  return(list(x = x, y = y, beta0 = beta))
}


genBeta <- function(p){
  j <- 1:p
  return((-1)^j * exp(-2*(j-1)/20))
}



