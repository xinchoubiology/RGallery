#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

//' Get multiple variable normal distribution via Choleski decompsition and Rcpp
//' 
//' @title cmvrnorm
//' @description Produces one or more samples from specified multivariate normal distribution by C-plus-plus
//' @param n the number of samples required. Default 1.
//' @param mu a vector giving the means of variables.
//' @param Sigma a positive-definite symmetric specifying the covariance matrix of variabless
//' @export
//' @examples
//' sigma <- matrix(c(1, 0.9, -0.3, 0.9, 1, -0.4, -0.3, -0.4, 1), ncol = 3)
//' mu <- c(10, 5, -3)
//' var(cmvrnorm(10, mu,  sigma))
//' @return mat a matrix of n samples of p variables on p-mvr
//' @author Eric xin Zhou \url{xxz220@@miami.edu}
// [[Rcpp::export]]
arma::mat cmvrnorm(int n, arma::vec mu, arma::mat Sigma){
  int ncols = Sigma.n_cols;
  arma::mat Y = arma::randn(n, ncols);
  return arma::repmat(mu, 1, n).t() + Y * arma::chol(Sigma);
}
