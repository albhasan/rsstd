#' @title Matern covariance function.
#'
#' @description
#' Equation 4.6 - page 126
#'
#' @details
#' OUT OF ORDER
#' 
#' @param h h
#' @param theta_1 theta_1
#' @param theta_2 theta_2
#' @param variance_0 variance_0
#' @param variance_1 variance_1
eq4.6.C_Y_of_h_and_theta <- function(h, theta_1, theta_2, variance_0, variance_1){
  #TODO: It is not working. I couldn't understand the first term of the equation.
  #norm_of_h <- sqrt(sum(h^2)) # norm(as.matrix(h))
  #res <- 0
  #if (norm_of_h != 0){
  #  res <- variance_0 * diag(x = h, nrow = length(h), ncol = length(h)) * (as.numeric(norm_of_h == 0)) + variance_1 * (2^(theta_2 - 1) * gamma(theta_2))^-1 * (norm_of_h/theta_1)^theta_2 * besselY(x = (norm_of_h/theta_1), nu = theta_2)
  #}
  # return(as.vector(res))
} 



#' @title Powered-exponential isotropic variogram
#'
#' @description
#' Equation 4.7 - page 127
#'
#' @details
#' OUT OF ORDER
#' 
#' @param h h
#' @param theta_1 theta_1
#' @param theta_2 theta_2
#' @param variance_0 variance_0
#' @param variance_1 variance_1
eq4.7.gamma_Y_of_h_and_theta <- function(h, theta_1, theta_2, variance_0, variance_1){
  #TODO: It is not working. I couldn't understand the first term of the equation.
} 