#' @title Matern covariance function.
#'
#' @description
#' Equation 4.6 - page 126
#'
#' @details
#' No details.
#' 
#' @param h h
#' @param theta_1 theta_1
#' @param theta_2 theta_2
#' @param variance_0 variance_0
#' @param variance_1 variance_1
eq4.6.gamma_Y_of_h_and_theta <- function(h, theta_1, theta_2, variance_0, variance_1){
  norm_of_h <- norm(as.matrix(h))
  variance_0 * diag(length(h)) * (as.numeric(norm_of_h != 0)) + variance_1 * (2^(theta_2 - 1) * gamma(theta_2))^-1 * (norm_of_h/theta_1)^theta_2 * besselY(x = (norm_of_h/theta_1), nu = theta_2)
} 