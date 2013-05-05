#' R scripts for the figures and examples from the book STATISTICS FOR SPATIO-TEMPORAL DATA by Cressie and Wikle 2011.
#'
#' rsstd provides R code for building the figures and examples in the book.
#'
#' @references Noel Cressie, Christopher K. Wikle (2011). Statistics for Spatio-Temporal Data. Wiley. \url{http://eu.wiley.com/WileyCDA/WileyTitle/productCd-EHEP002348.html}.
#' @docType package
#' @name rsstd
NULL



#' Wind observations in the Pacific
#' 
#' A dataset containing wind oobservations
#' 
#' \itemize{
#'   \item u
#'   \item v
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 11323 rows and 2 variables
#' @name T7UV
#' @details No details.
NULL



#' @title First-order affine dynamical system
#'
#' @description
#' Equation 3.11 - page 60
#'
#' @details
#' No details.
#' 
#' @param theta_0 theta_0
#' @param theta_1 theta_1
#' @param alpha_t.minus.1 alpha_t.minus.1
eq3.011.alpha_t <- function(theta_0, theta_1, alpha_t.minus.1){
  theta_0 + theta_1 * alpha_t.minus.1
} 

#' @title First-order affine dynamical system equilibrium
#'
#' @description
#' Equation in page 60 without a number
#'
#' @details
#' No details.
#' 
#' @param theta_0 theta_0
#' @param theta_1 theta_1
eq3.011.equilibrium <- function(theta_0, theta_1){
  if(theta_1 != 1){
    eq <- theta_0 / (1 - theta_1)
  }else{
    eq <- NA
  }
  return (eq)
}

#' @title Dummy for iterating a function that takes only one parameter (the previous result)
#'
#' @description
#' No description.
#'
#' @details
#' No details.
#' 
#' @param f function to iterate. It must be a function with one numeri input parameter and one numeric result (it uses the result of the last iteration as input for the next one)
#' @param alpha_0 Initial value
#' @param iterations Number of iterations
iterate.f1p <- function(f, alpha_0, iterations){
  res <- vector(mode = "numeric", length = iterations)
  for(i in 1:iterations){
    if(i == 1){
      res[i] <- alpha_0
    }else{
      res[i] <- f(res[i - 1])
    }
  }
  return (res)
}

#' @title Simple linear dynamical system
#'
#' @description
#' Equation 3.12 - page 61
#'
#' @details
#' No details.
#' 
#' @param alpha_t_minus1 alpha_t_minus1
eq3.012.alpha_t <- function(alpha_t_minus1){
  eq3.011.alpha_t(theta_0 = -30, theta_1 = 1.05, alpha_t_minus1)
}


