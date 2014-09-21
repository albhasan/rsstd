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


#' @title Simple linear dynamical system
#'
#' @description
#' Equation 3.12 - page 61
#'
#' @details
#' No details.
#' 
#' @param alpha_t.minus.1 alpha_t.minus.1
eq3.012.alpha_t <- function(alpha_t.minus.1){
  eq3.011.alpha_t(theta_0 = -30, theta_1 = 1.05, alpha_t.minus.1)
}


#' @title Second-order linear discrete homogeneus dynamical system
#'
#' @description
#' Equation 3.14 - page 62
#'
#' @details
#' No details.
#' 
#' @param theta_1 theta_1
#' @param theta_2 theta_2
#' @param alpha_t.minus.1 alpha_t.minus.1
#' @param alpha_t.minus.2 alpha_t.minus.2
eq3.014.alpha_t <- function(theta_1, theta_2, alpha_t.minus.1, alpha_t.minus.2){
  theta_1 * alpha_t.minus.1 + theta_2 * alpha_t.minus.2
}


#' @title Characteristic polynomial roots
#'
#' @description
#' No description
#'
#' @details
#' No details.
#' 
#' @param theta_1 theta_1
#' @param theta_2 theta_2
characteristicPolynomialRoots <- function(theta_1, theta_2){
  polyroot(c(1, -theta_1, -theta_2))
}


#' @title General solution of the second-order system when z1 != z2
#'
#' @description
#' Equation 3.19 - page 63
#'
#' @details
#' No details.
#' 
#' @param c_1 c_1
#' @param c_2 c_2
#' @param z_1 z_1
#' @param z_2 z_2
#' @param t t
eq3.019.alpha_t <- function(c_1, c_2, z_1, z_2, t){
  c_1 * z_1^-t + c_2 * z_2-t
}


#' @title Constant c1 for the case 1 of the general solution of the second-order system
#'
#' @description
#' Equation 3.20 - page 63
#'
#' @details
#' No details.
#' 
#' @param alpha_0 alpha_0
#' @param alpha_1 alpha_1
#' @param z_1 z_1
#' @param z_2 z_2
eq3.020.c_1 <- function(alpha_0, alpha_1, z_1, z_2){
  (alpha_1 - (alpha_0 * z_2^(-1)))/(z_1^(-1) - z_2^(-1))
}


#' @title Constant c2 for the case 1 of the general solution of the second-order system
#'
#' @description
#' Equation 3.20 - page 63
#'
#' @details
#' No details.
#' 
#' @param alpha_0 alpha_0
#' @param alpha_1 alpha_1
#' @param z_1 z_1
#' @param z_2 z_2
eq3.020.c_2 <- function(alpha_0, alpha_1, z_1, z_2){
  (alpha_1 - alpha_0 * z_1^-1)/(z_2^-1 - z_1^-1)
}


#' @title Constant c1 for the case 1 of the general solution of the second-order system
#'
#' @description
#' Equation 3.24 - page 64
#'
#' @details
#' No details.
#' 
#' @param c_1 c_1
#' @param psi psi
eq3.024.c_1 <- function(c_1, psi){
  i <- complex(real = 0, imaginary = 1)
  abs(c_1) * exp(i * psi)
}


#' @title z1 for the case 1 of the general solution of the second-order system
#'
#' @description
#' Equation 3.24 - page 64
#'
#' @details
#' No details.
#' 
#' @param z_1 z_1
#' @param phi phi
eq3.024.z_1 <- function(z_1, phi){
  i <- complex(real = 0, imaginary = 1)
  abs(z_1) * exp(i * phi)
}


#' @title General solution re-written
#'
#' @description
#' Equation 3.25 - page 64
#'
#' @details
#' No details.
#' 
#' @param c_1 c_1
#' @param z_1 z_1
#' @param t t
#' @param phi phi
#' @param psi psi
eq3.025.alpha_t <- function(c_1, z_1, t, phi, psi){
  2 * abs(c_1) * abs(z_1)^-t * cos(t*phi + psi)
}


#' @title Dynamical system
#'
#' @description
#' Equation 3.30 - page 65
#'
#' @details
#' No details.
#' 
#' @param alpha_t.minus.2 alpha_t.minus.2
eq3.030.alpha_t <- function(alpha_t.minus.2){
  eq3.014.alpha_t(theta_1 = 0, theta_2 = -0.64, alpha_t.minus.1 = 0, alpha_t.minus.2 = alpha_t.minus.2)
}


#' @title Non analytic solution of the second-order system
#'
#' @description
#' Function for solving the second-order system on page 65. This function iterates the whole time series.
#'
#' @details
#' No details.
#' 
#' @param f A second-order system function like 3.30
#' @param alpha_0 alpha_0
#' @param alpha_1 alpha_1
#' @param iterations iterations
sous.noAnalytic <- function(f, alpha_0, alpha_1, iterations){
  res <- rep(x = NA, times = iterations)
  for(i in 1:iterations){
    if(i == 1){
      res[i] <- alpha_0
    }else if(i == 2){
      res[i] <- alpha_1
    }else{
      res[i] <- f(res[i-2])
    }
  }
  return (res)
}


#' @title Analytic solution of the second-order system
#'
#' @description
#' Function for solving the second-order system on page 65. This function does not iterate the whole time series.
#'
#' @details
#' No details.
#' 
#' @param alpha_0 alpha_0
#' @param alpha_1 alpha_1
#' @param theta_1 theta_1
#' @param theta_2 theta_2
#' @param iterations iterations
sous.analytic <- function(alpha_0, alpha_1, theta_1, theta_2, iterations){
  #Vector for storing the results
  alpha_t.vector <- rep(x = NA, times = iterations)
  alpha_t.vector[1] <- alpha_0
  alpha_t.vector[2] <- alpha_1
  #Get the characteristic polynomial roots
  cpr <- characteristicPolynomialRoots(theta_1 = theta_1, theta_2 = theta_2)
  z_1 <- cpr[1]
  z_2 <- cpr[2]
  #Select case
  if(z_1 != z_2){
    #CASE 1
    c_1 <- eq3.020.c_1(alpha_0 = alpha_0, alpha_1 = alpha_1, z_1 = z_1, z_2 = z_2)
    c_2 <- eq3.020.c_2(alpha_0 = alpha_0, alpha_1 = alpha_1, z_1 = z_1,z_2 = z_2)
    if(Conj(z_1) == z_2){
      phi <- Arg(z_1)
      psi <- Arg(c_1)
      psi <- abs(psi)#In the book psi==pi/2
      c_1 <- eq3.024.c_1(c_1 = c_1,psi = psi)
      c_1 <- Im(c_1)#In the book abs(c_1)==3.125 instead of 3.125i
      z_1 <- eq3.024.z_1(z_1 = z_1, phi = phi)
      for(t in 3:iterations){
        alpha_t.vector[t] <- eq3.025.alpha_t(c_1 = c_1, z_1 = z_1, t = t, phi = phi, psi = psi)
      }
    }else{#z_1 and z_2 are not complex conjugate pair
      for(t in 3:iterations){
        alpha_t.vector[t] <- eq3.019.alpha_t(c_1 = c_1, c_2 = c_2, z_1 = z_1, z_2 = z_2, t = t)
      }
    }
  }else if(z_1 == z_2){
    #CASE 2
    print("CASE 2 not implemented")
  }
  return (alpha_t.vector)
}


#' @title Sensitivity matrix associated with the i-th latent root
#'
#' @description
#' Equation 3.46 - page 69
#'
#' @details
#' No details.
#' 
#' @param vhat_i vhat_i
#' @param wapos_i wapos_i
#' @param vapos_i vapos_i
#' @param w_i w_i
eq3.046.S_i <- function(vhat_i, wapos_i, vapos_i, w_i){
  vhat_i %*% wapos_i / (vapos_i %*% w_i)
}

#' @title Utility function for calculating the sensitivity matrix associated (eq 3.46)
#'
#' @description
#' No description
#'
#' @details
#' No details.
#' 
#' @param M Matrix M
#' @param delta Value used to change each element of M in order to estimate sensibility
sensibilityMatrix <- function(M, delta){
  sv <- svd(M)
  S_i.matrix <- matrix(data = NA, nrow = nrow(M), ncol = ncol(M))
  for(i in 1:nrow(M)){
    for(j in 1:ncol(M)){
      M.test <- M
      M.test[i, j] <- M[i, j] - delta
      sv.test <- svd(M.test)
      w_1 <- sv$v[,j]#What column should be used?
      v_1 <- sv$u[,j]
      w_2 <- sv.test$v[,j]#What column should be used?
      v_2 <- sv.test$u[,j]    
      S_i <- eq3.046.S_i(Conj(v_1), w_2, v_2, w_1)
      S_i.matrix[i, j] <- S_i
    }
  }
  return (S_i.matrix)
}


#' @title Logistic equation or logistic dynamical system
#'
#' @description
#' Equation 3.49 - page 71
#'
#' @details
#' No details.
#' 
#' @param theta_0 theta_0
#' @param theta_1 theta_1
#' @param alpha_t.minus.1 alpha_t.minus.1
eq3.049.alpha_t <- function(theta_0, theta_1, alpha_t.minus.1){
  (1 + theta_1) * alpha_t.minus.1 - (theta_1/theta_0) * alpha_t.minus.1^2
}


#' @title One parameter logistic equation
#'
#' @description
#' Equation 3.50 - page 73
#'
#' @details
#' No details.
#' 
#' @param theta theta
#' @param alpha_t.minus.1 alpha_t.minus.1
eq3.050.alpha_t <- function(theta, alpha_t.minus.1){
  theta * alpha_t.minus.1 * (1 - alpha_t.minus.1)
}


#' @title One-parameter logistic equation equilibrium
#'
#' @description
#' Equation in page 73 without a number
#'
#' @details
#' No details.
#' 
#' @param theta theta
eq3.050.equilibrium <- function(theta){
  a0 <- 0
  a1 <- (theta - 1)/theta 
  return (c(a0, a1))
}


#' @title One-parameter logistic equation equilibrium
#'
#' @description
#' Equation 3.51 - page 74
#'
#' @details
#' No details.
#' 
#' @param theta theta
eq3.051.equilibrium <- function(theta){
  #Supress warning related to NaNs production
  ow = options()$warn
  options(warn = -1)
  #Equation
  a0 <- sqrt(theta + 1) * ((sqrt(theta + 1) - sqrt(theta - 3))/(2 * theta))
  a1 <- sqrt(theta + 1) * ((sqrt(theta + 1) + sqrt(theta - 3))/(2 * theta))
  #Restores the option
  options(warn = ow)
  #Return
  return(c(a0, a1))
}


#' @title Simple two-dimensional nonlinear system (alpha)
#'
#' @description
#' Equation 3.52 - page 76
#'
#' @details
#' No details.
#' 
#' @param theta_1 theta_1
#' @param theta_2 theta_2
#' @param  alpha_t.minus.1 alpha_t.minus.1
#' @param  beta_t.minus.1 beta_t.minus.1
eq3.052.alpha_t <- function(alpha_t.minus.1, beta_t.minus.1, theta_1, theta_2){
  1 + beta_t.minus.1 - theta_1 * alpha_t.minus.1^2
}

#' @title Simple two-dimensional nonlinear system (beta)
#'
#' @description
#' Equation 3.53 - page 76
#'
#' @details
#' No details.
#' 
#' @param theta_2 theta_2
#' @param alpha_t.minus.1 alpha_t.minus.1
eq3.053.beta_t <- function(alpha_t.minus.1, theta_2){
  theta_2 * alpha_t.minus.1
}


#' @title Gaussian white noise
#'
#' @description
#' Example on page 85
#'
#' @details
#' No details.
#' 
#' @param n number of desired samples
#' @param mean mean
#' @param sd standard deviation
whiteNoise.gaussian <- function(n, mean, sd){
  rnorm(n = n, mean = mean, sd = sd)
}

#' @title Gaussian random walk
#'
#' @description
#' Example on page 86
#'
#' @details
#' No details.
#' 
#' @param n number of desired samples
#' @param mean mean
#' @param sd standard deviation
randomWalk.gaussian <- function(n, mean, sd){
  res <- vector(mode = "numeric", length = n - 1)
  wng <- whiteNoise.gaussian(n = n, mean = mean, sd = sd)
  for (i in 2:n){
    res[i] <- res[i - 1] + wng[i]
  }
  return (res)
}


#' @title Random walk
#'
#' @description
#' Example on page 86
#'
#' @details
#' No details.
#' 
#' @param whiteNoise.vector whiteNoise.vector
randomWalk <- function(whiteNoise.vector){
  n <- length(whiteNoise.vector)
  res <- vector(mode = "numeric", length = n - 1)
  res[1] <- whiteNoise.vector[1]
  for (i in 2:n){
    res[i] <- res[i - 1] + whiteNoise.vector[i]
  }
  return (res)
}


#' @title Autoregressive (1) process
#'
#' @description
#' Equation 3.77 - page 86
#'
#' @details
#' No details.
#' 
#' @param alpha_1 alpha_1
#' @param Y_t.minus.1 Y_t.minus.1
#' @param W_t W_t
eq3.077.Y_t <- function(alpha_1, Y_t.minus.1, W_t){
  alpha_1 * Y_t.minus.1 + W_t
}


#' @title Spectral density for the Autoregressive (1) process 
#'
#' @description
#' Equation 3.81 - page 87
#'
#' @details
#' No details.
#' 
#' @param omega omega
#' @param var_w var_w
#' @param alpha alpha
eq3.081.f_of_omega <- function(omega, var_w, alpha){
  if(omega > -0.5 & omega < 0.5){
    squared_alpha <- alpha^2
    res <- var_w/(1 - 2 * alpha * cos(2 * pi * omega) + squared_alpha)
  }
  return(res)
}


#' @title Autoregressive (2) process
#'
#' @description
#' Equation 3.82 - page 88
#'
#' @details
#' No details.
#' 
#' @param alpha_1 alpha_1
#' @param alpha_2 alpha_2
#' @param Y_t.minus.1 Y_t.minus.1
#' @param Y_t.minus.2 Y_t.minus.2
#' @param W_t W_t
eq3.082.Y_t <- function(alpha_1, alpha_2, Y_t.minus.1, Y_t.minus.2, W_t){
  alpha_1 * Y_t.minus.1 + alpha_2 * Y_t.minus.2 + W_t
}


#' @title Spectral density of the Autoregressive (2) process 
#'
#' @description
#' Equation 3.84 - page 89
#'
#' @details
#' No details.
#' 
#' @param omega omega
#' @param var_w var_w
#' @param alpha_1 alpha_1
#' @param alpha_2 alpha_2
eq3.084.f_of_omega <- function(omega, var_w, alpha_1, alpha_2){
  if(omega > -0.5 & omega < 0.5){
    res <- var_w/(1 + alpha_1^2 + alpha_2^2 - 2 * alpha_1 * (1 - alpha_2) * cos(2 * pi * omega) - 2 * alpha_2 * cos(4 * pi * omega))
  }
  return(res)
}


#' @title Moving Average Process adapted for q = 1
#'
#' @description
#' Equation 3.90 - page 91
#'
#' @details
#' No details.
#' 
#' @param W_t W_t
#' @param beta_1 beta_1
#' @param W_tminus1 W_tminus1
eq3.090.Y_t <- function(W_t, beta_1, W_tminus1){
  W_t + beta_1 * W_tminus1
}


#' @title Spectral density for Moving Average 1 MA(1) process
#'
#' @description
#' Equation 3.92 - page 91
#'
#' @details
#' No details.
#' 
#' @param omega omega
#' @param var_w var_w
#' @param beta beta
eq3.092.f_of_omega <- function(omega, var_w, beta){
  if(omega > -0.5 & omega < 0.5){
    res <- var_w * (1 + beta^2 + (2 * beta * cos(2 * pi * omega)))
  }
  return(res)
}


#' @title T Basis function phi a0 of t
#'
#' @description
#' Equation 3.134 - page104
#'
#' @details
#' No details.
#' 
#' @param T T
eq3.134.phi_a0_of_t <- function(T){
  return(sqrt(1/T))
}

#' @title T Basis function phi ak of t
#'
#' @description
#' Equation 3.135 - page104
#'
#' @details
#' No details.
#' 
#' @param T T
#' @param t t
#' @param k k
eq3.135.phi_ak_of_t <- function(T, t, k){
  return (sqrt(2/T) * cos(2 * pi * k * t/T))
}


#' @title T Basis function phi bk of t
#'
#' @description
#' Equation 3.136 - page104
#'
#' @details
#' No details.
#' 
#' @param T T
#' @param t t
#' @param k k
eq3.136.phi_bk_of_t <- function(T, t, k){
  return (sqrt(2/T) * sin(2 * pi * k * t/T))
}


#' @title T Basis function phi a half T of t
#'
#' @description
#' Equation 3.137 - page104
#'
#' @details
#' No details.
#' 
#' @param T T
#' @param t t
eq3.137.phi_aT2_of_t <- function(T, t){
  return(sqrt(1/T) * cos(pi * t))
}


#' @title Example time series
#'
#' @description
#' Equation 3.138 - page105
#'
#' @details
#' No details.
eq3.138.Y <- function(){
  return(c(-0.0831, 0.7187, 0.2182, 0.3068, -0.9568, -0.4170))
}


#' @title T Basis function phi a0 of t
#'
#' @description
#' Equation 3.139 - page105
#'
#' @details
#' No details.
#' 
#' 
eq3.139.phi_a0_of_t <- function(){
  T <- 6
  return(eq3.134.phi_a0_of_t(T))
}


#' @title T Basis function phi ak of t
#'
#' @description
#' Equation 3.140 - page105
#'
#' @details
#' No details.
#' 
#' @param t t
eq3.140.phi_ak_of_t <- function(t){
  T <- 6
  k <- 1
  return (eq3.135.phi_ak_of_t(T, t, k))
}


#' @title T Basis function phi bk of t
#'
#' @description
#' Equation 3.141 - page105
#'
#' @details
#' No details.
#' 
#' @param t t
eq3.141.phi_bk_of_t <- function(t){
  T <- 6
  k <- 1
  return (eq3.136.phi_bk_of_t(T, t, k))
}


#' @title T Basis function phi ak of t
#'
#' @description
#' Equation 3.142 - page105
#'
#' @details
#' No details.
#' 
#' @param t t
eq3.142.phi_ak_of_t <- function(t){
  T <- 6
  k <- 2
  return (eq3.135.phi_ak_of_t(T, t, k))
}


#' @title T Basis function phi bk of t
#'
#' @description
#' Equation 3.143 - page105
#'
#' @details
#' No details.
#' 
#' @param t t
eq3.143.phi_bk_of_t <- function(t){
  T <- 6
  k <- 2
  return (eq3.136.phi_bk_of_t(T, t, k))
}


#' @title T Basis function phi a half T of t
#'
#' @description
#' Equation 3.144 - page105
#'
#' @details
#' No details.
#' 
#' @param T T
#' @param t t
eq3.144.phi_aT2_of_t <- function(T, t){
  T <- 6
  return (eq3.137.phi_aT2_of_t(T, t))
}


#' @title Matrix PHI
#'
#' @description
#' Equation 3.145 - page106
#'
#' @details
#' No details.
eq3.145.PHI <- function(){
  T <- 6
  tVector <- seq(from = 1, to = 6, by = 1)
  phi_a0 <- vector(mode = "numeric", length = T)
  phi_a1 <- vector(mode = "numeric", length = T)
  phi_a2 <- vector(mode = "numeric", length = T)
  phi_a3 <- vector(mode = "numeric", length = T)
  phi_b1 <- vector(mode = "numeric", length = T)
  phi_b2 <- vector(mode = "numeric", length = T)
  for (t in tVector){
    phi_a0[t] <- eq3.139.phi_a0_of_t()
    phi_a1[t] <- eq3.140.phi_ak_of_t(t)
    phi_a2[t] <- eq3.142.phi_ak_of_t(t)
    phi_a3[t] <- eq3.144.phi_aT2_of_t(T, t)
    phi_b1[t] <- eq3.141.phi_bk_of_t(t)
    phi_b2[t] <- eq3.143.phi_bk_of_t(t)
  }
  PHI <- cbind(phi_a0, phi_a1, phi_b1, phi_a2, phi_b2, phi_a3)
  return(PHI)
}


#' @title alpha vector
#'
#' @description
#' Equation 3.146 - page106
#'
#' @details
#' No details.
eq3.146.alpha <- function(){
  PHI <- eq3.145.PHI()
  y <- cbind(eq3.138.Y())
  alpha <- t(PHI) %*% y
  return(alpha)
}



#' @title Amplitudes
#'
#' @description
#' Equation 3.147 - page106
#'
#' @details
#' No details.
eq3.147.amplitudes <- function(){
  alphaVector <- eq3.146.alpha()
  A_0 <- alphaVector[1]
  A_1 <- sqrt(alphaVector[2]^2 + alphaVector[3]^2)
  A_2 <- sqrt(alphaVector[4]^2 + alphaVector[5]^2)
  A_3 <- alphaVector[6]
  return(c(A_0, A_1, A_2, A_3))
}


#' @title Filtered coeficients
#'
#' @description
#' Equation 3.148 - page106
#'
#' @details
#' No details.
eq3.148.alpha_R <- function(){
  alphaVector <- as.vector(eq3.146.alpha())
  amplitudVector <- eq3.147.amplitudes()
  newAmplitudVector <- c(amplitudVector[1], amplitudVector[2], amplitudVector[2], amplitudVector[3], amplitudVector[3], amplitudVector[4])
  filtered_alphaVector <- alphaVector * as.numeric(newAmplitudVector > 0.5)
  return(filtered_alphaVector)
}


#' @title Filtered coeficients
#'
#' @description
#' Equation 3.149 - page106
#'
#' @details
#' No details.
eq3.149.Y_R <- function(){
  PHI <- eq3.145.PHI()
  alpha_R <- eq3.148.alpha_R()
  Y_R <- PHI %*% alpha_R
  return(Y_R)
}


















##################################################################################
#UTIL
##################################################################################

  
#' @title Dummy for iterating a function that takes its previous result as a parameter with a single initial parameter
#'
#' @description
#' No description.
#'
#' @details
#' No details.
#' 
#' @param f function to iterate. It must be a function with at least a numeric input parameter and one numeric result (it uses the result of the last iteration as input for the next one)
#' @param alpha_0 Initial value
#' @param iterations Number of iterations
#' @param ... Additional parameters for f
iterate.generic <- function(f, alpha_0, iterations, ...){
  res <- vector(mode = "numeric", length = iterations)
  for(i in 1:iterations){
    if(i == 1){
      res[i] <- alpha_0
    }else{
      res[i] <- f(alpha_t.minus.1 = res[i - 1], ...)
    }
  }
  return (res)
}



#' @title Dummy for iterating a function that takes its previous result as a parameter with a single initial parameter
#'
#' @description
#' No description.
#'
#' @details
#' No details.
#' 
#' @param alpha_0 alpha_0
#' @param beta_0 beta_0
#' @param theta_1 theta_1
#' @param theta_2 theta_2
#' @param iterations iterations
iterate.chaos <- function(alpha_0, beta_0, theta_1, theta_2, iterations){
  alpha <- vector(mode = "numeric", length = iterations)
  beta <- vector(mode = "numeric", length = iterations)
  for(i in 1:iterations){
    if(i == 1){
      alpha[i] <- alpha_0
      beta[i] <- beta_0
    }else{
      beta[i] <- eq3.053.beta_t(alpha_t.minus.1 = alpha[i - 1], theta_2)
      alpha[i] <- eq3.052.alpha_t(alpha_t.minus.1 = alpha[i - 1], beta_t.minus.1 = beta[i - 1], theta_1 = theta_1, theta_2 = theta_2)
    }
  }
  return (cbind(alpha, beta))
}


#' @title Dummy for iterating equation 3.77 on page 86 to get figure 3.14
#'
#' @description
#' No description.
#'
#' @details
#' No details.
#' 
#' @param n n
#' @param alpha alpha
#' @param W_t.vector W_t.vector
iterate.ar1 <- function(n, alpha, W_t.vector){
  res <- vector(mode = "numeric", length = n)
  if(length(W_t.vector) == n){
    for(i in 1:n){
      if(i > 1){
        Y_t.minus.1 = res[i - 1]
      }else{
        Y_t.minus.1 = alpha
      }
      res[i] <- eq3.077.Y_t(alpha_1 = alpha, Y_t.minus.1 = Y_t.minus.1, W_t.vector[i])  
    }
  }
  return(res)
}


#' @title Dummy for iterating equation 3.82 on page 90 to get figure 3.15
#'
#' @description
#' No description.
#'
#' @details
#' No details.
#' 
#' @param n n
#' @param alpha_1 alpha_1
#' @param alpha_2 alpha_2
#' @param W_t.vector W_t.vector
iterate.ar2 <- function(n, alpha_1, alpha_2, W_t.vector){
  res <- vector(mode = "numeric", length = n)
  if(length(W_t.vector) == n){
    for(i in 2:n){
      if(i > 2){
        Y_t.minus.1 = res[i - 1]
        Y_t.minus.2 = res[i - 2]
      }else{
        Y_t.minus.1 = alpha_1
        Y_t.minus.2 = alpha_2
      }
      res[i] <- eq3.082.Y_t(alpha_1 = alpha_1, alpha_2 = alpha_2, Y_t.minus.1 = Y_t.minus.1, Y_t.minus.2 = Y_t.minus.2, W_t = W_t.vector[i])
    }
  }
  return(res)
}


#' @title Dummy for iterating equation 3.90 on page 91 to get figure 3.17
#'
#' @description
#' No description.
#'
#' @details
#' No details.
#' 
#' @param n n
#' @param beta beta
#' @param W_t.vector W_t.vector
iterate.map1 <- function(n, beta, W_t.vector){
  res <- vector(mode = "numeric", length = n)
  if(length(W_t.vector) == n){
    res[1] <- W_t.vector[1]
    for(i in 2:n){
      res[i] <- eq3.090.Y_t(W_t = W_t.vector[i], beta_1 = beta, W_tminus1 = W_t.vector[i - 1])
    }
  }
  return(res)
}