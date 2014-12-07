#----------------------------------------------------------
#4.1. GEOSTATISTICAL PROCESSES
#---------------------------------------------------------

#----------------------------------------------------------
#4.1.1 The variogram and the covariance function
#Figure 4.2
#pg 127
#----------------------------------------------------------
theta_1 <- 1
theta_2 <- 1.5
variance_0 <- 0.1
variance_1 <- 0.9
h.vector <- seq(from = 0.0, to = 10, by = 0.1)

#TODO: eq 4.6 is not working
gamma_Y_eq46 <- function(h, theta_1, theta_2, variance_0, variance_1){
  C_Y_of_h_and_theta <- -1
  if(h > 0){
    normh <- norm(as.matrix(h))
    C_Y_of_h_and_theta <- variance_1 * exp(-normh) * (1 + normh)
  }else{
    C_Y_of_h_and_theta <- variance_0 + variance_1
  }
  return(C_Y_of_h_and_theta)
}

gamma_Y <- sapply(h.vector, gamma_Y_eq46, theta_1 = theta_1, theta_2 = theta_2, variance_0 = variance_0, variance_1 = variance_1)
plot(x = h.vector[-1], y = gamma_Y[-1], type = "l", col = "blue", xlab = "h", ylab = "C_Y(h; theta)", main = "Figure 4.2", lty = 1, lwd = 2.5, xaxs = "i", ylim = c(0, 1), xlim = c(0, 10))
points(x = h.vector[1], y = gamma_Y[1], col = "blue", pch = 16)



#----------------------------------------------------------
#4.1.1 The variogram and the covariance function
#Figure 4.3
#pg 128
#----------------------------------------------------------
theta_1 <- 1
theta_2 <- 1
variance_0 <- 0.1
variance_1 <- 0.9
h.vector <- seq(from = 0.0, to = 10, by = 0.1)

#TODO: eq 4.7 is not working
gamma_Y_eq47 <- function(h, theta_1, theta_2, variance_0, variance_1){
  C_Y_of_h_and_theta <- -1
  if(h != 0){
    normh <- norm(as.matrix(h))
    C_Y_of_h_and_theta <- variance_1 * exp(-normh)
  }else{
    C_Y_of_h_and_theta <- variance_0 + variance_1
  }
  return(C_Y_of_h_and_theta)
}

gamma_Y <- sapply(h.vector, gamma_Y_eq47, theta_1 = theta_1, theta_2 = theta_2, variance_0 = variance_0, variance_1 = variance_1)
plot(x = h.vector[-1], y = gamma_Y[-1], type = "l", col = "blue", xlab = "h", ylab = "C_Y(h; theta)", main = "Figure 4.3", lty = 1, lwd = 2.5, xaxs = "i", ylim = c(0, 1), xlim = c(0, 10))
points(x = h.vector[1], y = gamma_Y[1], col = "blue", pch = 16)



#----------------------------------------------------------
#4.1.1 The variogram and the covariance function
#Figure 4.4
#pg 129
#----------------------------------------------------------
theta_1 <- 1
theta_2 <- 1.5
variance_0 <- 0.1
variance_1 <- 0.9
h.vector <- seq(from = 0.0, to = 10, by = 0.01)

#TODO: eq 4.8 is not working
gamma_Y_eq48_andHalf <- function(h, theta_1, theta_2, variance_0, variance_1){
  C_Y_of_h_and_theta <- -1
  normh <- norm(as.matrix(h))
  if(h != 0){
    C_Y_of_h_and_theta <- variance_0 + variance_1 - variance_1 * exp(-normh)
  }else{
    #C_Y_of_h_and_theta <- 0 + variance_1 - variance_1 * exp(-normh)
    C_Y_of_h_and_theta <- variance_0 * diag(x = h, nrow = length(h), ncol = length(h)) * (as.numeric(normh == 0)) + variance_1 - variance_1 * exp(-normh)
  }
  
  
  return(C_Y_of_h_and_theta)
}

gamma_Y <- sapply(h.vector, gamma_Y_eq48_andHalf, theta_1 = theta_1, theta_2 = theta_2, variance_0 = variance_0, variance_1 = variance_1)
plot(x = h.vector[-1], y = gamma_Y[-1], type = "l", col = "blue", xlab = "h", ylab = "gamma_Y(h; theta)", main = "Figure 4.3", lty = 1, lwd = 2.5, xaxs = "i", ylim = c(0, 1), xlim = c(0, 10))
points(x = h.vector[1], y = gamma_Y[1], col = "blue", pch = 16)
