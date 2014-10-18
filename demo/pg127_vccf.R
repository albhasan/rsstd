#----------------------------------------------------------
#4.1. GEOSTATISTICAL PROCESSES
#---------------------------------------------------------

#----------------------------------------------------------
#4.1.1 The variogram and the covariance function
#Figure 4.2
#pg 127
#NOTE: In the book the y-axis has the wrong label
#----------------------------------------------------------
theta_1 <- theta[1]
theta_2 <- theta[2]
variance_0 <- theta[3]
variance_1 <- theta[4]
h.vector <- seq(from = 0.1, to = 10, by = 0.1)


C_Y_of_0_and_theta <- variance_0 + variance_1
C_Y_of_h_and_theta <- sapply(h.vector, gamma_Y_of_h, theta_1 = theta_1, theta_2 = theta_2, variance_0 = variance_0, variance_1 = variance_1)
gamma_Y_of_h_and_theta <- C_Y_of_0_and_theta - C_Y_of_h_and_theta

plot(x = h.vector, y = gamma_Y_of_h_and_theta, type = "l", col = "blue", xlab = "h", ylab = "gamma_Y(h;theta)", main = "Figure 4.2", lty = 1, lwd = 2.5, xaxs = "i", xlim = c(0, 10), ylim = c(0, 1), pch = 21)
points(x = 0, y = C_Y_of_0_and_theta, col = "blue", pch = 16)

