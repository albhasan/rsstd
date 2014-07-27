#----------------------------------------------------------
#3.4.4 MOVING AVERAGE PROCESS
#Figure 3.16
#pg 91
#----------------------------------------------------------
frequency <- seq(from = -0.49, to = 0.49, by = 0.01)
var_w <- 1
beta1 <- -0.7
beta2 <- 0.7

spectralDensity1 <- sapply(X = frequency, eq3.092.f_of_omega, var_w = var_w, beta = beta1)
spectralDensity2 <- sapply(X = frequency, eq3.092.f_of_omega, var_w = var_w, beta = beta2)

plot(x = frequency, y = spectralDensity1, type = "l", col = "red", xlab = "Frequency", ylab = "", main = "Figure 3.16b", lty = 1, lwd = 2.5, xaxs = "i")
plot(x = frequency, y = spectralDensity2, type = "l", col = "red", xlab = "Frequency", ylab = "", main = "Figure 3.16b", lty = 1, lwd = 2.5, xaxs = "i")
