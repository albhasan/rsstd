#----------------------------------------------------------
#3.4.4 MOVING AVERAGE PROCESS
#Figure 3.16
#pg 92
#----------------------------------------------------------
frequency <- seq(from = -0.49, to = 0.49, by = 0.01)
var_w <- 1
beta1 <- -0.7
beta2 <- 0.7

spectralDensity1 <- sapply(X = frequency, eq3.092.f_of_omega, var_w = var_w, beta = beta1)
spectralDensity2 <- sapply(X = frequency, eq3.092.f_of_omega, var_w = var_w, beta = beta2)

plot(x = frequency, y = spectralDensity1, type = "l", col = "red", xlab = "Frequency", ylab = "", main = "Figure 3.16a", lty = 1, lwd = 2.5, xaxs = "i")
plot(x = frequency, y = spectralDensity2, type = "l", col = "red", xlab = "Frequency", ylab = "", main = "Figure 3.16b", lty = 1, lwd = 2.5, xaxs = "i")


#----------------------------------------------------------
#3.4.4 MOVING AVERAGE PROCESS
#Figure 3.17
#pg 93
#----------------------------------------------------------
n <- 100
var_w <- 1
time.vector <- seq(from = 1, to = n, by = 1)
W_t.vector = rnorm(n = n, mean = 0, sd = sqrt(var_w))
beta_1 <- -0.7
beta_2 <- 0.7
Y_t.vector1 <- iterate.map1(n = n, beta = beta_1, W_t.vector = W_t.vector)
Y_t.vector2 <- iterate.map1(n = n, beta = beta_2, W_t.vector = W_t.vector)

plot(x = time.vector, y = Y_t.vector1, type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.17a", lty = 1, lwd = 2.5, xaxs = "i")
plot(x = time.vector, y = Y_t.vector2, type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.17b", lty = 1, lwd = 2.5, xaxs = "i")

