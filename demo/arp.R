#----------------------------------------------------------
#3.4.3 AUTOREGRESSIVE PROCESS
#Figure 3.13
#pg 88
#----------------------------------------------------------
frequency <- seq(from = -0.49, to = 0.49, by = 0.01)
var_w <- 1
alpha1 <- 0.8
alpha2 <- -0.8

spectralDensity1 <- sapply(X = frequency, eq3.081.f_of_omega, var_w = var_w, alpha = alpha1)
spectralDensity2 <- sapply(X = frequency, eq3.081.f_of_omega, var_w = var_w, alpha = alpha2)

plot(x = frequency, y = spectralDensity1, type = "l", col = "red", xlab = "Frequency", ylab = "", main = "Figure 3.13a", lty = 1, lwd = 2.5, xaxs = "i")
plot(x = frequency, y = spectralDensity2, type = "l", col = "red", xlab = "Frequency", ylab = "", main = "Figure 3.13b", lty = 1, lwd = 2.5, xaxs = "i")

#----------------------------------------------------------
#3.4.3 AUTOREGRESSIVE PROCESS
#Figure 3.14
#pg 89
#----------------------------------------------------------
n <- 100
time.vector <- seq(from = 1, to = n, by = 1)
alpha3 <- 1.01
W_t.vector = rnorm(n = n, mean = 0, sd = 1)

Y_t.vector1 <- iterate.ar1(n = n, alpha = alpha1, W_t.vector = W_t.vector)
Y_t.vector2 <- iterate.ar1(n = n, alpha = alpha2, W_t.vector = W_t.vector)
Y_t.vector3 <- iterate.ar1(n = n, alpha = alpha3, W_t.vector = W_t.vector)

plot(x = time.vector, y = Y_t.vector1, type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.14a", lty = 1, lwd = 2.5, xaxs = "i")
plot(x = time.vector, y = Y_t.vector2, type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.14b", lty = 1, lwd = 2.5, xaxs = "i")
plot(x = time.vector, y = Y_t.vector3, type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.14c", lty = 1, lwd = 2.5, xaxs = "i")
#eq3.077.Y_t(alpha_1, Y_t.minus.1, W_t)


