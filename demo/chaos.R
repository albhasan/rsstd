#----------------------------------------------------------
#3.2.4 CHAOS
#Figure 3.8
#pg 76
#----------------------------------------------------------
iterations <- 51
theta <- 4

alpha_0.1 <- 0.51
alpha_0.2 <- 0.510001
alpha_t.vector.1 <- iterate.generic(f = eq3.050.alpha_t, alpha_0 = alpha_0.1, iterations = iterations, theta = theta)
alpha_t.vector.2 <- iterate.generic(f = eq3.050.alpha_t, alpha_0 = alpha_0.2, iterations = iterations, theta = theta)

x = seq(from = 0, by = 1, length.out = iterations)
plot(x = x, y = alpha_t.vector.1, type = "l", col = "blue", xlab = "Iteration ('time')", ylab = "", main = "Figure 3.8", lty = 1, lwd = 2.5, xaxs = "i")
lines(x = x, y = alpha_t.vector.2, type = "l", col = "red", lty = 2, lwd = 2.5)
#----------------------------------------------------------
#3.2.4 CHAOS
#Figure 3.9
#pg 77
#----------------------------------------------------------
iterations <- 501
theta_1 <- 1.4
theta_2 <- 0.3
alpha_0 <- 0.5
beta_0 <- 0.4

res <- iterate.chaos(alpha_0, beta_0, theta_1, theta_2, iterations)

x = seq(from = 0, by = 1, length.out = iterations)
plot(x = x, y = res[,1], type = "l", col = "blue", xlab = "Iteration ('time')", ylab = "", main = "Figure 3.9a", lty = 1, lwd = 2.0, xaxs = "i")
plot(x = x, y = res[,2], type = "l", col = "red", xlab = "Iteration ('time')", ylab = "", main = "Figure 3.9b", lty = 1, lwd = 2.0, xaxs = "i")

#----------------------------------------------------------
#3.2.4 CHAOS
#Figure 3.10
#pg 78
#----------------------------------------------------------
plot(x = res[,1], y = res[,2], type = "p", col = "blue", xlab = "alpha_t", ylab = "beta_t", main = "Figure 3.10")
