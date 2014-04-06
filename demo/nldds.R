#----------------------------------------------------------
#3.2.2 NONLINEAR DISCRETE DYNAMICAL SYSTEMS
#Figure 3.4
#pg 72
#----------------------------------------------------------
alpha_0 <- 4
iterations <- 51

theta_0 <- 10
theta_1 <- 0.25
alpha_t.vector <- iterate.generic(f = eq3.049.alpha_t, alpha_0 = alpha_0, theta_0 = theta_0, theta_1 = theta_1, iterations = iterations)
plot(x = 0:(iterations-1), y = alpha_t.vector, type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.4 a", lty = 1, lwd = 2.5, xaxs = "i")

theta_0 <- 10
theta_1 <- -0.25
alpha_t.vector <- iterate.generic(f = eq3.049.alpha_t, alpha_0 = alpha_0, theta_0 = theta_0, theta_1 = theta_1, iterations = iterations)
plot(x = 0:(iterations-1), y = alpha_t.vector, type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.4 b", lty = 1, lwd = 2.5, xaxs = "i")
