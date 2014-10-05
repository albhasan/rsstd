#----------------------------------------------------------
#3.2.3 BIFURCATION
#Figure 3.5
#pg 73
#----------------------------------------------------------
iterations <- 51
theta.vector <- seq(from = 0, to = 3.5, length.out = iterations)

a <- sapply(theta.vector, eq3.050.equilibrium, simplify = TRUE)
plot(x = theta.vector, y = a[2,], type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.5", lty = 1, lwd = 2.5, xaxs = "i", xlim = c(0, 3), ylim = c(-1.5, 1))
lines(x = theta.vector,y = a[1,], type = "l", col = "blue", pch = 22, lty = 2)
legend(x = 0.1, y = 1, c("a = (theta - 1) / theta", "a = 0"), cex=0.8, col=c("blue","blue"),  lty=1:2);
#----------------------------------------------------------
#3.2.3 BIFURCATION
#Figure 3.6
#pg 74
#----------------------------------------------------------
a1 <- sapply(theta.vector, eq3.051.equilibrium, simplify = TRUE)
plot(x = theta.vector, y = a[2,], type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.6", lty = 1, lwd = 2.5, xaxs = "i", xlim = c(0, 3.5), ylim = c(-1.5, 1.5))
lines(x = theta.vector,y = a[1,], type = "l", col = "blue", pch = 22, lty = 2)
lines(x = theta.vector,y = a1[1,], type = "l", col = "green", pch = 22, lty = 1, lwd = 2.5)
lines(x = theta.vector,y = a1[2,], type = "l", col = "green", pch = 22, lty = 1, lwd = 2.5)
#----------------------------------------------------------
#3.2.3 BIFURCATION
#Figure 3.7
#pg 75
#----------------------------------------------------------
iterations <- 101
theta <- 4
alpha_0 <- 0.4

alpha_t.vector <- iterate.generic(f = eq3.050.alpha_t, alpha_0 = alpha_0, iterations = iterations, theta = theta)
plot(x = seq(from = 0, by = 1, length.out = iterations), y = alpha_t.vector, type = "l", col = "blue", xlab = "Iteration ('time')", ylab = "", main = "Figure 3.7", lty = 1, lwd = 2.5, xaxs = "i")

