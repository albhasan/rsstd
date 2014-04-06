#----------------------------------------------------------
#3.2.3 BIFURCATION
#Figure 3.5
#pg 73
#----------------------------------------------------------
iterations <- 51
theta.vector <- seq(from = 0, to = 3, length.out = iterations)
res <- sapply(theta.vector, eq3.050.equilibrium, simplify = TRUE)

plot(x = theta.vector, y = res[2,], type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.5", lty = 1, lwd = 2.5, xaxs = "i", ylim = c(-1.5, 1))
lines(x = theta.vector,y = res[1,], type = "l", col = "blue", pch = 22, lty = 2)
legend(x = 0.1, y = 1, c("a = (theta - 1) / theta", "a = 0"), cex=0.8, col=c("blue","blue"),  lty=1:2);
