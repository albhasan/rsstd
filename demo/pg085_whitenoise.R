#----------------------------------------------------------
#3.4.1 WHITE-NOISE PROCESS
#Figure 3.11
#pg 85
#----------------------------------------------------------
n <- 101
mean <- 0
sd <- 2
realization1 <- whiteNoise.gaussian(n = n, mean = mean, sd = sd)
realization2 <- whiteNoise.gaussian(n = n, mean = mean, sd = sd)

plot(x = realization1, type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.11a", lty = 1, lwd = 2.5, xaxs = "i")
plot(x = realization2, type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.11b", lty = 1, lwd = 2.5, xaxs = "i")
