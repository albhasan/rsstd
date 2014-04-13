#----------------------------------------------------------
#3.4.2 RANDOM-WALK PROCESS
#Figure 3.12
#pg 86
#----------------------------------------------------------
n <- 1001
mean <- 0
sd <- 2

realization1 <- whiteNoise.gaussian(n = n, mean = mean, sd = sd)
realization2 <- whiteNoise.gaussian(n = n, mean = mean, sd = sd)
plot(x = realization1, type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.11a", lty = 1, lwd = 2.5, xaxs = "i")
plot(x = realization2, type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.11b", lty = 1, lwd = 2.5, xaxs = "i")
rw1 <- randomWalk(realization1)
rw2 <- randomWalk(realization2)
plot(x = rw1, type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.12a", lty = 1, lwd = 2.5, xaxs = "i")
plot(x = rw2, type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.12b", lty = 1, lwd = 2.5, xaxs = "i")
#rw1 <- randomWalk.gaussian(n = n, mean = mean, sd = sd)
#rw2 <- randomWalk.gaussian(n = n, mean = mean, sd = sd)

