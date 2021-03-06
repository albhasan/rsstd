#----------------------------------------------------------
#3.5. SPECTRAL REPRESENTATION OF TEMPORAL PROCESSES
#---------------------------------------------------------

#----------------------------------------------------------
#3.5.2 DISCRETE-TIME SPECTRAL EXPANSION
#Figure 3.19
#pg 105
#----------------------------------------------------------
Y <- eq3.138.Y()
Y_R <- eq3.149.Y_R()

plot(x = Y, type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.19", lty = 1, lwd = 2.5, xaxs = "i", ylim = c(-1, 1), pch = 21)
lines(x = Y_R, type = "l", col = "green", pch = 22, lty = 2, lwd = 2.5)
legend(4, 1, c("Original Series", "Filtered Series"), cex=0.8, col=c("blue","green"), pch=21:22, lty=1:2);


#----------------------------------------------------------
#3.5.3 UNIVARIATE SPECTRAL ANALYSIS
#Figure 3.20
#pg 110
# 11323 is the number of days from 1964.01.01 to 1994.12.31
#TODO: It seems like we have the wrong data
#----------------------------------------------------------
data(list = c("T7UV"), package = c("rsstd"))
rawinsondesChuukNS <- as.vector(unlist(T7UV[2]))

rawinsondesChuukNS <- subsettimeseries(timeseries = rawinsondesChuukNS, takeeach = 32)

T <- length(rawinsondesChuukNS)
# Apply the tapering ot each end of the time series
tapperSide <- 0.1
tapperrawinsondesChuukNS <- cosineballtapper(timeseries = rawinsondesChuukNS, tapperSide = tapperSide)
# Periodogram calculation using eq3.154
P <- 50
k.vector <- seq(from = 0, to = T/2, by = 1)
fcircumflex_of_omega_k <- unlist(lapply(k.vector, eq3.154.fcircumflex_of_omega_k, P = P, Y = tapperrawinsondesChuukNS))
# Periodogram calculation using AR(24)
tapperrawinsondesChuukNS.ar24 <- ar(tapperrawinsondesChuukNS, aic = FALSE, order.max = 24)
tapperrawinsondesChuukNS.ar24.ts <- ar.ts(w.vector = tapperrawinsondesChuukNS.ar24[[2]], ts.vector = tapperrawinsondesChuukNS)
fcircumflex_of_omega_k.ar24 <- unlist(mclapply(k.vector, eq3.154.fcircumflex_of_omega_k, P = P, Y = tapperrawinsondesChuukNS.ar24.ts))
# Plot
step <- 0.5/(length(fcircumflex_of_omega_k) - 1)
x.vec <- seq(from = 0, to = 0.5, by = step)
plot(x = cbind(x.vec, fcircumflex_of_omega_k), type = "l", col = "red", xlab = "Frequency (cycle/day)", ylab = "Power Spectral Density: (ms-1)2 day", main = "Figure 3.20", lty = 1, lwd = 2.5, xaxs = "i",  pch = 21)
lines(x = cbind(x.vec, fcircumflex_of_omega_k.ar24), type = "l", col = "green", pch = 22, lty = 2, lwd = 2.5)
legend(0.28, 10, c("Smoothed Periodogram", "AR Spectrum"), cex=0.8, col=c("red","green"), pch=21:22, lty=1:2);

# Validation
library(TSA)
par(mfrow = c(2, 1))
plot(x = fcircumflex_of_omega_k, type = "l", col = "red", xlab = "Frequency (cycle/day)", ylab = "Power Spectral Density: (ms-1)2 day", main = "Figure 3.20", lty = 1, lwd = 2.5, xaxs = "i",  pch = 21)
periodogram(tapperrawinsondesChuukNS,ylab='tapperrawinsondesChuukNS.ar24.ts');  abline(h=0)
plot(x = fcircumflex_of_omega_k.ar24, type = "l", col = "green", pch = 22, lty = 2, lwd = 2.5)
periodogram(tapperrawinsondesChuukNS.ar24.ts,ylab='tapperrawinsondesChuukNS.ar24.ts');  abline(h=0)
par(mfrow = c(1, 1))

#----------------------------------------------------------
#3.5.3 UNIVARIATE SPECTRAL ANALYSIS
#Figure 3.21
#pg 112
#TODO: It seems like we have the wrong data
#----------------------------------------------------------
data(list = c("T7UV"), package = c("rsstd"))
rawinsondesChuukEW <- as.vector(unlist(T7UV[1]))

#rawinsondesChuukEW <- subsettimeseries(timeseries = rawinsondesChuukEW, takeeach = 32)

T <- length(rawinsondesChuukEW)
# Apply the tapering ot each end of the time series
tapperSide <- 0.1
tapperrawinsondesChuukEW<- cosineballtapper(timeseries = rawinsondesChuukEW, tapperSide = tapperSide)
# Periodogram calculation using eq3.154
P <- 50
k.vector <- seq(from = 0, to = T/2, by = 1)
fcircumflex_of_omega_k <- unlist(mclapply(k.vector, eq3.154.fcircumflex_of_omega_k, P = P, Y = tapperrawinsondesChuukEW))
# Plot
step <- 0.5/(length(fcircumflex_of_omega_k) - 1)
x.vec <- seq(from = 0, to = 0.5, by = step)
plot(x = cbind(x.vec, fcircumflex_of_omega_k), type = "l", col = "red", xlab = "Frequency (cycle/day)", ylab = "Power Spectral Density: (ms-1)2 day", main = "Figure 3.21", lty = 1, lwd = 2.5, xaxs = "i",  pch = 21)



