#----------------------------------------------------------
#3.5. SPECTRAL REPRESENTATION OF TEMPORAL PROCESSES
#3.5.2 DISCRETE-TIME SPECTRAL EXPANSION
#Figure 3.19
#pg 105
#----------------------------------------------------------

Y <- eq3.138.Y()
Y_R <- eq3.149.Y_R()

plot(x = Y, type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.19", lty = 1, lwd = 2.5, xaxs = "i", ylim = c(-1, 1))
lines(x = Y_R, type = "l", col = "green", pch = 22, lty = 1, lwd = 2.5)
