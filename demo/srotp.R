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
#----------------------------------------------------------
data(list = c("T7UV"), package = c("rsstd"))
rawinsondesChuuk <- as.vector(unlist(T7UV[1]))
######################################################################
subset <- (1:length(rawinsondesChuuk)) %% 7 == 0
rawinsondesChuuk <- subset(rawinsondesChuuk, subset = subset)
######################################################################
T <- length(rawinsondesChuuk)
w_t.vector <- vector(mode = "numeric", length = T)
tapperSide <- 0.1
for(t in 1:T){
  if(t < (T * tapperSide) || t > T - (T * tapperSide)){
    w_t.vector[t] <- eq3.153.w_t(T, t)  
  }else{
    w_t.vector[t] <- 1
  }
}


.ar.ts <- function(w.vector, ts.vector){
  p <- length(w.vector)
  t.vector <- seq(from = p + 1, to = length(ts.vector), by = 1)
  unlist(lapply(t.vector, .dummy.ar.ts, p = p, w.vector = w.vector, ts.vector = ts.vector))
}
.dummy.ar.ts <- function(t, p, w.vector, ts.vector){
  sum(ts.vector[(t - p):(t - 1)] * w.vector, na.rm = FALSE)
}

tapperRawinsondesChuuk <- rawinsondesChuuk * w_t.vector
P <- 50
k.vector <- seq(from = 0, to = T/2, by = 1)
fcircumflex_of_omega_k <- unlist(mclapply(k.vector, eq3.154.fcircumflex_of_omega_k, P = P, Y = tapperRawinsondesChuuk))

tapperRawinsondesChuuk.ar24 <- ar(tapperRawinsondesChuuk, aic = FALSE, order.max = 24)
tapperRawinsondesChuuk.ar24.ts <- .ar.ts(w.vector = tapperRawinsondesChuuk.ar24[[2]], ts.vector = tapperRawinsondesChuuk)
fcircumflex_of_omega_k.ar24 <- unlist(mclapply(k.vector, eq3.154.fcircumflex_of_omega_k, P = P, Y = tapperRawinsondesChuuk.ar24.ts))


plot(x = fcircumflex_of_omega_k, type = "l", col = "red", xlab = "Frequency (cycle/day)", ylab = "Power Spectral Density: (ms-1)2 day", main = "Figure 3.20", lty = 1, lwd = 2.5, xaxs = "i",  pch = 21)
lines(x = fcircumflex_of_omega_k.ar24, type = "l", col = "green", pch = 22, lty = 2, lwd = 2.5)
legend(100, 1500, c("Smoothed Periodogram", "AR Spectrum"), cex=0.8, col=c("red","green"), pch=21:22, lty=1:2);

# Validation
#library(TSA)
#par(mfrow = c(2, 1))
#plot(x = fcircumflex_of_omega_k, type = "l", col = "red", xlab = "Frequency (cycle/day)", ylab = "Power Spectral Density: (ms-1)2 day", main = "Figure 3.20", lty = 1, lwd = 2.5, xaxs = "i",  pch = 21)
#periodogram(tapperRawinsondesChuuk,ylab='tapperRawinsondesChuuk.ar24.ts');  abline(h=0)

#plot(x = fcircumflex_of_omega_k.ar24, type = "l", col = "green", pch = 22, lty = 2, lwd = 2.5)
#periodogram(tapperRawinsondesChuuk.ar24.ts,ylab='tapperRawinsondesChuuk.ar24.ts');  abline(h=0)
#par(mfrow = c(1, 1))
