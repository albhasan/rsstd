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
subset <- (1:length(rawinsondesChuuk)) %% 30 == 0
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
tapperRawinsondesChuuk <- rawinsondesChuuk * w_t.vector
P <- 50
k.vector <- seq(from = 0, to = T/2, by = 1)
#fcircumflex_of_omega_k <- unlist(mclapply(k.vector, eq3.154.fcircumflex_of_omega_k, P = P, Y = tapperRawinsondesChuuk))


tapperRawinsondesChuuk.ar24 <- ar(tapperRawinsondesChuuk, order.max = 24)

plot(x = fcircumflex_of_omega_k, type = "l", col = "red", xlab = "Time", ylab = "", main = "Figure 3.19", lty = 1, lwd = 2.5, xaxs = "i",  pch = 21)


ar.ts <- function(w.vector, ts.vector){
  p <- length(w.vector)
  p.vector <- seq(from = p, to = length(ts.vector), by = 1)
  
}



#initial_k <- 0
#omega_k.vector <- eq3.130.omega_k(T, initial_k)
#contruit funciones con estas
#eq3.131.phi_k_of_t
#eq3.132.phi_0_of_t
#eq3.133.phi_halfT_of_t
#yluego llamara 3.151
