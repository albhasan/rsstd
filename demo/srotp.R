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
#----------------------------------------------------------

data(list = c("T7UV"), package = c("rsstd"))
rawinsondesChuuk <- T7UV[1]
T <- length(rawinsondesChuuk[[1]])
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
eq3.154.fcircumflex_of_omega_k(k, P)







