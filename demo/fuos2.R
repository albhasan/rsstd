#----------------------------------------------------------
#3.2.1 LINEAR DISCRETE DYNAMICAL SYSTEMS
#Fisrt-order univariate systems
#pg 61
#Figure 3.1
#pg 62
#----------------------------------------------------------
alpha_t <- function(alpha_t.minus.1){
  eq3.011.alpha_t(theta_0 = 0, theta_1 = -.7, alpha_t.minus.1 = alpha_t.minus.1)
}

alpha_0 <- 12
t <- 11
alpha_0.fixedPoint <- eq3.011.equilibrium(theta_0 = 0, theta_1 = -0.7)
alpha_t.oscilatingAndAMplitudeDecreasing <- iterate.f1p(f = alpha_t, alpha_0 = alpha_0, iterations = t)
plot(x = 0:(t-1), y = alpha_t.oscilatingAndAMplitudeDecreasing, type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.1", lty = 1, lwd = 2.5, xaxs = "i")
