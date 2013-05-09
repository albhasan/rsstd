#----------------------------------------------------------
#3.2.1 LINEAR DISCRETE DYNAMICAL SYSTEMS
#Second-order univariate systems
#pg 65
#----------------------------------------------------------
alpha_0 <- 0
alpha_1 <- -5
theta_1 <- 0
theta_2 <- -0.64
iterations <- 21

#Non-analytic solution
suosresult <- sous.noAnalytic(f = eq3.030.alpha_t, alpha_0 = alpha_0, alpha_1 = alpha_1, iterations = iterations)
plot(x = 0:(iterations-1), y = suosresult, type="l", col="blue", xlab="Time", ylab="",main="Figure 3.2 (Non-analytic solution)", lty=1, lwd=2.5, xaxs="i")

#Analytic solution
alpha_t.vector <- sous.analytic(alpha_0,alpha_1,theta_1,theta_2,iterations)
#The solution is not the same as presented in the book
plot(x = 0:(iterations-1), y = alpha_t.vector, type = "l", col = "blue", xlab = "Time", ylab = "", main = "Figure 3.2 (Analytic solution)", lty = 1, lwd = 2.5, xaxs = "i")

#Difference among the analytical and non-analytical calculations
diff <- alpha_t.vector - suosresult
plot(x = 0:(iterations-1), y = diff, type = "l", col = "blue", xlab = "Time", ylab = "Analytic - Non analytic", main = "Solution differences (?)", lty = 1, lwd = 2.5, xaxs = "i")