#----------------------------------------------------------
#3.2.1 LINEAR DISCRETE DYNAMICAL SYSTEMS
#Fisrt-order univariate systems
#pg 60
#----------------------------------------------------------
iterations <- 40
alpha_0.fixedPoint <- eq3.011.equilibrium(theta_0 = -30, theta_1 = 1.05)
alpha_0.lessThanFixedPoint <- alpha_0.fixedPoint - 50
alpha_0.moreThanFixedPoint <- alpha_0.fixedPoint + 50
alpha_t.fixed <- iterate.f1p(f = eq3.012.alpha_t, alpha_0 = alpha_0.fixedPoint, iterations = iterations)

alpha_t.decrease <- iterate.f1p(f = eq3.012.alpha_t, alpha_0 = alpha_0.lessThanFixedPoint, iterations = iterations)
alpha_t.increase <- iterate.f1p(f = eq3.012.alpha_t, alpha_0 = alpha_0.moreThanFixedPoint, iterations = iterations)

plot(0:(iterations-1), alpha_t.fixed, type = "l", col = "blue", lty = 1, lwd = 2.5, xaxs = "i", xlab = "Iterations", ylab = "Values", main = "(Not in the book)")
lines(0:(iterations-1), alpha_t.decrease, col = "green", lty = 2, lwd = 2.5)
lines(0:(iterations-1), alpha_t.increase, col = "red", lty = 2, lwd = 2.5)
legend(0, 850, c("alpha_0 = fixed point","alpha_0 less than fixed point", "alpha_0 greater than fixed point"), lty = c(1,2,2), lwd = c(2.5,2.5,2.5), col = c("blue", "green", "red"))



