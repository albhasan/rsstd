#----------------------------------------------------------
#3.2.1 LINEAR DISCRETE DYNAMICAL SYSTEMS
#Spectral representation of the first-order system
#pg 70
#----------------------------------------------------------
M <- matrix(c(1.1, 0.2, -0.2, 0.3, 1.0, 0.4, 0.2, -0.1, 0.9), ncol=3)
lamda_i <- eigen(M,FALSE,TRUE)$values
abs_lamda_1 <- abs(lamda_i)
phi_over_pi <- Arg(lamda_i)/pi
Table3.1 <- cbind(lamda_i,abs_lamda_1,phi_over_pi)
#Table 3.1 Eigenvalue Spectrum of M
print(Table3.1)


delta <- 0.1# Not sure if this is the value. What about 2*pi/PHI
S_1 <- sensibilityMatrix(M, delta)
S_1.res = matrix(c(0.56, 0.69, 0.11, 0.35, 0.42, 0.07, 0.06, 0.08, 0.01), nrow = 3)
expect_that(S_1, equals(S_1.res))
print("The sensibility matrix still needs work")
