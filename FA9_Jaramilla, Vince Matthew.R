#Formative Assessment 9 (Exercise 18.1)
#1
mean_1 <- 200
var_1 <- 256
sd_1 <- sqrt(var_1)
prob_1_a <- 1-pnorm(224,mean_1,sd_1)
prob_1_a
prob_1_b <- pnorm(224,mean_1,sd_1) - pnorm(186,mean_1,sd_1)
prob_1_b
quantile_1_c <- qnorm(0.25,mean_1,sd_1)
quantile_1_c
prob_1_d <- (pnorm(240,mean_1,sd_1) - pnorm(210,mean_1,sd_1)) / (1-pnorm(210,mean_1,sd_1))
prob_1_d
qrange_1_e <- qnorm(0.75,mean_1,sd_1) - qnorm(0.25,mean_1,sd_1)
qrange_1_e
prob_1_f <- (pnorm(220,mean_1,sd_1) - pnorm(210,mean_1,sd_1)) / (1-pnorm(210,mean_1,sd_1))
prob_1_f
prob_1_g <- (1-pnorm(220,mean_1,sd_1)) / (1-pnorm(200,mean_1,sd_1))
prob_1_g

cat("1. a) Probability that the signal will exceed 224 𝜇V =", prob_1_a*100, "%")
cat("\n   b) Probability that it will be between 186 and 224 𝜇V =", prob_1_b*100, "%")
cat("\n   c) Micro voltage below which 25% of the signals will be =", quantile_1_c, "𝜇V")
cat("\n   d) Probability that the signal will be less than 240 𝜇V, given that it is larger than 210 𝜇V =", prob_1_d*100, "%")
cat("\n   e) Interquartile range =", qrange_1_e, "𝜇V")
cat("\n   f) Probability that the signal will be less than 220 𝜇V, given that it is larger than 210 𝜇V =", prob_1_f*100, "%")
cat("\n   g) Probability that the signal is greater than 220 𝜇V, given that it is greater than 200 𝜇V =", prob_1_g*100, "%")


#2
mean_2 <- 25
var_2 <- 144
sd_2 <- sqrt(var_2)
bound1_2_a <- qnorm(0.025,mean_2,sd_2)
bound1_2_a
bound2_2_a <- qnorm(0.975,mean_2,sd_2)
bound2_2_a
bound_2_b <- qnorm(0.9,mean_2,sd_2)
bound_2_b

cat("2. a) Bounds which will include 95% of the downtime of all the customers =", bound1_2_a, " and", bound2_2_a)
cat("\n   b) Bound above which 10% of the downtime is included =", bound_2_b)