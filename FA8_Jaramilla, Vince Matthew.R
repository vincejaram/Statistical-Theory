#Formative Assessment 8 (Exercise 16.1)
#1
lambda_1 <- 4
prob_lessthan_15s <- pexp(0.25,lambda_1) #1-exp(-lambda_1*0.25)
prob_lessthan_15s
prob_greaterthan_30s <- 1-pexp(0.5,lambda_1) #1-exp(-lambda_1*0.50)
prob_greaterthan_30s
prob_15s_to_60s <- pexp(1,lambda_1) - pexp(0.25,lambda_1) #(1-exp(-lambda_1*1)) - (1-exp(-lambda_1*0.25))
prob_15s_to_60s

cat("1. P(T<=0.25) =", prob_lessthan_15s)
cat("\n   P(T>0.5) =", prob_greaterthan_30s)
cat("\n   P(0.25<=T<1) =", prob_15s_to_60s)

#3
lambda_3 <- 2
prob_3_a <- 1-dpois(2,lambda_3)-dpois(1,lambda_3)-dpois(0,lambda_3)
prob_3_a
prob_3_b <- 1-pexp(0.5,lambda_3)
prob_3_b
prob_3_c <- pexp(0.5,lambda_3)
prob_3_c
prob_3_d <- 1-pexp(0.5,lambda_3)
prob_3_d

cat("3. a) P(X=2) =", prob_3_a)
cat("\n   b) P(T>0.5) =", prob_3_b)
cat("\n   c) P(T<0.5) =", prob_3_c)
cat("\n   d) P(T>0.5) if none in last 0.5 =", prob_3_d)

#7
lambda_7 <- 15
prob_7_a <- 1-pexp(1/6,lambda_7)
prob_7_a
prob_7_b <- sum(dpois(0:7,lambda_7))
prob_7_b
prob_7_c <- pexp(0.25,lambda_7)
prob_7_c
prob_7_d <- qpois(0.75,lambda_7)
prob_7_d

cat("7. a) P(T>1/6) =", prob_7_a)
cat("\n   b) P(X<8) =", prob_7_b)
cat("\n   c) P(T<0.25) given that  15 mins have elapsed since the last visit =", prob_7_c)
cat("\n   d) Top quantile =", prob_7_d, ", showing that the number of visits in an hour may be greater than 18 25% of the time.")