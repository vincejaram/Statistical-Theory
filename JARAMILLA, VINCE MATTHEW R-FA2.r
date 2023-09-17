#Statistical Theory FA2 - Jaramilla

#3.49
#assigning values to X
X <- c(1, 4, 9, 16, 25)
#left side of the equation
left_349 <- sum((X-1)^2)
left_349
#right side of the equation
right_349 <- sum(X^2) - 2*(sum(X))+length(X)
right_349
#printing the results
cat("The left side =", left_349, "\n")
cat("The right side =", right_349)
cat("Since both sides of the equation result to the same value, the equation is proven to be true.")

#3.51
#assigning variables
U <- c(3, -2, 5)
V <- c(-4, -1, 6)
#a
a_351 <- sum(U*V)
a_351
#b
b_351 <- sum((U+3)*(V-4))
b_351
#c
c_351 <- sum(V^2)
c_351
#d
d_351 <- (sum(U))*(sum(V))^2
d_351
#e
e_351 <- sum(U*V^2)
e_351
#f
f_351 <- sum(U^2-(2*V^2)+2)
f_351
#g
g_351 <- sum(U/V)
g_351
#printing of results
cat("a) ΣUV =", a_351, "\n")
cat("b) Σ(U+3)(V-4) =", b_351, "\n")
cat("c) ΣV^2 =", c_351, "\n")
cat("d) (ΣU)(ΣV)^2 =", d_351, "\n")
cat("e) ΣUV^2 =", e_351, "\n")
cat("f) Σ(U^2 - 2V^2 + 2) =", f_351, "\n")
cat("g) Σ(U/V) =", g_351, "\n")

#3.90
#assigning of variables
A <- c(3, 5, 8, 3, 7, 2)
B <- c(28.5, 73.6, 47.2, 31.5, 64.8)
#a
A_geomean <- exp(mean(log(A)))
A_geomean
#b
B_geomean <- exp(mean(log(B)))
B_geomean
#printing of results
cat("The geometric mean of A =", A_geomean, "\n")
cat("The geometric mean of B =", B_geomean, "\n")