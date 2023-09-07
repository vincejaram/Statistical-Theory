#Statistical Theory FA3 - Jaramilla
#declaration of data
scores <- c(88, 45, 53, 86, 33, 86, 85, 30, 89, 53, 41, 96, 56, 38, 62, 71, 51, 86, 68, 29, 28, 47, 33, 37, 25, 36, 33, 94, 73, 46, 42, 34, 79, 72, 88, 99, 82, 62, 57, 42, 28, 55, 67, 62, 60, 96, 61, 57, 75, 93, 34, 75, 53, 32, 28, 73, 51, 69, 91, 35)

#computation of values
q1 <- quantile(scores,0.25)
q1
q2 <- quantile(scores,0.5)
q2
q3 <- quantile(scores,0.75)
q3
d9 <- quantile(scores,0.9)
d9
p95 <- quantile(scores,0.95)
p95

#printing of results
cat("Q1 =", q1, "\n")
cat("Q2 =", q2, "\n")
cat("Q3 =", q3, "\n")
cat("D9 =", d9, "\n")
cat("P95 =", p95, "\n")