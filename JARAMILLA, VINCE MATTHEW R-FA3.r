#Statistical Theory FA3 - Jaramilla
#declaration of data
library('moments')
library("psych")
scores <- c(88, 45, 53, 86, 33, 86, 85, 30, 89, 53, 41, 96, 56, 38, 62, 71, 51, 86, 68, 29, 28, 47, 33, 37, 25, 36, 33, 94, 73, 46, 42, 34, 79, 72, 88, 99, 82, 62, 57, 42, 28, 55, 67, 62, 60, 96, 61, 57, 75, 93, 34, 75, 53, 32, 28, 73, 51, 69, 91, 35)
n <- length(scores)
n

#computation of values
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode_scores <- getmode(scores)
mode_scores
mean_scores <- mean(scores)
mean_scores
median_scores <- median(scores)
median_scores
sd_scores <- sd(scores)
sd_scores
var_scores <- var(scores)
var_scores
skew_scores <- skewness(scores)
skew_scores
ses_scores <- sqrt((6*(n-1))/((n-2)*(n+1)*(n+3)))
ses_scores
kurt_scores <- kurtosis(scores)
kurt_scores
sek_scores <- sqrt((24*n*(n-2)*(n-3))/((n+3)*(n+5)*(n-1)*(n-1)))
sek_scores
min_scores <- min(scores)
min_scores
max_scores <- max(scores)
max_scores
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
#cat("Q1 =", q1, "\n")
#cat("Q2 =", q2, "\n")
#cat("Q3 =", q3, "\n")
#cat("D9 =", d9, "\n")
#cat("P95 =", p95, "\n")

df <- data.frame(values=c('Mode','Median','Mean','Std. Deviation','Variance','Skewness','Std. Error of Skewness','Kurtosis','Std. Error of Kurtosis','Minimum','Maximum','25th percentile','50th percentile','75th percentile','90th percentile','95th percentile'), results=c(mode_scores,median_scores,mean_scores,sd_scores,var_scores,skew_scores,ses_scores,kurt_scores,sek_scores,min_scores,max_scores,q1,q2,q3,d9,p95))
df