#Statistical Theory FA4 - Jaramilla
library("e1071")
#assigning values
normal <- c(67,70,63,65,68,60,70,64,69,61,66,65,71,62,66,68,64,67,62,66,65,63,66,65,63,69,62,67,59,66,65,63,65,60,67,64,68,61,69,65,62,67,70,64,63,68,64,65,61,66)
skewright <- c(31,43,30,30,38,26,29,55,46,26,29,57,34,34,36,40,28,26,66,63,30,33,24,35,34,40,24,29,24,27,35,33,75,38,34,85,29,40,41,35,26,34,19,23,28,26,31,25,22,28)
skewleft <- c(102,55,70,95,73,79,60,73,89,85,72,92,76,93,76,97,10,70,85,25,83,58,10,92,82,87,104,75,80,66,93,90,84,73,98,79,35,71,90,71,63,58,82,72,93,44,65,77,81,77)
uniform <- c(12.1,12.1,12.4,12.1,12.1,12.2,12.2,12.2,11.9,12.2,12.3,12.3,11.7,12.3,12.3,12.4,12.4,12.1,12.4,12.4,12.5,11.8,12.5,12.5,12.5,11.6,11.6,12.0,11.6,11.6,11.7,12.3,11.7,11.7,11.7,11.8,12.5,11.8,11.8,11.8,11.9,11.9,11.9,12.2,11.9,12.0,11.9,12.0,12.0,12.0)

#1
moments_norm <- c(mean(normal), var(normal), skewness(normal), kurtosis(normal))
moments_right <- c(mean(skewright), var(skewright), skewness(skewright), kurtosis(skewright))
moments_left <- c(mean(skewleft), var(skewleft), skewness(skewleft), kurtosis(skewleft))
moments_unif <- c(mean(uniform), var(uniform), skewness(uniform), kurtosis(uniform))
#printing of results
cat("Normal Data Set Moments: Mean =", moments_norm[1], ", Variance =", moments_norm[2], ", Skewness =", moments_norm[3], ", Kurtosis =", moments_norm[4], "\n\n")
cat("Skewed-Right Data Set Moments: Mean =", moments_right[1], ", Variance =", moments_right[2], ", Skewness =", moments_right[3], ", Kurtosis =", moments_right[4], "\n\n")
cat("Skewed-Left Data Set Moments: Mean =", moments_left[1], ", Variance =", moments_left[2], ", Skewness =", moments_left[3], ", Kurtosis =", moments_left[4], "\n\n")
cat("Uniform Data Set Moments: Mean =", moments_unif[1], ", Variance =", moments_unif[2], ", Skewness =", moments_unif[3], ", Kurtosis =", moments_unif[4], "\n")

#2
meanmoment <- function(data) {
  mean_val <- mean(data)
  centered_data <- data - mean_val
  n <- length(data)
  
  moment_1 <- mean_val
  moment_2 <- sum(centered_data^2) / n
  moment_3 <- sum(centered_data^3) / n
  moment_4 <- sum(centered_data^4) / n
  
  return(c(moment_1, moment_2, moment_3, moment_4))
}
meanmoments_norm <- meanmoment(normal)
meanmoments_right <- meanmoment(skewright)
meanmoments_left <- meanmoment(skewleft)
meanmoments_unif <- meanmoment(uniform)
#printing of results
cat("Normal Data Set Moments about the Mean: 1st Moment =", meanmoments_norm[1], ", 2nd Moment =", meanmoments_norm[2], ", 3rd Moment =", meanmoments_norm[3], ", 4th Moment =", meanmoments_norm[4], "\n\n")
cat("Skewed-Right Data Set Moments about the Mean: 1st Moment =", meanmoments_right[1], ", 2nd Moment =", meanmoments_right[2], ", 3rd Moment =", meanmoments_right[3], ", 4th Moment =", meanmoments_right[4], "\n\n")
cat("Skewed-Left Data Set Moments about the Mean: 1st Moment =", meanmoments_left[1], ", 2nd Moment =", meanmoments_left[2], ", 3rd Moment =", meanmoments_left[3], ", 4th Moment =", meanmoments_left[4], "\n\n")
cat("Uniform Data Set Moments about the Mean: 1st Moment =", meanmoments_unif[1], ", 2nd Moment =", meanmoments_unif[2], ", 3rd Moment =", meanmoments_unif[3], ", 4th Moment =", meanmoments_unif[4], "\n")

#3
numbermoment <- function(data, number) {
  n <- length(data)
  centered_data <- data - number
  
  moment_1 <- sum(centered_data) / n
  moment_2 <- sum(centered_data^2) / n
  moment_3 <- sum(centered_data^3) / n
  moment_4 <- sum(centered_data^4) / n
  
  return(c(moment_1, moment_2, moment_3, moment_4))
}
moment75_norm <- numbermoment(normal, 75)
#printing of results
cat("1st Moment about 75 =", moment75_norm[1], ", 2nd Moment about 75 =", moment75_norm[2], ", 3rd Moment about 75 =", moment75_norm[3], ", 4th Moment about 75 =", moment75_norm[4], "\n")

#4
#a
m2 <- moment75_norm[2] - (moment75_norm[1])^2
m2
#b
m3 <- moment75_norm[3] - 3*moment75_norm[1]*moment75_norm[2] + 2*moment75_norm[1]^3
m3
#c
m4 <- moment75_norm[4] - 4*moment75_norm[1]*moment75_norm[3] + 6*(moment75_norm[1]^2)*moment75_norm[2] - 3*moment75_norm[1]^4
m4
#printing of results
cat("a) 2nd moment about mean =", meanmoments_norm[2], ", m2 =", m2, "\n")
cat("b) 3rd moment about mean =", meanmoments_norm[3], ", m3 =", m3, "\n")
cat("c) 4th moment about mean =", meanmoments_norm[4], ", m4 =", m4, "\n")
cat("Therefore, the relations between the moments have been verified.")