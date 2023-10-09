#Statistical Theory FA5 - Jaramilla
library("ggplot2")
#8.18
#assigning values
hours <- c(9, 12, 15)
#computations
first_values <- numeric(0)
second_values <- numeric(0)
sample_means <- numeric(0)
xbar_values <- numeric(0)
for (i in 1:length(hours)) {
  for (j in 1:length(hours)) {
    hours_sample <- c(hours[i], hours[j])
    hours_mean <- mean(hours_sample)
    hours_xbar <- hours_mean
    
    first_values <- c(first_values, hours_sample[1])
    second_values <- c(second_values, hours_sample[2])
    sample_means <- c(sample_means, hours_mean)
    xbar_values <- c(xbar_values, hours_xbar)
  }
}
pxbar_values <- numeric(0)
xbar_pxbar_values <- numeric(0)
xbar_squared_pxbar_values <- numeric(0)
for (hours_mean in sample_means) {
  hours_pxbar <- sum(hours_mean == sample_means) / length(sample_means)
  hours_xbar_pxbar <- hours_mean * hours_pxbar
  hours_xbar_squared_pxbar <- (hours_mean^2) * hours_pxbar
  
  pxbar_values <- c(pxbar_values, hours_pxbar)
  xbar_pxbar_values <- c(xbar_pxbar_values, hours_xbar_pxbar)
  xbar_squared_pxbar_values <- c(xbar_squared_pxbar_values, hours_xbar_squared_pxbar)
}
table_8.18 <- data.frame(First = first_values,
                          Second = second_values,
                          Sample_Mean = sample_means,
                          Xbar = xbar_values,
                          Pxbar = pxbar_values,
                          Xbar_Pxbar = xbar_pxbar_values,
                          Xbar_Squared_Pxbar = xbar_squared_pxbar_values)
#printing of results
print(table_8.18)
ggplot(table_8.18, aes(x = Xbar, y = Pxbar)) +
  geom_point() +
  labs(x = "xbar", y = "P(xbar)") +
  ggtitle("Scatterplot of xbar vs. P(xbar)")

#8.21
#assigning values
population <- c(3, 7, 11, 15)
#computations
population_mean <- mean(population) #a
population_sd <- sd(population) #b
sampling_mean <- population_mean #c
sampling_sd <- population_sd/sqrt(2) #d
#printing of results
cat("Population Mean:", population_mean, "\n")
cat("Population Standard Deviation:", population_sd, "\n")
cat("Mean of the Sampling Distribution of Means:", sampling_mean, "\n")
cat("Standard Deviation of the Sampling Distribution of Means:", sampling_sd, "\n")

#8.34
#assigning values
children <- 200
prob <- 0.5
#computations
#a
a_40 <- 0.4*children
a_prob <- pbinom(a_40 - 1, children, prob)
#b
b_43 <- 0.43*children
b_57 <- 0.57*children
b_prob <- pbinom(b_57, children, prob) - pbinom(b_43 - 1, children, prob)
#c
c_54 <- 0.54*children
c_prob <- 1 - pbinom(c_54 - 1, children, prob)
#printing of results
cat("Probability that less than 40% will be boys:", a_prob, "\n")
cat("Probability that between 43% and 57% will be girls:", b_prob, "\n")
cat("Probability that more than 54% will be boys:", c_prob, "\n")

#8.49
#assigning values
x <- c(6, 9, 12, 15, 18)
x_prob <- c(0.1, 0.2, 0.4, 0.2, 0.1)
#computations
x_mean <- sum(x*x_prob)
x_var <- sum((x-x_mean)^2*x_prob)
samples <- expand.grid(x, x)
sample_means <- rowMeans(samples)
sample_probs <- numeric(length(sample_means))
for (i in 1:length(sample_means)) {
  sample_probs[i] <- sum(x_prob[x == samples[i, 1]] * x_prob[x == samples[i, 2]])
}
sample_data <- data.frame(Sample = apply(samples, 1, paste, collapse = ", "),
                          Sample_Mean = sample_means,
                          Probability = sample_probs)
#printing of results
cat("Mean:", x_mean, "\n")
cat("Variance:", x_var, "\n")
print(sample_data)