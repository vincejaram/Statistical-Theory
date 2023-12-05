install.packages("car")
library(car)

#declaration of data
males <- c(12, 7, 7, 10, 8, 10, 11, 9, 9, 13, 4, 9, 12, 11, 9, 9, 7, 12, 10, 13, 11, 10, 6, 12, 11, 9, 10, 12, 8, 9, 13, 10, 9, 7, 10, 7, 10, 8, 8, 11, 10, 11, 9, 15, 8, 9, 9, 11, 13, 10, 13)
females <- c(11, 10, 11, 10, 11, 12, 12, 10, 9, 9, 9, 10, 8, 7, 12, 9, 7, 8, 9, 8, 7, 9, 9, 12, 10, 9, 13, 9, 9, 10, 9, 6, 12, 8, 11, 8, 8, 11, 11, 12, 9, 10, 11, 14, 12, 7, 11, 11, 10, 9, 11)

#two-sample t-test
t_test_result <- t.test(males, females, alternative = "two.sided", var.equal = TRUE)

#printing results
print(t_test_result)

#2
if (t_test_result$p.value <= 0.05) {
  cat("The p-value is", t_test_result$p.value, "which is less than or equal to 0.05.\n")
  cat("Therefore, we reject the null hypothesis.\n")
} else {
  cat("The p-value is", t_test_result$p.value, "which is greater than 0.05.\n")
  cat("Therefore, we do not reject the null hypothesis.\n")
}

#3
#descriptive statistics for males
summary_males <- summary(males)

#descriptive statistics for females
summary_females <- summary(females)

#printing results
print("Descriptive Statistics for Males:")
print(summary_males)

print("\nDescriptive Statistics for Females:")
print(summary_females)

#4
#95% confidence intervals
ci_males <- t.test(males)$conf.int
ci_females <- t.test(females)$conf.int
ci_difference <- t.test(males, females)$conf.int

#printing results
cat("95% Confidence Interval for Males: [", ci_males[1], ", ", ci_males[2], "]\n")
cat("95% Confidence Interval for Females: [", ci_females[1], ", ", ci_females[2], "]\n")
cat("95% Confidence Interval for the Difference between Means: [", ci_difference[1], ", ", ci_difference[2], "]\n")

#6
#Shapiro-Wilk test for normality
shapiro_test_males <- shapiro.test(males)
shapiro_test_females <- shapiro.test(females)

#printing results
print("Shapiro-Wilk Test for Normality - Males:")
print(shapiro_test_males)

print("\nShapiro-Wilk Test for Normality - Females:")
print(shapiro_test_females)

#Levene's test for homogeneity of variances
levene_test <- leveneTest(males, females)

#printing results
print("Levene's Test for Homogeneity of Variances:")
print(levene_test)

#two-sample t-test
t_test_result <- t.test(males, females, alternative = "two.sided", var.equal = TRUE)

#printing results
print(t_test_result)
if (t_test_result$p.value <= 0.05) {
  cat("The p-value is", t_test_result$p.value, "which is less than or equal to 0.05.\n")
  cat("Therefore, we reject the null hypothesis.\n")
} else {
  cat("The p-value is", t_test_result$p.value, "which is greater than 0.05.\n")
  cat("Therefore, we do not reject the null hypothesis.\n")
}