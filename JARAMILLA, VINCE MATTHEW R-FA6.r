#Statistical Theory FA6 - Jaramilla
install.packages("car")
library(car)

#participant <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
#cloak <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1)
#mischief <- c(3,1,5,4,6,4,6,2,0,5,4,5,4,3,6,6,8,5,5,4,2,5,7,5)


# Assumption 1: Normality - Checking the normality of the two groups
# You can use a normality test like Shapiro-Wilk test or visual inspection (e.g., histogram, Q-Q plot)

# Shapiro-Wilk test for cloak = 0 group
shapiro.test(Invisibility.Cloak$Mischief[Invisibility.Cloak$Cloak == 0])

# Shapiro-Wilk test for cloak = 1 group
shapiro.test(Invisibility.Cloak$Mischief[Invisibility.Cloak$Cloak == 1])

# Visual inspection (histogram and Q-Q plot)
par(mfrow = c(1,2))
hist(Invisibility.Cloak$Mischief[Invisibility.Cloak$Cloak == 0], main = "Cloak = 0", xlab = "Mischief Level", col = "lightblue")
hist(Invisibility.Cloak$Mischief[Invisibility.Cloak$Cloak == 1], main = "Cloak = 1", xlab = "Mischief Level", col = "lightgreen")
qqnorm(Invisibility.Cloak$Mischief[Invisibility.Cloak$Cloak == 0], main = "Cloak = 0", col = "lightblue")
qqline(Invisibility.Cloak$Mischief[Invisibility.Cloak$Cloak == 0], col = "blue")
qqnorm(Invisibility.Cloak$Mischief[Invisibility.Cloak$Cloak == 1], main = "Cloak = 1", col = "lightgreen")
qqline(Invisibility.Cloak$Mischief[Invisibility.Cloak$Cloak == 1], col = "darkgreen")

# Assumption 2: Homogeneity of Variance - Checking the homogeneity of variances
# You can use Levene's test or visual inspection (e.g., boxplot)

# Levene's test for homogeneity of variances
#leveneTest(Invisibility.Cloak$Mischief ~ Invisibility.Cloak$Cloak)

# Visual inspection (boxplot) of homogeneity of variances
boxplot(Invisibility.Cloak$Mischief ~ Invisibility.Cloak$Cloak, main = "Boxplot of Mischief Level by Cloak", xlab = "Cloak", ylab = "Mischief Level", col = c("lightblue", "lightgreen"))

# Assumption 3: Independence - Assuming the data points are independent (this is often assumed)

# Assumption 4: Data Format - Confirming the data format and structure
str(Invisibility.Cloak$Mischief)

# Assumption 5: Equal Sample Size (if applicable) - Checking if the two groups have equal sample sizes
table(Invisibility.Cloak$Cloak)

# Performing independent samples t-test
t_test_result <- t.test(Invisibility.Cloak$Mischief ~ Invisibility.Cloak$Cloak)

# Displaying the result
print(t_test_result)
