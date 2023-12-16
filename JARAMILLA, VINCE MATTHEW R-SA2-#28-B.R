#necessary libraries
library(ggplot2)
library(tidyr)
library(car)

#visualize data
ggplot(Alzheimers.Mice.Data, aes(x = Treatment, y = Memory, group = AD_Status, color = factor(AD_Status))) +
  geom_line() +
  geom_point() +
  labs(title = "Memory Errors by Treatment and AD Status", x = "Treatment", y = "Memory Errors") +
  theme_minimal()

# Check the assumptions
# Residuals vs. Fitted Plot
plot(model, which = 1)

# Normal Q-Q Plot
qqnorm(residuals(model))
qqline(residuals(model))

# Homogeneity of Variances
fligner.test(residuals(model) ~ interaction(AD_Status, Treatment), data = Alzheimers.Mice.Data)

#Fligner-Killeen test of homogeneity of variances

#data:  residuals(model) by interaction(AD_Status, Treatment)
#Fligner-Killeen:med chi-squared = 2.4865, df = 7, p-value = 0.9281


# Two-Factor ANOVA
anova_result <- Anova(model, type = "III")
summary(anova_result)     

#     Sum Sq              Df        F value            Pr(>F)         
#Min.   :  2.592   Min.   : 1   Min.   : 0.8651   Min.   :0.0000017  
#1st Qu.:  5.445   1st Qu.: 1   1st Qu.: 1.5793   1st Qu.:0.0821545  
#Median :  8.067   Median : 1   Median : 2.2549   Median :0.1477945  
#Mean   : 44.314   Mean   : 8   Mean   : 9.4882   Mean   :0.1635239  
#3rd Qu.: 97.607   3rd Qu.: 1   3rd Qu.:10.1637   3rd Qu.:0.2291638  
#Max.   :107.860   Max.   :36   Max.   :32.5778   Max.   :0.3585049  
#                               NA's   :1         NA's   :1  