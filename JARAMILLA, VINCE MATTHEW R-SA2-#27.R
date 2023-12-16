#install needed packages
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}
if (!requireNamespace("multcomp", quietly = TRUE)) {
  install.packages("multcomp")
}
if (!requireNamespace("emmeans", quietly = TRUE)) {
  install.packages("emmeans")
}

#necessary libraries
library(tidyr)
library(stats)
library(car)
library(multcomp)
library(emmeans)

#dataframe
car_mileage <- data.frame(
  Cars = rep(c(1, 2, 3, 4, 5), each = 4),
  Oil = rep(c(1, 2, 3, 4), times = 5),
  Mileage = c(36, 38, 30, 29, 34, 38, 30, 29, 34, 28, 38, 32, 38, 34, 20, 44, 26, 28, 34, 50)
)

#wide format (each engine oil as a separate column)
wide_data <- spread(car_mileage, key = Oil, value = Mileage)

#repeated measures ANOVA
result_anova <- aov(Mileage ~ Oil + Error(Cars/Oil), data = car_mileage)

#display results
summary(result_anova)

#Error: Cars
#Df Sum Sq Mean Sq F value Pr(>F)
#Residuals  1  5.625   5.625               

#Error: Cars:Oil
#Df Sum Sq Mean Sq
#Oil  1    107     107

#Error: Within
#Df Sum Sq Mean Sq F value Pr(>F)  
#Oil        1  195.0  195.01   5.894 0.0274 *
#Residuals 16  529.4   33.09                 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#assumptions
#normality of residuals
shapiro.test(car_mileage$Mileage)

#Shapiro-Wilk normality test
#data:  car_mileage$Mileage
#W = 0.95894, p-value = 0.5228


#sphericity
sphericity_test <- oneway.test(car_mileage$Mileage ~ car_mileage$Oil)
print(sphericity_test)

#One-way analysis of means (not assuming equal variances)
#data:  car_mileage$Mileage and car_mileage$Oil
#F = 0.46559, num df = 3.0000, denom df = 8.6652, p-value = 0.7137


#homogeneity of variances
bartlett_test <- bartlett.test(Mileage ~ Oil, data = car_mileage)
print(bartlett_test)

#Bartlett test of homogeneity of variances
#data:  Mileage by Oil
#Bartlett's K-squared = 2.5732, df = 3, p-value = 0.4622


#post-hoc tests
posthoc_test <- pairwise.t.test(car_mileage$Mileage, car_mileage$Oil, p.adj = "bonferroni")
print(posthoc_test)

#Pairwise comparisons using t tests with pooled SD 
#data:  car_mileage$Mileage and car_mileage$Oil 

#  1    2    3   
#2 1.00 -    -   
#3 1.00 1.00 -   
#4 1.00 1.00 0.93

#P value adjustment method: bonferroni 