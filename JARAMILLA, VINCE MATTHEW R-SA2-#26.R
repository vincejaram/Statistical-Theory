#install needed packages
if (!requireNamespace("car", quietly = TRUE)) {
  # If not installed, install the package
  install.packages("car")
}

#load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)

#explore the data
summary(RatExploration_csvFile)

#       ID          Stimuli               Time          X
#Min.   : 1.00   Length:36          Min.   :0.500   Mode:logical  
#1st Qu.: 9.75   Class :character   1st Qu.:1.688   NA's:36       
#Median :18.50   Mode  :character   Median :3.025                 
#Mean   :18.50                      Mean   :2.697                 
#3rd Qu.:27.25                      3rd Qu.:3.750                 
#Max.   :36.00                      Max.   :5.000     


#one-way ANOVA
result_anova <- aov(RatExploration_csvFile$Time ~ RatExploration_csvFile$Stimuli, data = RatExploration_csvFile)

#ANOVA assumptions
shapiro.test(residuals(result_anova))  #normality

#Shapiro-Wilk normality test
#data:  residuals(result_anova)
#W = 0.97714, p-value = 0.6481


leveneTest(RatExploration_csvFile$Time ~ RatExploration_csvFile$Stimuli, data = RatExploration_csvFile)  # Check for homogeneity of variances
#Levene's Test for Homogeneity of Variance (center = median)
#  Df   F   value  Pr(>F)
#group  2  0.4313 0.6533
#      33      
#display results


summary(result_anova)

#                               Df Sum Sq Mean Sq F value   Pr(>F)    
#RatExploration_csvFile$Stimuli  2  44.53  22.263   62.09 6.53e-12 ***
#Residuals                      33  11.83   0.359                     
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#post-hoc tests (Tukey's HSD)
posthoc <- TukeyHSD(result_anova)
print(posthoc)

#Tukey multiple comparisons of means
#95% family-wise confidence level

#Fit: aov(formula = RatExploration_csvFile$Time ~ RatExploration_csvFile$Stimuli, data = RatExploration_csvFile)

#$`RatExploration_csvFile$Stimuli`
#                     diff        lwr       upr     p adj
#Picture-Pattern  1.066667  0.4668045  1.666529 0.0003414
#Shape-Pattern   -1.637500 -2.2373622 -1.037638 0.0000004
#Shape-Picture   -2.704167 -3.3040289 -2.104304 0.0000000


# Plot the results
RatExploration_csvFile %>%
  ggplot(aes(x = Stimuli, y = Time, fill = Stimuli)) +
  geom_boxplot() +
  labs(title = "Exploration Time by Stimuli",
       x = "Stimuli",
       y = "Exploration Time (seconds)")