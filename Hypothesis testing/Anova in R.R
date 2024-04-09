## There are several Types of ANOVA: one-way ANOVA, two-way ANOVA, mixed ANOVA, repeated measures Anova
### Source: https://statsandr.com/blog/anova-in-r/#introduction

# Although ANOVA is used to make inference about means of different groups, the method 
# is called “analysis of variance”. Because it compares the “between” 
# variance (the variance between the different groups) and the variance “within” (the variance within each group). 
# If the between variance is significantly larger than the within variance, the group means are declared to be different.


library(palmerpenguins)
library(tidyverse)
  
colnames(penguins)

data <- penguins %>% 
              select(c('species', "flipper_length_mm"))


ggplot(data) +
  aes(x= species, y = flipper_length_mm, color = species) +
  geom_jitter() +
  theme(legend.position = 'none')


# Underlying Assumptions of ANOVA:
#--> one should be continuous quantitative dependent variable and one qualitative independent var.
#--> Independent. (variable are independent of each other. no autocorrelation)
#--> Normality.
#--> Homogeneity.

#### Normality test: 
# visually:

result_aov <- aov(flipper_length_mm ~ species, data)

# par(mfrow = c(1, 2)) # combine plots

hist(result_aov$residuals)
library(car)
qqPlot(result_aov$residuals,
       id = FALSE)

# or we can perform normality test.
shapiro.test(result_aov$residuals)
# p value is greater than 0.05, we fail to reject H0 --> residual follows normal
# distribution.


#### Equality of Variance (aka homogeneity):
# Boxplot
boxplot(flipper_length_mm ~ species, data)
#the boxplot show a similar variance for the different species.

# Levene's test
library(car)

leveneTest(flipper_length_mm ~ species,
           data = data)
# p value is = 0.7188, we fail to reject the null hypothesis.


#### Preliminary Analysis Before anova:
ggplot(data) +
  aes(x = species, y = flipper_length_mm) +
  geom_boxplot()
# flipper_length on Gentoo is larger than other two species.

# Descriptive Stat
data %>% 
  group_by(species) %>% 
  summarise(mean = mean(flipper_length_mm, na.rm = TRUE),
            sd = sd(flipper_length_mm, na.rm = TRUE))

# Above results shows that there is difference in flipper length for species on that sample
# But is it true for population. We confirm it by ANOVA

### ANOVA:
#Research question: Is there any difference in flipper length for 3 different species of penguins?
# H0: there is no difference
# H1: At least one of them is different.

oneway.test(flipper_length_mm ~ species,
            data,
            var.equal = TRUE)
# p-value < 2.2e-16
# we reject the null hypothesis.

# 2nd method:
res_aov <- aov(flipper_length_mm ~ species,
               data = data
)

summary(res_aov)

## When null hypothesis accepted we stop there.
## Since, we failed to reject null hypothesis we can further find which mean is different than others.

### Post-hoc test in R: Tukey HSD, Dunnett test.

library(multcomp)

# Tukey HSD test:
post_test <- glht(res_aov,
                  linfct = mcp(species = "Tukey")
)

summary(post_test)


