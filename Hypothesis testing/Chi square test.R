## Chi-square test is used to find the association between two categorical variables.

#--> Chi square test compares observed frequency with expected frequency (freq. when there is no association)
# and checks if the values are significantly different form observed frequency.
#--> It assumes that, each observations are independent of each other.
#--> If observations are independent of each other we use Mcnemar's or Cochran's test.

data <- iris

data$size <- ifelse(data$Sepal.Length < median(data$Sepal.Length),
               'small', 'big')

# creating a contingency table.

table(data$Species, data$size)

# performing chi square test.
test <- chisq.test(table(data$Species, data$size))
test

# p-value < 2.2e-16
# Since the p value is less than 0.01 we reject the null hypothesis at 1% 
# level of significance and conclude that there is a association between species and
# size of the flowers.

test$statistic
test$p.value
test$expected
test$method

# second method:
summary(table(data$Species, data$size))

#### visualization for better representation:
# load packages
library(ggstatsplot)
library(ggplot2)

# plot
ggbarstats(
  data,
  x = size,
  y = Species
) +
  labs(caption = NULL) # remove caption
