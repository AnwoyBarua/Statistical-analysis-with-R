##### t test:

# Types of t test
# One sample t test, Two sided t test, One sided t test,  paired t test.

################# One Sample t test #############

# We perform this test to validate the equality of Population mean value of an variable 
# with an assumed mean based on the sample data we have.

# necessary libraries:
library(tidyverse)
library(patchwork)
library(gapminder)

data(gapminder)
View(gapminder)

# Hypothesis Testing,  H0: The mean life expectancy for Africa is 50 years.
#                      H1: The mean life expectancy for Africa is not 50 years.

# first draw a density plot for better understanding:
# Calculate mean value
mean_value <- gapminder %>% 
                  filter(continent == 'Africa') %>% 
                  select(lifeExp) %>% 
                  summarise(mean = mean(lifeExp))

gapminder %>% 
  filter(continent == 'Africa') %>% 
  select(lifeExp) %>% 
  t.test(mu = 50)

# Comment: significance level: 5%
# the p value is less than 0.05, which means that we have enough statistical evidence from the data 
# to reject the null hypothesis at 5% level of significance.


################# Two sided t-test #################

# H0: there is no difference in mean life expectancy between Africa and Europe. mean1 -mean2 = 0
# H1: there is a difference.


gapminder %>% 
  filter(continent %in% c('Africa', 'Europe')) %>% 
  t.test(lifeExp ~ continent, data = .,
         alternative = "two.sided")

# Comment:
## we reject the null hypothesis. That means, there is no a difference between means.


############## One sided t test ##########

#H0 : Mean value of Ireland < Mean value of switzerland.
# H1: Exact opposite.

gapminder %>% 
  filter(country %in% c("Ireland", "Switzerland")) %>% 
  t.test(lifeExp ~ country, data = . ,
         alternative = 'less')

# Comment: significance level 5 %
# The p value is 0.05835 which greater than 0.05, hence we fail to reject the null hypothesis.


############### Paired t test #########

# H0: There is a difference in life expectancy in two time period 2007 and 1957 for Africa.
# H1: There is no difference. (difference is not equal to zero)


gapminder %>% 
  filter(year %in% c(2007, 1957) & continent == "Africa") %>% 
  mutate(year = factor(year, levels = c(2007, 1957))) %>% 
  t.test(lifeExp ~ year, data = .,
         paired = TRUE)

#p-value = 1.308e-15
# we reject the null hypothesis. There is a difference.



########### Drawing density plots to better understand the difference.





