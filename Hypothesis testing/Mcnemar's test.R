# Mcnemar's test is a non-parametric test used for paired data.
# It can be useful on scenarios like before/after effect and case-control effect.
# Outcomes should be binary.

# Create a 2x2 contingency table representing the counts of symptom relief outcomes
data <- matrix(c(70, 30, 60, 40), nrow = 2,
               dimnames = list("After Treatment B" = c("Relieved", "Not Relieved"),
                               "After Treatment A" = c("Relieved", "Not Relieved")))
data

# performing mcnemar's test:
mcnemar.test(data)

# without continuity correction
mcnemar.test(data, correct = F)

# The null hypothesis:
# H0: There is no difference between these two treatments.
# H1: There is a difference between these two treatments.
# without continuity correction,  p-value = 0.001565

# Hence, we reject the null-hypothesis at 5% level of significance.

####
set.seed(150)
data2 <- data.frame(before = sample(c("Positive",
                                     "Positive",
                                     "Positive",
                                     "Positive",
                                     "Negative"),
                                   300, replace = TRUE),
                   after = sample(c("Positive",
                                    "Positive",
                                    "Positive",
                                    "Positive",
                                    "Negative"),
                                  300, replace = TRUE))
table(data2$before, data2$after)

mcnemar.test(data2$before, data2$after, correct = FALSE)
# McNemar's chi-squared = 0.34615, df = 1, p-value = 0.5563
# There is no significant difference.

library(ggstatsplot)

ggbarstats(
  data2,
  x = before,
  y = after,
  paired = TRUE, # It will perform chi-sqaure test if this is false.
  labels = 'both'
)

# on that plot g means effect size.
library(effectsize)

interpret_cohens_g(0.03)
#"very small"

