# Vincent Lazo
# STA 4163
# Mini project 1, Dataset 1: Wage

# Notes:


Wage <- read_excel("Wage.xlsx")


# Part (a): Hypothesis test

# Step 0: check assumptions

# Check assumption: normal distribution

# Shapiro test for normality
# After running the following line, we see that the p-value < 0.05, therefore
# showing proving the population's normality:
shapiro.test(Wage$wage)

# Step 1/2: Null and Alternative Hypotheses

# H_0: The variance of wages for those with health insurance is the same as
#      for those who do not have health insurance.
# H_A: The variance of wages for those with health insurance is NOT the same as
#      for those who do not have health insurance.

# Step 3: Finding the Test Statistic
# wage_variance_with <- var(Wage)






