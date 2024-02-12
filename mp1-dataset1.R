# Vincent Lazo
# STA 4163
# Mini project 1, Dataset 1: Wage


# Auxiliary variables:
Wage <- read_excel("Wage.xlsx")
df <- data.frame(Wage)
wage_col <- Wage$wage
health_ins_col <- Wage$health_ins


# Part (a): Hypothesis test

# Step 0: check assumptions

# Check assumption: normal distribution

# Shapiro test for normality
# After running the following line, we see that the p-value < 0.05, therefore
# showing proving the population's normality:
shapiro.test(wage_col)

# Step 1/2: Null and Alternative Hypotheses

# H_0: The variance of wages for those with health insurance is the same as
#      for those who do not have health insurance.
# H_A: The variance of wages for those with health insurance is NOT the same as
#      for those who do not have health insurance.

# Step 3/4: Finding the Test Statistic
# After running the following line, we can see the p-value is 1.812e-06
var.test(wage_col ~ health_ins_col, data = df)

# Step 5: Conclusion

# At alpha = 0.05, we reject the null hypothesis, as 1.812e-06 < 0.05. There is
# enough evidence to claim that the variance of wages for those with health insurance
# is NOT the same as for those who do not have health insurance.



# Part (b): One-factor ANOVA test

# Step 0: Assumptions

# We assume that the population of the workers is approximately normal with
# equal variance.

# Step 1/2: Null and Alternative hypotheses

# H_0: The mean wages are equal across all education levels
# H_A: There are at least two means that are different.

# Step 3/4: Finding the Test Statistic
# After running the following line, we see that the p-value is <2e-16
ts_oneway <- aov(wage ~ education, data = df)
summary(ts_oneway)

# Step 5: Conclusion

# At alpha = 0.05, we reject the null hypothesis, as 2e-16 < 0.05. There is
# sufficient evidence that there are at least two mean wages that are different.



# Part (c): Blocking

# Step 0: Assumptions

# We assume that the population has a normal distribution, the blocks are randomly selected,
# and the block variances are equal.

# Step 1/2: Null and Alternative hypotheses

# H_0: The two block means are equal.
# H_A: The two block means are not equal.

# Step 3/4 Finding the Test Statistic
# After running the following line, we can see that the p-value is < 2e-16
block_result <- aov(wage ~ education + as.factor(jobclass), data=df)
summary(block_result)

# Step 5: Conclusion
# At alpha = 0.05, we reject the null hypothesis, as 2e-16 < 0.05. There is sufficient
# evidence to show that blocking by job class was effective.



# Part (d): Finding lowest/ highest mean wage (since blocking was effective)
tukey_result <- TukeyHSD(block_result)
print(tukey_result)

# When running the print statement, we can see that:
# The education level with the lowest mean wage was "HS Grad" and
# the education level with the highest mean wage was "Advanced Degree"



# Part (e): Two-factor ANOVA test

# Step 0: Assumptions

# We assume the normality and constant 

