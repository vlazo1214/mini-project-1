# Vincent Lazo
# STA 4163
# Mini project 1, Dataset 1: Wage


# Auxiliary variables:
df <- data.frame(Wage)
wage_col <- Wage$wage
health_ins_col <- Wage$health_ins

attach(df)



# Part (a): Hypothesis test

# Step 0: check assumptions

# Check assumption: normal distribution

# Shapiro test for normality
# After running the following line, we see that the p-value < 0.05, therefore
# showing proving the population's normality:
shapiro.test(wage_col)

# Step 1/2: Null and Alternative Hypotheses

# H_0: The variance of wages for those with health insurance IS the same as
#      for those who do not have health insurance.
# H_A: The variance of wages for those with health insurance IS NOT the same as
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

# We assume that each factor-level combination has a normal distribution, the
# response variance is constant for all treatments, and random and independent
# samples are taken for each treatment.

# Step 1/2: Null and Alternative hypotheses

# H_0: There is NO interaction between the mean wages when comparing workers with health
#      insurance and their education level (i.e. there is no difference in their means).

# H_A: There IS an interaction between the mean wages when comparing workers with health
#      insurance and their education level. (i.e. at least two means differ)


# Step 3/4: Finding the test statistic
ts_twoway <- aov(wage ~ education + health_ins + education * health_ins, data = df)
summary(ts_twoway)

# Alternative:

# Tukey
TukeyHSD(ts_twoway)
plot(TukeyHSD(ts_twoway))


# Conclusion

# We see in the summary of the two-factor ANOVA test performed on line 122
# that the p-value is 0.131, which is larger than alpha = 0.05, which means we
# can conclude that there is enough evidence to show that the factors interact with each other. Although I saw that a
# post-hoc wasn't necessary, I did include it to further demonstrate that the factors
# interact with each other since there are lines that cross.
