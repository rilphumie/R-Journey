####################################################################################################
# Chi Square Lab

# Part - I 

# a) 
chisq.test(rbind(c(42,203),c(7,114)),correct=FALSE)
# p-value = 0.002684
chisq.test(rbind(c(42,7), c(203,114)), correct = FALSE)
# p-value = 0.002684

# b)
# calculate the total number of women, as well as the fraction of women who had cervical cancer 
# total number of women
(42 + 7 + 203 + 114)
# 366
# fraction of women who had cervical cancer
(42+7) /(42 + 7 + 203 + 114)
# 0.1338798

# the fraction of women who who didn't have cervical cancer 
(203 + 114) / (42 + 7 + 203 + 114)
# 0.8661202

# Also calculate the fraction of women who had their first pregnancy before 25 years
(42+203) / (42 + 7 + 203 + 114)
# 0.6693989

# Also calculate the fraction of women who had their first pregnancy after 25 years
(7+114) / (42 + 7 + 203 + 114)
# 0.3306011

# c)
# Probability of women who had their first pregnancy before 25 years and had cervical cancer
((42+203) / (42 + 7 + 203 + 114)) * ((42+7) / (42 + 7 + 203 + 114))
# 0.08961898

# Probability of women who had their first pregnancy before 25 years and didn't have cervical cancer
((42+203) / (42 + 7 + 203 + 114)) * ((203 + 114) / (42 + 7 + 203 + 114))
# 0.5797799

# Probability of women who had their first pregnancy after 25 years and had have cervical cancer
((7+114) / (42 + 7 + 203 + 114)) *((42+7) / (42 + 7 + 203 + 114))
# 0.0442608

# Probability of women who had their first pregnancy after 25 years and didn't have cervical cancer
((7+114) / (42 + 7 + 203 + 114)) * ((203 + 114) / (42 + 7 + 203 + 114))
# 0.2863403

# expected value of who had their first pregnancy before 25 years and had cervical cancer
0.08961898*366
# 32.80055

# expected value of who had their first pregnancy before 25 years and didn't have cervical cancer
0.5797799*366
# 212.1994

# expected value of who had their first pregnancy after 25 years and had cervical cancer
0.0442608 * 366
# 16.19945

# expected value of who had their first pregnancy after 25 years and didn't have cervical cancer
0.2863403 * 366 
# 104.8005

# matrix of observed values
observed <- matrix(c(42,203,7,114),nrow =2,ncol = 2)

# matrix of expected values
expected <- matrix(c(32.80055,212.1994,16.19945,104.8005),nrow =2,ncol = 2)

# Calculating the Chisqaure Test statistics 
Chi_Square <- sum(c(((observed - expected)^2)/(expected)))

# degree of freedom = (2-1)(2-1) =  1
dof <- 1

# calculating the p-value from chi-sqaure test statistics
pchisq(Chi_Square,df = dof, lower.tail = FALSE)
# p-value = 0.0027

# Alternate Solution
# Chi-Square Test
chisq.test(observed, expected, correct=FALSE)

# Pearson's Chi-squared test
# 
# data:  observed
# X-squared = 9.0107, df = 1, p-value = 0.002684

# Note : P-value < 0.05 we can reject the 
# null hypothesis that there is no association between cervical cancer and age at first pregnancy) 


# d) 
# total number of women
# 366

# use marginal probabilities from b)

# women who had cervical cancer
0.1338798 * 366 
# 49

# women who didn't have cervical cancer
0.8661202*366
# 317

#women who had their first pregnancy before 25 years 
0.6693989*366
# 245

# women who had their first pregnancy after 25 years 
0.3306011*366
# 121

# women who had cervical cancer and had their first pregnancy before 25 years 
# 49 # given in the question 

# Start filling in each cell in the matrix using row and column totals from above
# so women who had cervical cancer and had their first pregnancy after 25 years 
49 - 49 
# 0

# women who didn't have cervical cancer and had their first pregnancy before 25 years 
245 - 49 
# 196

# women who didn't have cervical cancer and had their first pregnancy after 25 years 
317 - 196
# 121

# degrees of freedom on a 3x4 contigency table
(3-1)*(4-1)
# 6

# e)
## note Parts of question e) done in c)

# matrix of observed values given in the question
obs <- matrix(c(33,212,16,105),nrow =2,ncol = 2)

# matrix of expected values
expected <- matrix(c(32.80055,212.1994,16.19945,104.8005),nrow =2,ncol = 2)

# Calculating the Chisqaure Test statistics 
Chi_Square <- sum(c(((obs - expected)^2)/(expected)))

# degree of freedom = (2-1)(2-1) =  1
dof <- 1

# calculating the p-value from chi-sqaure test statistics
pchisq(Chi_Square,df = dof,lower.tail = FALSE)

# Alternate Solution
# Chi-Square Test
chisq.test(observed,expected,correct=FALSE)

# Pearson's Chi-squared test
# 
# data:  observed
# X-squared = 9.0107, df = 1, p-value = 0.002684

# f)
chi2 <- rchisq(20000, df=1) 
hist(chi2,30)
abline(v=9.01)

# g) 
length(chi2[chi2 > 5])

# h)
# No. You only rejected the null hypothesis of no association. You showed cervical
# cancer is associated with pregnancy before age 25. Association, or correlation,
# do not prove causality.
# We can say the chance of the following statement being false is less than 1%:
# "pregnancy before age 25 is associated with cervical cancer."

# i)
fisher.test(rbind(c(42,203),c(7,114)))

# p-value = 0.002937
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval: 1.433247 9.158597
# sample estimates: odds ratio 3.360159 

# Part 2
?pf
pf(1.126577, 49, 49, lower.tail = F)
# 0.3391081 > 0.05
# We reject Ho, Mill operator 2 does not have a significantly lower vaiance than Mill operator 1
