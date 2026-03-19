##################################################################
# Contingency tables                                          
# Author: Jose Antonio Perusquia Cortes
# Afil: Facultad de Ciencias - UNAM
# Module: Nonparametric statistical inference
##################################################################

##################################################################
# Required libraries
library(ggplot2)        # Versión 3.4.4
library(ggthemes)       # Versión 5.0.0
##################################################################

##################################################################
# Chi squared test for independence 

# Vaccines example and getting sick
x = matrix(c(43,237,52,198,25,245,48,212,57,233),nrow=5,byrow=T)
N = sum(x)
r = nrow(x);r
c = ncol(x);c

# Marginal probabilites
p_row = rowSums(x)/N
p_col = colSums(x)/N

# If H0 is true the joint probabilites are the product of the
# marginal ones
probs = matrix(nrow=5,ncol=2)
probs[,1] = p_col[1]*p_row
probs[,2] = p_col[2]*p_row

# Expected values and Pearson statistic
expected = probs*N
Q = sum(((x-expected)^2)/expected);Q

# Compare it against chi-squared distribution with (c-1)*(r-1)
# degrees of freedom (we reject the null hypothesis)
df = (r-1)*(c-1)
alpha = .05
qchisq(1-alpha,df)

# Contingency coefficient
C = (Q/(Q+N))^(1/2);C

# Phi coefficient
phi = sqrt(Q/N);phi

# Cramer's V (same as phi in the case of 2 rows or 2 columns)
V = sqrt(Q/(N*(min(r,c)-1)));V

# In R the test is performed with chisq.test
chisq.test(x)
##################################################################

##################################################################
# Fisher test (for small samples)

# Fisher's example, tea lady
tea = matrix(c(3,1,1,3),byrow=T,nrow=2)
fisher.test(tea,alternative='greater')
##################################################################

##################################################################
# McNemar test

# Pain in patients with cancer May and Johnson (1997)
pain = matrix(c(18,4,12,5),nrow=2,byrow=T)

# Correct=True yields Yates correction
mcnemar.test(pain)
mcnemar.test(pain,correct=F)
##################################################################

