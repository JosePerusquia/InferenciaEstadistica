##################################################################
# Signs test                                           
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
# Median test

# Example of rents in Mexico City where H0: Median is 16000 vs
# H1: Median is less than 16000
rents = c(4000,15000,14000,8500,15000,14000,5000,19500,14900,
           6700,7000,12000,6500,14000,9500,15000,15500,17000,
           28000,6000,8000,20500)

# Count the number of rents that are above the median
x = sum(sign(rents-16000)==1)
n = length(rents)

# Under H0 the distribution of the test statistic is a symmetric 
# binomial of parameters n=22, p = .5
z = c(0:22)
fz = dbinom(z,22,.5)

df_z = data.frame(z,fz)
ggplot(data=df_z,aes(x=z,y=fz))+
  geom_point()+
  geom_segment(x=z,xend=z,y=0,yend=fz,linetype=2)+
  labs(x='',y='')+
  theme_minimal()

# The test can be carried out with the binom.test function
binom.test(x,n,alternative = 'less')

# The p-value is the probability of observing something
# as extreme, in this case the probability of being less than 4
pbinom(4,22,.5)

# The confidence interval is given by Clopper - Pearson 
# in this case is (0,be(1-alpha,x+1,n-x))
CI = c(0,qbeta(.95,x+1,n-x));CI

# For H1: Median > 16000 
binom.test(x,n,alternative = 'greater')

# In this case the p-value is given by P(Z>=4)
pbinom(3,22,.5,lower.tail = F)

# The confidence interval is given by (be(alpha,x,n-x+1),1)
CI = c(qbeta(.05,x,n-x+1),1);CI

# Finally for H1: Median != 16000 the test is
binom.test(x,n)

# The p-value is 2*min(P(Z<=4),P(Z>=4))
2*min(pbinom(4,22,.5),pbinom(3,22,.5,lower.tail = F))

# The CI is (be(alpha/2,x,n-x+1),be(1-alpha/2,x+1,n-x))
CI=c(qbeta(.025,x,n-x+1),qbeta(.975,x+1,n-x));CI
##################################################################

##################################################################
# Paired-sample test

# Sensitivity to hypnosis before and after treatment. H0 there is
# no difference Median = 0 vs H1: Med > 0 (more sensitive to 
# hypnosis)
before = c(10,16,7,4,7,2)
after = c(18,19,11,3,5,3)

# Number of patients more susceptible 
x = sum(sign(after-before)==1)
n = length(after)

# Binomial test
binom.test(x,n,alternative='greater')
##################################################################
