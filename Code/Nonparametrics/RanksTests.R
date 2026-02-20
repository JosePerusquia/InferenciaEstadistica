##################################################################
# Ranks tests                                           
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
# Mann-Whitney (Wilcoxon rank sum test) for 2 independent samples
# with H0:theta=0 vs H1:theta!=0 where Fy(x)=Fx(x-theta)

# mean sister chromatid exchange (MSCE) in native and caucasian
native = c(8.5,9.48,8.65,8.16,8.83,7.76,8.63)
m = length(native)

caucasian = c(8.27,8.20,8.25,8.14,9,8.1,7.2,8.32,7.7)
n = length(caucasian)

# Wilcox exact test (returns the Mann-Whitney statistic)
res = wilcox.test(native,caucasian,exact=T);res
U = res$statistic;U

# Wilcoxon statistic is given by 
W = U+(m*(m+1)/2);W

# Normal approximation with continuity correction (no ties)
mu_U = m*n/2;mu_U
var_U = m*n*(m+n+1)/12

# We substract -.5 since U > mu_U
Z = (U-.5-mu_U)/sqrt(var_U)
pval = 2*pnorm(Z,lower.tail=F);pval

# Wilcox test in R
wilcox.test(native,caucasian,exact=F,continuity=T)
##################################################################

##################################################################
# Signed rank Wilcoxon test 

# Example of accidents (person/hour) of 8 industrial plants before
# and after receiving a security programme

before=c(51.2,46.5,24.1,10.2,65.3,92.1,30.3,49.2)
after=c(45.8,41.3,15.8,11.1,58.5,70.3,31.6,35.4)

# Number of industrial plants with less accidents
x = sum(sign(before-after)==1)
n = length(after)

# Signs test 
binom.test(x,n,alternative='g')

# Wilcoxon signed rank test
wilcox.test(before,after,alternative='g',paired = T)
##################################################################

