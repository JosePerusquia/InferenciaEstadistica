################################################################
# Association tests                                          
# Author: Jose Antonio Perusquia Cortes
# Afil: Facultad de Ciencias - UNAM
# Module: Nonparametric statistical inference
################################################################

################################################################
# Required libraries
library(ggplot2)        # Versión 3.4.4
library(ggthemes)       # Versión 5.0.0
library(DescTools)      # Versión 0.99.56
################################################################

################################################################
# Linear relationship
set.seed(123)
n = 100

x1 = rnorm(n)
y1 = 2*x1 + rnorm(n, sd = 0.5)

df = data.frame(x=x1,y=y1)
ggplot(data=df, aes(x,y)) +
  geom_point() +
  theme_minimal()+
  labs(x='',y='')

cor.test(x1, y1, method = "pearson")
cor.test(x1, y1, method = "spearman")
SpearmanRho(x1,y1,conf.level = .95)
# cor.test(x1, y1, method = 'kendall')
################################################################

################################################################
# Linear relationship with outlier
x2 = x1
y2 = y1
x2[1] = 5
y2[1] = -20

df = data.frame(x=x2,y=y2)
ggplot(data=df, aes(x,y)) +
  geom_point() +
  theme_minimal()+
  labs(x='',y='')

cor.test(x2, y2, method = "pearson")
cor.test(x2, y2, method = "spearman")
SpearmanRho(x2,y2,conf.level = .95)
# cor.test(x2, y2, method = 'kendall')
################################################################

################################################################
# Nonlinear monotone relationship
x3 = runif(n, 0.1, 5)
y3 = log(x3) + rnorm(n, sd = 0.2)

df = data.frame(x=x3,y=y3)
ggplot(data=df, aes(x,y)) +
  geom_point() +
  theme_minimal()+
  labs(x='',y='')

cor.test(x3, y3, method = "pearson")
cor.test(x3, y3, method = "spearman")
#cor.test(x3, y3, method = 'kendall')
################################################################

################################################################
# Quadratic relationship (non-monotonic)
x4 = seq(-2, 2, length.out = n)
y4 = x4^2 + rnorm(n, sd = 0.3)

df = data.frame(x=x4,y=y4)
ggplot(data=df, aes(x,y)) +
  geom_point() +
  theme_minimal()+
  labs(x='',y='')

cor.test(x4, y4, method = "pearson")
cor.test(x4, y4, method = "spearman")
# cor.test(x4, y4, method = 'kendall')
################################################################

################################################################
# Example found in Gibbons pag 440 about the ranks given by
# 2 judges in a competition

judge1 = c(1,5,9,7,4,6,8,2,3)
judge2 = c(4,3,6,8,2,7,9,1,5)

# Pearson correlation test from scratch and comparison with
# cor.test function
r_p = cor(judge1,judge2);r_p
z = .5*log((1+r_p)/(1-r_p))
n = length(judge1)

z_inf = z-qnorm(.975)/sqrt(n-3)
z_sup = z+qnorm(.975)/sqrt(n-3)

r_inf = (exp(2*z_inf)-1)/(exp(2*z_inf)+1);r_inf
r_sup = (exp(2*z_sup)-1)/(exp(2*z_sup)+1);r_sup

cor.test(judge1,judge2)

# Spearman rho p-value from scratch and comparison with
# cor.test exact and t approximation
r_s = cor(judge1,judge2,method='s')
tstat = r_s*(sqrt((n-2)/(1-r_s^2)));tstat
pval = 2*pt(tstat,7,lower.tail = F);pval

cor.test(judge1,judge2,method='s',exact = F)
cor.test(judge1,judge2,method='s',exact = T)

# Spearman CI using Z transform from scratch and comparison
# with DescTools SpearmanRho
z = .5*log((1+r_s)/(1-r_s))

z_inf = z-qnorm(.975)*sqrt(1/(n-3))
z_sup = z+qnorm(.975)*sqrt(1/(n-3))

r_inf = (exp(2*z_inf)-1)/(exp(2*z_inf)+1);r_inf
r_sup = (exp(2*z_sup)-1)/(exp(2*z_sup)+1);r_sup
SpearmanRho(judge1,judge2,conf.level = .95)

# Kendall tau p-value from scratch and comparison with
# cor.test Kendall exact and approximation
r_k = cor.test(judge1,judge2,method='k');r_k
r_k = r_k$estimate
var_T = 2*(2*n+5)/(9*n*(n-1))

z = r_k/sqrt(var_T); z
pval = 2*min(pnorm(z,lower.tail = F),
             pnorm(z,lower.tail = T));pval

cor.test(judge1,judge2,method='k',exact = F)
cor.test(judge1,judge2,method='k',exact = T)

# Kendall confidence intervals using DescTools, since there
# are no ties we use KendallTauA otherwise use KendallTauB
KendallTauA(judge1,judge2,conf.level = .95)
################################################################