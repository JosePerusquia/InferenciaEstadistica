##################################################################
# Goodness of fit                                        
# Author: Jose Antonio Perusquia Cortes
# Afil: Facultad de Ciencias - UNAM
# Module: Nonparametric statistical inference
##################################################################

##################################################################
# Required libraries
library(ggplot2)        # Version 4.0.2
library(ggthemes)       # Version 5.2.0
library(nortest)        # Version 1.0-4
##################################################################

##################################################################
# Chi squared test for discrete distributions

# Example on the number of defective observations
defects = c(0,1,2,3,4,5,6)
observations = c(10,24,10,4,1,1,0)
n = sum(observations)

# Mean estimation
mu_hat = sum(defects*observations)/n

# Join the last two cells
defects = c(0,1,2,3,4,5)
observations = c(10,24,10,4,1,1)
K = length(defects)

# Probabilities of the Poisson distribution
probs = numeric(K)
probs[c(1:(K-1))] = dpois(defects[c(1:(K-1))],mu_hat)
probs[K] = ppois(defects[K]-1,mu_hat,lower.tail = F)
sum(probs)

# Expected values per cell
expected = probs*n;expected

# The last expected value is small so we join the last two cells
defects = c(0,1,2,3,4)
observations = c(10,24,10,4,2)
K_new = length(defects)

# Probabilities of the Poisson distribution
probs = numeric(K_new)
probs[c(1:(K_new-1))] = dpois(defects[c(1:K_new-1)],mu_hat)
probs[K_new]=1 - sum(probs[1:(K_new-1)])
sum(probs)

# Expected values
expected = probs*n;expected

# Test statistics
Q = sum(((observations-expected)^2)/expected);Q

# Compare it against a chi-square distribution of 3 degrees
# yielding that we do not reject the null hypothesis
parametersEstimated = 1
df = K_new - 1 - parametersEstimated
alpha=.05
qchisq(1-alpha,df)

# p-value
p_value = pchisq(Q, df=df, lower.tail=FALSE);p_value

# Alternatively in R we use chisq.test functions which
# required the observatios and the theoretical probabilities
# Warning: it does not take into account if you are estimating
# the parameter so it compares against a chi squared distribution
# with df equal to K-1
chisq.test(observations,p=probs)
##################################################################

##################################################################
# Kolmogorov-Smirnov on the weights of eggs and other normality
# tests

# Weights are already ordered so there's no need to sort them
weights = c(156,162,168,182,186,190,193,196,202,210,
            214,220,226,230,232,236,238,242,246,270)
n_weights = length(weights)

# Histogram
df_weights = data.frame(weights)
ggplot(data=df_weights,aes(x=weights))+
  geom_histogram(breaks=hist(weights,plot=F)$breaks,
                 col='black',fill='gold2')+
  labs(x='',y='')+
  theme_minimal()

# Boxplot
ggplot(data=df_weights,aes(x=weights))+
  geom_boxplot(col='black',fill='gold2')+
  labs(x='',y='')+
  theme_minimal()

# qqplot
ggplot(data=df_weights,aes(sample=weights))+
  geom_qq(distribution=qnorm,
          dparams = list(200,sqrt(1225)))+
  geom_qq_line(distribution=qnorm,
               dparams = list(200,sqrt(1225)),
               col='red')+
  labs(x='Theoretical',y='Empirical')+
  theme_minimal()

# Build the empirical distribution
locations=unique(weights);locations
n_unique_weights = length(locations)
jumps=numeric(n_unique_weights)

for(i in 1:length(jumps)){
  jumps[i]=sum(weights==locations[i])/n_weights
}

jumps=cumsum(jumps)

# Prepare the data
x = c(min(locations)-10,locations);x
xend = c(locations,max(locations)+10);xend
y = c(0,jumps)
yend = y
df_Fn=data.frame(x=x,y=y,xend=xend,yend=yend)

p=ggplot(data=df_Fn)+
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend),alpha=.75)+
  theme_minimal()+
  labs(x='',y='')
print(p)

# Theoretical distribution normal with mean 200 and sigma^2=1225
x=seq(min(locations)-10,max(locations)+10,by=.1)
Fx=pnorm(x,mean=200,sd=sqrt(1225))
df_Fx=data.frame(x=x,y=Fx)
p+geom_line(data=df_Fx,aes(x=x,y=y),col='red',linetype='dashed')

# Kolmogorov-Smirnov test
Fn=seq(0,1,by=1/20);Fn
Fx=pnorm(weights,mean=200,sd=sqrt(1225))
df_Fx2=data.frame(weights,Fx)
p=p+
  geom_point(data=df_Fx2,aes(x=weights,y=Fx),col='red3',size=1)
print(p)

# Test statistic
i = 1:n_weights

# We visually check the Dn+ first
df_Dn1 = data.frame(x=weights,y=Fn[-1],z=Fx)
p+geom_segment(data=df_Dn1,aes(x=x,xend=x,y=y,yend=z),
               col='darkblue',linetype='dashed',
               arrow = arrow(length = unit(0.2, "cm")))
Dn1 = max(i/n_weights - Fx);Dn1

# We visually check the Dn-
df_Dn2 = data.frame(x=weights,z=Fn[-21],y=Fx)
p+geom_segment(data=df_Dn2,aes(x=x,xend=x,y=y,yend=z),
               col='darkblue',linetype='dashed',
               arrow = arrow(length = unit(0.2, "cm")))
Dn2 = max(Fx - (i-1)/n_weights);Dn2

# Kolmogorv-Smirnov test in R
ks.test(weights,'pnorm',200,sqrt(1225))

# Lilliefors test in R
lillie.test(weights)

# Cramer-von Mises test in R
cvm.test(weights)

# Anderson Darling test in R
ad.test(weights)

# Shapiro test in R
shapiro.test(weights)
##################################################################