##################################################################
# Point estimation classic vs bayesian vs bootstrap                                      
# Author:  Jose Antonio Perusquia Cortes
# Afil:    Facultad de Ciencias - UNAM
# Module:  Statistical inference
##################################################################

##################################################################
# Libraries
library(ggplot2)        # Versión 3.4.4
library(ggthemes)       # Versión 5.0.0
library(dplyr)          # Version 1.32.0-2
##################################################################

##################################################################
# Bernoulli experiments for different sample sizes
theta = .7
n = seq(10,100,by=10)

theta_emv = numeric(length(n))
theta_bay = numeric(length(n))
theta_boot = numeric(length(n))

# Bayesian prior is beta(alpha,beta)
alpha = 3
beta = 1

# Number of resamples for bootstrap
M=500

set.seed(314)
for(i in 1:length(n)){
  
  # Data
  x = rbinom(n[i],1,theta)
  
  # EMV
  theta_emv[i] = mean(x)
  
  # Bayes estimator with unif(0,1) prior
  theta_bay[i] = (alpha+sum(x))/(n[i]+alpha+beta)
  
  # Transform data to dataframe to use dplyr
  x = as.data.frame(x)
  
  boot_dist = replicate(
    n=M,
    expr = { x %>%
        slice_sample(prop=1,replace=T)%>%
        summarise(mean_resample=mean(x))%>%
        pull(mean_resample)
    }
  )
  
  theta_boot[i] = mean(boot_dist)
}

# Plots
estimators = c(theta_emv,theta_bay,theta_boot)
type = c(rep('EMV',length(n)),
         rep('Bayes',length(n)),
         rep('Bootstrap',length(n)))
n=rep(n,3)

plot_df = data.frame(n,estimators,type)

ggplot(data=plot_df,aes(x=n,y=estimators,col=type))+
  geom_point()+
  geom_line()+
  theme_minimal()+
  labs(x='',y='')+
  theme(legend.title = element_blank())
##################################################################