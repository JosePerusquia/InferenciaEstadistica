##################################################################
# Interval estimation classic vs bayesian vs bootstrap                                      
# Author:  Jose Antonio Perusquia Cortes
# Afil:    Facultad de Ciencias - UNAM
# Module:  Statistical inference
##################################################################

##################################################################
# Libraries
library(ggplot2)        # Versión 3.4.4
library(ggthemes)       # Versión 5.0.0
library(dplyr)          # Version 1.32.0-2
library(boot)           # Version 1.3-31
##################################################################

##################################################################
# Bernoulli experiments for different sample sizes
theta = .7
n = 20

theta_emv = numeric(3)
theta_bay = numeric(3)
theta_boot = numeric(3)

# Bayesian prior is beta(alpha,beta)
alpha = 1
beta = 1

# Statistic for the bootstrap estimation
boot_mean = function(data, idx) {
  mean(data[idx])
}

# Number of resamples for bootstrap
M=500

# Data
set.seed(314)
x = rbinom(n,1,theta)
#####################################################################

#####################################################################
# Classical approach
Ftheta = function(n,to,theta,alpha,flag=T){
  if(flag){
    target = pbinom(to,n,theta,lower.tail = flag)-alpha
  }else{
    target = pbinom(to-1,n,theta,lower.tail = flag)-alpha
  }
  return(target)
}

ConfInt = function(x,alpha1,alpha2){
  n=length(x)
  to=sum(x)
  root1 = uniroot(Ftheta,n=n,to=to,alpha=alpha1,
                  interval=c(0,1))$root
  root2 = uniroot(Ftheta,n=n,to=to,alpha=alpha2,interval=c(0,1),
                  flag=F)$root
  thetaL=min(root1,root2)
  thetaU=max(root1,root2)
  return(c(thetaL,thetaU))
}

theta_emv[1]=mean(x)
res=ConfInt(x,.025,.025)
theta_emv[2]=res[1]
theta_emv[3]=res[2]

theta_emv
#####################################################################

#####################################################################
# Bootstrap approach
boot_res=boot(x, statistic = boot_mean, R = M)
theta_boot[1] = mean(boot_res$t)

# For perc change bca for perc
ci_boot=boot.ci(boot_res, type = "bca")
theta_boot[2]=ci_boot$bca[4]
theta_boot[3]=ci_boot$bca[5]

theta_boot
#####################################################################

#####################################################################
# Bayesian approach
theta_bay[1] = (alpha+sum(x))/(n+alpha+beta)
theta_bay[2] = qbeta(0.025,alpha+sum(x),n+beta-sum(x))
theta_bay[3] = qbeta(0.975,alpha+sum(x),n+beta-sum(x))
theta_bay
#####################################################################

#####################################################################
# Plots
estimators = c(theta_emv,theta_bay,theta_boot)
type = c(rep('EMV',3),
         rep('Bayes',3),
         rep('Bootstrap',3))
n=c(1,1,1,2,2,2,3,3,3)

plot_df = data.frame(n,estimators,type)

ggplot(data=plot_df,aes(x=n,y=estimators,col=type))+
  geom_point()+
  geom_line(linetype=2)+
  geom_hline(yintercept=.7,col='darkred')+
  theme_minimal()+
  labs(x='',y='')+
  theme(legend.title = element_blank())+
  theme(axis.text.x = element_blank())
##################################################################