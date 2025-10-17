#####################################################################
# Confidence interval for beta(theta,1) distribution
# Author: Jose Antonio Perusquía Cortés
# Afil. : Facultad de Ciencias - UNAM
# Module : Statistical inference
#####################################################################

#####################################################################
# Required libraries 
library(ggplot2)      # Version 3.5.2
library(ggthemes)     # Version 5.1.0
library(dplyr)        # Version 1.1.4
#####################################################################

#####################################################################
# Create 100 confidence intervels with 100 samples of beta(3,1)
x = matrix(nrow=100,ncol=100)

set.seed(314)
for(i in 1:100){
  x[i,]=rbeta(100,3,1)
}

# Point estimate
sumLogX=-apply(log(x),1,sum)
emv = 100/sumLogX

# Upper and lower limits of the confidence intervals
# with significance of alpha=0.05
L=numeric(100)
U=numeric(100)

for(i in 1:100){
  L[i]=qgamma(0.025,100,1)/sumLogX[i]
  U[i]=qgamma(0.975,100,1)/sumLogX[i]
}
#####################################################################

#####################################################################
# Analysis

n=c(1:100)

# Dataframe
res = data.frame(n,emv,L,U)

# Add an indicator variable to know which intervals actually
# contain the true parameter
res = res%>%
  mutate(flag=ifelse(L<3,ifelse(U>3,1,0),0))

# Counts and plot
table(res$flag)

ggplot(data = res,aes(x=n,y=emv,colour=as.factor(flag)))+
  geom_point(show.legend = F)+
  geom_segment(show.legend = F,x=n,xend=n,y=L,yend=U)+
  geom_hline(yintercept = 3,col='red')+
  scale_colour_manual(values=c('black','grey'))+
  theme_minimal()+
  ylim(c(min(L),max(U)))+
  labs(x='',y='')
#####################################################################
