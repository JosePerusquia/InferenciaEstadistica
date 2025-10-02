#####################################################################
# Confidence interval for the mean of normal distribution
# with known variance
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
# Create 100 confidence intervels with 100 samples of N(2,5)
x = matrix(nrow=100,ncol=100)

set.seed(314)
for(i in 1:100){
  x[i,]=rnorm(100,2,5)
}

# Point estimate
xbarra=apply(x,1,mean)

# Upper and lower limits of the confidence intervals
# with significance of alpha=0.05
L=numeric(100)
U=numeric(100)

for(i in 1:100){
  L[i]=xbarra[i]-sqrt(25/100)*qnorm(.995)
  U[i]=xbarra[i]+sqrt(25/100)*qnorm(.995)
}
#####################################################################

#####################################################################
# Analysis

n=c(1:100)

# Dataframe
res = data.frame(n,xbarra,L,U)

# Add an indicator variable to know which intervals actually
# contain the true parameter
res = res%>%
  mutate(flag=ifelse(L<2,ifelse(U>2,1,0),0))

# Counts and plot
table(res$flag)

ggplot(data = res,aes(x=n,y=xbarra,colour=as.factor(flag)))+
  geom_point(show.legend = F)+
  geom_segment(show.legend = F,x=n,xend=n,y=L,yend=U)+
  geom_hline(yintercept = 2,col='red')+
  scale_colour_manual(values=c('black','grey'))+
  theme_minimal()+
  ylim(c(min(L),max(U)))+
  labs(x='',y='')
#####################################################################
