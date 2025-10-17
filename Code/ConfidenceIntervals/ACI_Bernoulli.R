#####################################################################
# Asymptotic confidence interval for Ber(theta) 
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
# Create 100 confidence intervels with 10000 samples of Ber(.7)
n=100
x = matrix(nrow=100,ncol=n)

set.seed(314)
for(i in 1:100){
  x[i,]=rbinom(n,1,.7)
}

# Point estimate
emv=apply(x,1,mean)

# Upper and lower limits of the confidence intervals
# with significance of alpha=0.05
L=numeric(100)
U=numeric(100)

for(i in 1:100){
  L[i]=emv[i]-(qnorm(.975)*sqrt(emv[i]*(1-emv[i])/n))
  U[i]=emv[i]+(qnorm(.975)*sqrt(emv[i]*(1-emv[i])/n))
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
  mutate(flag=ifelse(L<.7,ifelse(U>.7,1,0),0))

# Counts and plot
table(res$flag)

ggplot(data = res,aes(x=n,y=emv,colour=as.factor(flag)))+
  geom_point(show.legend = F)+
  geom_segment(show.legend = F,x=n,xend=n,y=L,yend=U)+
  geom_hline(yintercept = .7,col='red')+
  scale_colour_manual(values=c('black','grey'))+
  theme_minimal()+
  ylim(c(min(L),max(U)))+
  labs(x='',y='')
#####################################################################