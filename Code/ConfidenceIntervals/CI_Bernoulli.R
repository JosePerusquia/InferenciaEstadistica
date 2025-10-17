#####################################################################
# Confidence interval for Bernoulli(theta)
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
# Distribution as a function of theta for fixed n and x
theta=seq(0,1,by=.01)
FT=pbinom(4,10,theta)

df=data.frame(theta,FT)
ggplot(data=df,aes(x=theta,y=FT))+
  geom_line()+
  theme_minimal()+
  labs(x=expression(theta),y='')
#####################################################################

#####################################################################
# Function to obtain the roots 
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
#####################################################################

#####################################################################
# Create 100 confidence intervels with 100 samples of Ber(.7)
x = matrix(nrow=100,ncol=100)

set.seed(314)
for(i in 1:100){
  x[i,]=rbinom(100,1,.7)
}

# Point estimate
emv=apply(x,1,mean)

# Upper and lower limits of the confidence intervals
# with significance of alpha=0.05
L=numeric(100)
U=numeric(100)

for(i in 1:100){
  res=ConfInt(x[i,],.025,.025)
  L[i]=res[1]
  U[i]=res[2]
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