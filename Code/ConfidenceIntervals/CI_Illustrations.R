#####################################################################
# Illustrations for confidence intervals
# with known variance
# Autor: Jose Antonio Perusquía Cortés
# Afil : Facultad de Ciencias - UNAM
# Curso : Inferencia Estadística
#####################################################################

#####################################################################
# Required libraries 
library(ggplot2)      # Version 3.5.2
library(ggthemes)     # Version 5.1.0
library(dplyr)        # Version 1.1.4
#####################################################################

#####################################################################
# Symmetric distribution
x=seq(-3,3,by=.01)
fx=dnorm(x)

df = data.frame(x,fx)
ggplot(df,aes(x=x,y=fx))+
  geom_line()+
  theme_minimal()+
  geom_segment(x=qnorm(.025),y=0,xend=qnorm(.025),
               yend=dnorm(qnorm(.025)),col='red',
               linewidth=.5)+
  geom_segment(x=qnorm(.975),y=0,xend=qnorm(.975),
               yend=dnorm(qnorm(.975)),col='red',
               linewidth=.5)+
  annotate("text",x=0,y=.15,label=expression(1-alpha),
           size=10)+
  annotate("text",x=-2.3,y=.015,label=expression(alpha/2),
           size=4)+
  annotate("text",x=2.25,y=.015,label=expression(alpha/2),
           size=4)+
  annotate("text",x=qnorm(.025),y=-0.025,label=expression(theta[L]),
           size=5)+
  annotate("text",x=qnorm(.975),y=-0.025,label=expression(theta[U]),
           size=5)+
  labs(x='',y='')+
  theme(axis.text = element_blank())
#####################################################################

#####################################################################
# Quantile function for continuous random variable
x=seq(-3,3,by=.01)
fx=pnorm(x)
df=data.frame(x,fx)

ggplot(df,aes(x=x,y=fx))+
  geom_line()+
  theme_minimal()+
  labs(x='',y='')+
  geom_segment(x=qnorm(0),xend=qnorm(0.25),y=0.25,
               yend=0.25,col='red')+
  geom_segment(x=qnorm(0.25),xend=qnorm(0.25),y=pnorm(qnorm(0.25)),
               yend=qnorm(0.25),col='red')
#####################################################################

#####################################################################
# Quantile function for discrete distributions
x=c(0:10)
fx=pbinom(x,prob=.5,size=10)
df=data.frame(x,fx)

x_in = x[-11]
x_end = x[-1]
y_in = fx[-11]
y_end = fx[-11]

df1=data.frame(x_in,x_end,y_in,y_end)

ggplot(df,aes(x=x,y=fx))+
  geom_point()+
  geom_segment(data=df1,aes(x=x_in,xend=x_end,y=y_in,yend=y_end))+
  theme_minimal()+
  labs(x='',y='')+
  geom_hline(yintercept=0.25,col='red')

# The quantile for 0.25 is 4 but it accumulates much more mass
qbinom(.25,10,.5)
pbinom(4,10,.5)
#####################################################################