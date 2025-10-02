#####################################################################
# Confidence interval for Exp(lambda)
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
# Distribution of pivotal quantity is gamma(n,1) (it is skewed)
x=seq(0,25,by=.01)
fx=dgamma(x,10,1)

df=data.frame(x,fx)

p=ggplot(data=df,aes(x=x,y=fx))+
    geom_line()+
    labs(x='',y='')+
    theme_minimal()
plot(p)
#####################################################################

#####################################################################
# Create 100 confidence intervals with 100 samples of exp(2) with
# alpha = 0.05 and the gamma .025 and .975 quantiles
x = matrix(nrow=100,ncol=10)

set.seed(314)
for(i in 1:100){
  x[i,]=rexp(10,2)
}

# Point estimate
xbarra=apply(x,1,mean)

# Plot of the interval for Q
p+geom_segment(x=qgamma(.025,10,1),xend=qgamma(.025,10,1),
               y=0,yend=dgamma(qgamma(.025,10,1),10,1),col='red')+
  geom_segment(x=qgamma(.975,10,1),xend=qgamma(.975,10,1),
               y=0,yend=dgamma(qgamma(.975,10,1),10,1),col='red')

# Upper and lower limits of the confidence intervals
L=numeric(100)
U=numeric(100)

for(i in 1:100){
  L[i]=qgamma(.025,10,1)/(xbarra[i]*10)
  U[i]=qgamma(.975,10,1)/(xbarra[i]*10)
}

# Analysis
n=c(1:100)

# Dataframe
res = data.frame(n,y=1/xbarra,L,U)

# Add an indicator variable to know which intervals actually
# contain the true parameter
res = res%>%
  mutate(flag=ifelse(L<2,ifelse(U>2,1,0),0))

# Counts and plot
table(res$flag)

ggplot(data = res,aes(x=n,y=y,colour=as.factor(flag)))+
  geom_point(show.legend = F)+
  geom_segment(show.legend = F,x=n,xend=n,y=L,yend=U)+
  geom_hline(yintercept = 2,col='red')+
  scale_colour_manual(values=c('black','grey'))+
  theme_minimal()+
  ylim(c(min(L),max(U)))+
  labs(x='',y='')
#####################################################################

#####################################################################
# Create 100 confidence intervals with 100 samples of exp(2) with
# alpha = 0.05 using the shortest interval
alpha=0.05
n=10
tol = .00000001

# First method
fn = function(c,n){
  return(qgamma(1-alpha+c,n,1)-qgamma(c,n,1))
}

c_star1 = optimize(fn,n=n,interval=c(0.0001,0.05),
                   maximum=FALSE,tol=tol)$minimum

u = qgamma(1-alpha+c_star1,n,1)
l = qgamma(c_star1,n,1)

# Check that the coverage is actually .95
pgamma(u,n,1)-pgamma(l,n,1)

# Second method
mode=n-1

fx_c = function(x,n,c){
  return(dgamma(x,n,1)-c)
}

root_fx_c = function(c,n){
  mode = n-1
  left_root = uniroot(fx_c,n=n,c=c,lower = 0,tol=tol,
                      upper = mode)$root
  upper_bound = 1000
  right_root = uniroot(fx_c,n=n,c=c,lower=mode,tol=tol,
                       upper=upper_bound)$root
  return(c(left_root,right_root))
}

target = function(c,n,alpha){
  roots=root_fx_c(c,n)
  return(pgamma(roots[2],n,1)-pgamma(roots[1],n,1)+alpha-1)
}

c_star2 = uniroot(target,n=n,alpha=alpha,tol=tol,
                 lower = 0.00000000000001, 
                 upper = dgamma(mode,n,1))$root

l = uniroot(fx_c,n=10,c=c_star2, lower = 0, upper = n-1,
            tol=tol)$root
u = uniroot(fx_c,n=10,c=c_star2, lower = mode, upper = 1000,
            tol=tol)$root

# Graphically we are doing
p+geom_hline(yintercept = dgamma(u,10,1),col='red')
p+geom_segment(y=dgamma(u,10,1),yend=dgamma(u,10,1),x=l,xend=u,
               col='red')+
  geom_segment(x=l,xend=l,y=0,yend=dgamma(u,10,1),col='red')+
  geom_segment(x=u,xend=u,y=0,yend=dgamma(u,10,1),col='red')

# Upper and lower limits of the confidence intervals
L=numeric(100)
U=numeric(100)

for(i in 1:100){
  L[i]=l/(xbarra[i]*10)
  U[i]=u/(xbarra[i]*10)
}

# Analysis
n=c(1:100)

# Dataframe
res = data.frame(n,y=1/xbarra,L,U)

# Add an indicator variable to know which intervals actually
# contain the true parameter
res = res%>%
  mutate(flag=ifelse(L<2,ifelse(U>2,1,0),0))

# Counts and plot
table(res$flag)

ggplot(data = res,aes(x=n,y=y,colour=as.factor(flag)))+
  geom_point(show.legend = F)+
  geom_segment(show.legend = F,x=n,xend=n,y=L,yend=U)+
  geom_hline(yintercept = 2,col='red')+
  scale_colour_manual(values=c('black','grey'))+
  theme_minimal()+
  ylim(c(min(L),max(U)))+
  labs(x='',y='')
#####################################################################