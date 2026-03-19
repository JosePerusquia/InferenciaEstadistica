##################################################################
# Runs test                                           
# Author: Jose Antonio Perusquia Cortes
# Afil: Facultad de Ciencias - UNAM
# Module: Nonparametric statistical inference
##################################################################

##################################################################
# Required libraries
library(ggplot2)        # Versión 3.4.4
library(ggthemes)       # Versión 5.0.0
library(DescTools)      # Versión 0.99.56 (Para prueba de rachas)
##################################################################

##################################################################
# Probability mass function and distribution functions
# of the number of runs

# Probability mass function for the number of runs
# Parameters: 
# r = number of runs
# n1 = number of type 1 objects
# n2 = number of type 2 objects
druns = function(r,n1,n2){
  if(r%%2==0){
    return(2*choose(n1-1,r/2-1)*choose(n2-1,r/2-1)/choose(n1+n2,n1))
  }else{
    return((choose(n1-1,(r-1)/2)*choose(n2-1,(r-3)/2)+
    choose(n1-1,(r-3)/2)*choose(n2-1,(r-1)/2))/choose(n1+n2,n1))
  }
}

# Distribution function for the number of runs
# Parameters: 
# r = number of runs
# n1 = number of type 1 objects
# n2 = number of type 2 objects
# lower.tail = T obtains P(R<=r) otherwise P(R>r)
pruns = function(r,n1,n2,lower.tail=T){
  if(lower.tail){
    res=0
    for(i in 2:r){
      res=res+druns(i,n1,n2)
    }
  }else{
    res=0
    rmax = 2*min(n1,n2) + as.integer(n1 != n2)
    for(i in r:rmax){
      res=res+druns(i,n1,n2)
    }
  }
  return(res)
}

# Number of runs in a sequence
# Parameters:
# x = numeric sequence to be analysed 
# val = reference value, if not provided it uses the median
num_runs = function(x,val=NULL){
  
  if(is.null(val)) {
    val = median(x)
  }
  
  x = x[x != val]
  r = sign(x - val)
  
  # For degenerate cases with r=0 or r=1
  if(length(r) == 0) return(list(n1=0,n2=0,R=0))
  if(length(r) == 1) return(list(n1=sum(r==1),n2=sum(r==-1),R=1))
  
  n1=sum(r==1)
  n2=sum(r==-1)
    
  no_runs=1
    
  for(i in 2:length(r)){
    if(r[i]!=r[i-1]){
      no_runs=no_runs+1
    }
  }
  

  L=list('n1'=n1,'n2'=n2,'R'=no_runs)
  return(L)
}
##################################################################

##################################################################
# Men and women sitting order in the cinema (n1=n2)
x=c(1,1,1,1,0,1,0,0,0,0)
res=num_runs(x,0.5);res
n1=res$n1
n2=res$n2
r =res$R
rmax = 2*min(n1,n2) + as.integer(n1 != n2)

# Distribution for all the possible values of R
sop_R = c(2:rmax);sop_R
pmf_R = sapply(sop_R,druns,n1,n2);pmf_R

df_R = data.frame(x=sop_R,y=pmf_R)
ggplot(data=df_R,aes(x=x,y=y))+
  geom_point()+
  geom_segment(x=sop_R,xend=sop_R,y=0,yend=pmf_R,linetype=2)+
  labs(x='',y='')+
  theme_minimal()
  
# Significance level for the rejection region {2,10}
2*pmf_R[1]

# Significance level for the rejection region {2,3,9,10}
2*sum(pmf_R[1:2])

# Runs test in DescTools and how it obtains p-value
RunsTest(x,exact = T)
pval = 2*pruns(4,n1,n2);pval
##################################################################

##################################################################
# Men and women sitting order in the cinema (n1!=n2)
x=c(1,0,0,0,0,1,0,0,0,0,0,0,0,0,1)
res=num_runs(x,0.5);res
n1=res$n1
n2=res$n2
r =res$R
rmax = 2*min(n1,n2) + as.integer(n1 != n2)

# Distribution for all the possible values of R
sop_R = c(2:rmax);sop_R
pmf_R = sapply(sop_R,druns,n1,n2);pmf_R

df_R = data.frame(x=sop_R,y=pmf_R)
ggplot(data=df_R,aes(x=x,y=y))+
  geom_point()+
  geom_segment(x=sop_R,xend=sop_R,y=0,yend=pmf_R,linetype=2)+
  labs(x='',y='')+
  theme_minimal()

# p-value
sum(pmf_R[which(pmf_R<=druns(r,n1,n2))])

# Runs test in DescTools and how it obtains the p-value
RunsTest(x,exact = T)

mu_r = 1+(2*n1*n2/(n1+n2))
extreme = abs(sop_R - mu_r) >= abs(r - mu_r)
sum(pmf_R[extreme])
##################################################################

##################################################################
# Paint cans 
weights = c(69.60, 71.50, 73.08, 70.68, 71.16, 70.59, 70.95, 
            66.62, 74.47, 71.11, 71.15, 69.38, 72.44, 69.93,
            65.93, 68.09, 71.03, 68.55, 70.75, 70.34, 72.93,
            66.42, 71.81, 70.36, 67.94, 69.61, 70.32, 73.11,
            70.94, 68.31)
res=num_runs(weights);res
n1=res$n1
n2=res$n2
r =res$R
rmax = 2*min(n1,n2) + as.integer(n1 != n2)

# Distribution for all the possible values of R
sop_R = c(2:rmax);sop_R
pmf_R = sapply(sop_R,druns,n1,n2);pmf_R

df_R = data.frame(x=sop_R,y=pmf_R)
ggplot(data=df_R,aes(x=x,y=y))+
  geom_point()+
  geom_segment(x=sop_R,xend=sop_R,y=0,yend=pmf_R,linetype=2)+
  labs(x='',y='')+
  theme_minimal()

# Runs test in DescTools
RunsTest(weights,exact=F)

# Normal approximation without correction
mu_r = 1+(2*n1*n2/(n1+n2))
var_r = 2*n1*n2*((2*n1*n2)-n1-n2)/(((n1+n2)^2)*(n1+n2-1))
z = (r-mu_r)/sqrt(var_r);z

# p-value without correction
2*min(pnorm(z,lower.tail=F),pnorm(z,lower.tail=T))
RunsTest(weights,exact=F,correct=F)

# Normal approximation with correction of -.5 since r > mu_r 
# if r < mu_r we should add .5
z = (r-.5-mu_r)/sqrt(var_r);z

# p-value with correction
2*min(pnorm(z,lower.tail=F),pnorm(z,lower.tail=T))
RunsTest(weights,exact=F,correct=T)
##################################################################