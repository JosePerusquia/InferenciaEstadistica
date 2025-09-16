##################################################################
# Point estimation                                      
# Author:  Jose Antonio Perusquia Cortes
# Afil:    Facultad de Ciencias - UNAM
# Module:  Statistical inference
##################################################################

##################################################################
# Libraries
library(ggplot2)        # Versión 3.4.4
library(ggthemes)       # Versión 5.0.0
library(gganimate)      # Version 1.0.11
library(gifski)         # Version 1.32.0-2
##################################################################

##################################################################
# Point estimation of exp(-theta) where Xi ~ Poisson(theta)

# The sample size
n=c(1:500)

# First estimator is the unbiased estimator T=Ind(X1=0) and
# the second one is the Rao-Blackwell estimator, that is
# E(T|S) with S the sufficient statistic
theta1=numeric(500)
theta2=numeric(500)

for(i in 1:500){
  X=rpois(n[i],lambda = 1)
  
  if(X[1]==0){
    theta1[i]=1
  }

  theta2[i]=((n[i]-1)/n[i])^(sum(X))
}

# Plot of the estimators for varying n
df=data.frame(n,theta1,theta2)

p=ggplot(data=df,aes(x=n))+
    geom_line(aes(y=theta2),col='darkblue')+
    geom_point(aes(y=theta1),size=.8)+
    geom_hline(yintercept = exp(-1),col='red')+
    labs(x=expression(n),y='')+
    theme_minimal()
print(p)

# Create gif
p1=p+
  transition_reveal(n)+
  shadow_trail(distance = 0.01)

animate(p1, width = 600, height = 400, fps = 20, duration = 10, 
        renderer = gifski_renderer())

# Save as GIF
anim_save("myFirstRaoBlackwellAnimation.gif", 
          animation = last_animation())
##################################################################