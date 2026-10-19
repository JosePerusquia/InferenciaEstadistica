##################################################################
# Rao-Blackwell unbiased estimation Bernoulli distribution                                    
# Author:  Jose Antonio Perusquia Cortes
# Afil:    Facultad de Ciencias - UNAM
# Module:  Statistical inference
##################################################################

##################################################################
# Libraries
library(ggplot2)        # Version 4.0.2
library(ggthemes)       # Version 5.2.0
library(gganimate)      # Version 1.0.11
library(gifski)         # Version 1.32.0-2
##################################################################

##################################################################
# Unbiased estimation of theta parameter

# True parameter
set.seed(314159)
theta = rbeta(1,1,1)

# Different sample sizes
n=c(1:500)

# First estimator is the unbiased estimator T=Ind(X1=1) and
# the second one is the Rao-Blackwell estimator, that is
# E(T|S) with S= sum X_i the sufficient statistic
theta1=numeric(500)
theta2=numeric(500)

for(i in 1:500){
  X=rbinom(n[i],size=1,prob = theta)
  
  if(X[1]==1){
    theta1[i]=1
  }

  theta2[i]=mean(X)
}

# Plot of the estimators for varying n
df = data.frame(n, theta1, theta2)

p = ggplot(df, aes(x = n)) +
  geom_line(aes(y = theta2), col = "darkblue") +
  geom_point(aes(y = theta1), size = .8) +
  geom_hline(yintercept = theta, col = "darkred") +
  labs(x = expression(n), y = "") +
  theme_minimal()+
  theme(axis.title.x = element_text(size = 11,face='bold'),
        axis.title.y = element_text(size = 11,face='bold'),
        axis.text.x = element_text(size=11,face='bold'),
        axis.text.y = element_text(size=11,face='bold'))
print(p)

ggsave('mse_bernoulli.pdf',width = 6,
       height = 4,device = cairo_pdf)

# Create gif
p1 = p +
  transition_manual(n, cumulative = TRUE)

animate(p1,width = 1000,height = 800,
  fps = 20,duration = 10,renderer = gifski_renderer()
)

# Save as GIF
anim_save("RaoBlacwellBernoulli.gif", 
          animation = last_animation())
##################################################################