################################################################################
# Ejemplo estimación puntual                                      
# Autor: Jose Antonio Perusquia Cortes
# Afil: Facultad de Ciencias - UNAM
# Curso: Inferencia Estadística
################################################################################

################################################################################
# Librerías requeridas
library(ggplot2)        # Versión 3.4.4
library(ggthemes)       # Versión 5.0.0
################################################################################

################################################################################
# Generamos un vector que indican el tamaño de muestra, para cada uno de ellos
# obtendremos dos estimadores para P(X=0) con X v.a. Poisson (lambda)

n=c(1:500)

# El primer estimador será la indicadora de que X_1=0
theta1=numeric(500)

# El segundo estimador es el UMVUE
theta2=numeric(500)

# Iteramos para encontrar el estimador con diferentes tamaños de muestra
for(i in 1:500){
  
  # Generamos la muestra aleatoria
  X=rpois(n[i],lambda = 1)
  
  # Calculamos el primer estimador
  if(X[1]==0){
    theta1[i]=1
  }
  
  # Calculamos el UMVUE
  theta2[i]=((n[i]-1)/n[i])^(sum(X))
  
}

# Para graficar utilizamos ggplot.
df=data.frame(n,theta1,theta2)

ggplot(data=df,aes(x=n,y=theta1))+
  geom_point(size=.5)+
  geom_line(aes(x=n,y=theta2),col='darkblue')+
  geom_hline(yintercept = exp(-1),col='red')+
  labs(x=expression(n),y=expression(hat(theta)))+
  theme_minimal()
################################################################################



