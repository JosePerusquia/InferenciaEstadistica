################################################################################
# Intervalo de confianza para la media de una distribución normal con varianza
# conocida
# Autor: Jose Antonio Perusquía Cortés
# Afil : Facultad de Ciencias - UNAM
# Curso : Inferencia Estadística
################################################################################

################################################################################
# Librerías
library(ggplot2)
library(ggthemes)
library(dplyr)
################################################################################

################################################################################
# Simulamos 100 muestras de tamaño 100 de una distribución normal(2,5)
x = matrix(nrow=100,ncol=100)

set.seed(314)
for(i in 1:100){
  x[i,]=rnorm(100,2,5)
}

# Creamos vectores para guardar los límites inferiores y superiores
L=numeric(100)
U=numeric(100)

# Calculamos la media para cada muestra
xbarra=apply(x,1,mean)

# Calculamos los límites inferiores y superiores.
for(i in 1:100){
  L[i]=xbarra[i]-sqrt(25/100)*qnorm(.975)
  U[i]=xbarra[i]+sqrt(25/100)*qnorm(.975)
}
################################################################################

################################################################################
# Gráficas

# Generamos un vector con entradas del 1 al 100 para usarlos en el eje x 
# al momento de graficar los intervalos
n=c(1:100)

# Se crea un data frame con n, xbarra, L y U
res = data.frame(n,xbarra,L,U)

# Agregamos variable indicadora para saber si el intervalo contiene al valor
# real del parámetro
res = res%>%
  mutate(flag=ifelse(L<2,ifelse(U>2,1,0),0))

# Tabla de frecuencias para saber cuantos contienen al cero.
table(res$flag)

# Graficamos los resultados
ggplot(data = res,aes(x=n,y=xbarra,colour=as.factor(flag)))+
  geom_point(show.legend = F)+
  geom_segment(show.legend = F,x=n,xend=n,y=L,yend=U)+
  geom_hline(yintercept = 2,col='red')+
  scale_colour_manual(values=c('black','grey'))+
  theme_minimal()+
  ylim(c(min(L),max(U)))+
  labs(x='',y='')
################################################################################
