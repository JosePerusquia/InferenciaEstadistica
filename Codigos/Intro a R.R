##########################################################################
# Introduccion a R                                              
# Autor: Jose Antonio Perusquia Cortes
# Afil : Facultad de Ciencias - UNAM
# Curso : Inferencia estadística
# Semestre: 2025 - I
##########################################################################

##########################################################################
# Operaciones aritméticas                                           
2+2
3-1
3/4
3*4
3^2

sqrt(4)
exp(2)
log(2) 
log(2,base=5) 

min(c(1,2))
max(c(1,2))

floor(4.5)
ceiling(4.5)

round(3.1415926)

sin(pi/2)
cos(0)
tan(pi)

asin(0)
acos(0)
atan(0)

sinh(pi)
cosh(pi)
tanh(pi)

asinh(0)
acosh(1)
atanh(0)
##########################################################################

##########################################################################
# Operaciones logicas                                           

2<2
2<=2
2>2
2>=2

3< 4 && 3>5
3< 4 || 3>5
##########################################################################


##########################################################################
# Estructuras de datos                                          

##########################################################################
# Vectores                                                      
a=c(1,2,3,4)
b=c(5,6,7,8)

# Las operaciones entre vectores las hace entrada por entrada
a+b 
a-b 
a/b
a*b

# Operaciones con escalares y vectores                          
2+a 
2-a;a-2 # Importa el orden 
2/a;a/2 # Importa el orden
2*a

sqrt(a)
a^2
exp(a)
log(a)

# Obtener una entrada de un vector
a[1]
a[2:3]
a[c(1,4)]

# Concatenar vectores para crear uno nuevo
c=c(a,b);c

# Crear vector nulo
z=numeric(10);z 

# Secuencia de numeros
s=seq(1,10,by=1);s     # numeros enteros
s=seq(1,10,by=.5);s    # numeros reales

# Replicar un objeto n veces
r=rep(0,10);r
r=rep(s,2);r

# Ordenar un vector
o=sort(c(1,4,2,5));o                # Creciente
o=sort(c(1,4,2,5),decreasing=T);o   # Decreciente
##########################################################################

##########################################################################
# Matrices                                                      
A=matrix(data=c(1,2,3,4),nrow=2,byrow=T);A
B=matrix(data=c(5,6,7,8),nrow=2,byrow=F);B

# Otra forma de definirlas a partir de concatenación
B=rbind(a,b);B # Unimos vectores como renglones 
C=cbind(a,b);C # Unimos vectores como columnas 

# Matriz transpuesta
t(A)

# Operaciones matriciales
A+B
A-B
A*B   # Multiplican las entradas
A%*%B # Multiplicacion matricial 
A^2   # Se elevan las entradas 

# Operaciones con escalares
2+A
2-A;A-2 
2*A
2/A;A/2
sqrt(A)

# Obtener una entrada, una columna o un renglon
A[2,2]
A[,2]
A[2,]

# Concatenar matrices 
rbind(A,A)    # Por renglon 
cbind(A,A)    # Por columna 

# Matriz diagonal 
D=diag(3);D

# Nombre de renglones y columnas
rownames(A)=c("a","b")
colnames(A)=c("c","d")
A

# Sistemas de ecuaciones
b=diag(2);b

# Obtener la inversa de una matriz
Ainv=solve(A,b)

A%*%Ainv
Ainv%*%A

# vectores y valores propios
eigen(A)
##########################################################################

##########################################################################
# Arreglos                                                      

A=array(dim=c(2,2,4));A
A[,,1]=B

A[1,1,1]
A[1,,1]
A[,1,1]
A[,,1]
##########################################################################

##########################################################################
# Listas                                                       
L=list('x'=c(1:5),'y'=c(1:10),'z'=c('Hola','Mundo'))
L$x
L$y
L$z

val_vec_propios=eigen(B)
val_vec_propios$values
val_vec_propios$vectors

##########################################################################

##########################################################################
# Data frames                                                        
data(iris)

iris$Sepal.Length
iris$Sepal.Width
iris$Petal.Length
iris$Petal.Width
iris$Species


df=data.frame('x'=c(1:5),'y'=c(1:10),'z'=c('Hola','Mundo')) #No es adecuado
##########################################################################


##########################################################################
# Probabilidad                                                  

# De las familias mas conocidas podemos evaluar la densidad, 
# distribucion, encontrar cuantiles y generar muestras aleatorias.

# Para valuar la densidad
dnorm(0,mean=0,sd=1)

# Para valuar la distribucion
pnorm(0,mean=0,sd=1)

# Para encontrar los cuantiles
qnorm(.5,mean=0,sd=1)

# Generar muestra aleatoria
m=rnorm(10000,mean=0,sd=1)
##########################################################################

##########################################################################
# Estadistica descriptiva                                       

##########################################################################
# Medidas de tendencia central

# media
mean(m)

# mediana
median(m)

# Para la moda no existe ninguna función predeterminda, pero se 
# puede definir una función para que obtenga el valor

moda=function(x){
  frec=table(x) # Obtiene la tabla de frecuencias 
  max_frec = max(frec) # Obtiene la frecuencia mas grande
  res = names(frec)[which(frec==max_frec)] # Obtiene la moda
  return(res) # Regresa la moda
}

##########################################################################

##########################################################################
# Medidas de localizacion

# cuantiles
quantile(m,probs = seq(.05,1,by=.1))
##########################################################################

##########################################################################
# Medidas de dispersión

# varianza
var(m)

# desviacion estandar
sd(m)
sqrt(var(m))

# rango
r=max(m)-min(m);r

# rango intercuartilico
IQR=quantile(m,.75)-quantile(m,.25);IQR

# coeficiente de variacion
cv=sd(m)/mean(m);cv
##########################################################################

##########################################################################
# Graficas para valores numéricos                                                     

# Secuencia del 1 al 100 de 1 en 1
t=seq(1,length(m),1)

# Grafica 
plot(t,m,type="l",col="black",main="",xlab="x",ylab="muestra")

# Histograma
hist(m)          # Utiliza las frecuencias
hist(m,freq = F) # Utiliza la densidad

# Diagrama de caja y bigotes
boxplot(m)
##########################################################################

##########################################################################
# Ejemplo estadistica descriptiva con un ejemplo discreto       

datos=c(2,2,0,0,5,8,3,4,1,0,0,7,1,7,1,5,4,0,4,0,1,8,9,7,0,1,7,2,
        5,5,4,0,0,3,3,2,5,1,3,0,1,0,2,4,5,0,5,7,5,1)

# Medidas de tendencia central y dispersion
mean(datos)
median(datos)
moda(datos)

var(datos)
sd(datos)

# Histograma
breaks=seq(-.5,9.5,1)
hist=hist(datos,breaks,right=T)

# Poligono de frecuencias
plot(hist$mids,hist$counts,type="b",ylab="frecuencia",xlab="")

# Grafica de ojivas
frec=cumsum(hist$counts)
plot(hist$mids,frec,type="b",ylab="Frec. Absoluta",xlab="")

# Diagrama de caja
boxplot(datos)

# Grafica de pastel
pie(hist$counts,hist$mids,radius=1)
##########################################################################

##########################################################################
# Ejemplo estadistica descriptiva con un ejemplo categórico       

datos=c(rep('A',10),rep('B',5),rep('C',15))

# Medidas de tendencia central y dispersion
moda(datos)

# Grafica de pastel
pie(c(10,5,15),c('A','B','C'),radius=1)

# Gráfica de barras
barplot(c(10,5,15),names.arg=c('A','B','C'))
##########################################################################