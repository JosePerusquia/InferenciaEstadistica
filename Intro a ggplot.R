##########################################################################
# Introduccion a ggplot2                                              
# Autor: Jose Antonio Perusquia Cortes
# Afil : Facultad de Ciencias - UNAM
# Curso : Inferencia estadística
# Semestre: 2025 - I
##########################################################################

##########################################################################
# Librerías (se tienen que instalar)
library(ggplot2)
library(ggthemes) #descarga diferentes tipos de temas a utilizar  
##########################################################################

##########################################################################
# La idea es que una grafica se puede dividir en 3 partes:
#             plot=data+aesthetics+geometry
# data lo recibe en forma de un data frame
# Aesthetics le indica cuales son las variables x, y ademas de controlar el 
# color, tamaño, forma de los puntos, altura de las barras, etc.
# Geometry indica el tipo de gráfica (e.g. histograma, box plot, etc.)
##########################################################################


##########################################################################
# Un primer histograma
set.seed(3.14159)
x=rnorm(10000)
x=data.frame(x)

# Un primer histograma usando las frecuencias
ggplot(x,aes(x=x))+
  geom_histogram()

# Modificar el tema, el color de las barras, el fondo, las etiquetas
ggplot(x,aes(x=x))+
  geom_histogram(stat="bin",color="black",fill="lightblue")+
  labs(x='',y='Frecuencias',title='Mi primer histograma')+
  theme_minimal()

# Añadir la densidad gaussiana
t=seq(-4,4,by=.01)
ft=dnorm(t)
dft=data.frame(t,ft)

ggplot(x,aes(x=x,y=..density..))+
  geom_histogram(stat="bin",color="black",fill="lightblue")+
  geom_line(data=dft,aes(x=t,y=ft),col="red")+
  labs(x='',y='Densidad')+
  theme_minimal()

# Graficar la densidad empírica
ggplot(x,aes(x=x))+
  geom_density()+
  labs(x='',y='Densidad')+
  theme_minimal()

# Otra forma de hacerlo es con stat_density
ggplot(x,aes(x=x))+
  stat_density()+
  labs(x='',y='Densidad')+
  theme_minimal()
##########################################################################

##########################################################################
# base de datos mtcars 
data(mtcars)
df=mtcars[,c("mpg","cyl","wt")]

df$cyl=as.factor(df$cyl)
head(df)

# Diagrama de dispersión de wt contra mpg
ggplot(df,aes(x=wt,y=mpg))+
  geom_point()+
  theme_minimal()

# Podemos cambiar el tamaño y forma de los puntos
ggplot(mtcars,aes(x=wt,y=mpg))+
  geom_point(size=4,col="blue",shape=2)+
  theme_minimal()
##########################################################################

##########################################################################
# Peso de las personas
set.seed(314159)
wdata=data.frame(
  sex=factor(rep(c("F","M"),each=200)),
  weight=c(rnorm(200,55),rnorm(200,60)))

# Graficar la densidad empírica
ggplot(wdata,aes(x=weight))+
  geom_density(col="blue")+
  labs(x='peso',y='densidad')+
  theme_minimal()

# Colorear regiones debajo de la densidad
dat=with(density(wdata$weight),data.frame(x,y))
ggplot(data=dat,mapping=aes(x=x,y=y))+
  geom_line()+
  geom_area(mapping=aes(x=ifelse(x>58&x<59,x,0)),fill="red")+
  xlim(45,70)


# Podemos guardar una gráfica para modificarla a nuestro antojo
y=ggplot(wdata,aes(x=weight))

# Densidad empírica separada
y+geom_density(aes(color=sex))

# Otros parámetros para modificar: alpha, color, fill, linetype, size

# Con alpha bajamos la intensidad de los colores
means=data.frame(mu=c(mean(wdata$weight[which(wdata$sex=="F")]),
                      mean(wdata$weight[which(wdata$sex=="M")])),
                 sex=c("F","M"))

p=ggplot(wdata,aes(x=weight,fill=sex))+
  geom_density(alpha=.5)+
  theme_linedraw()+
  geom_vline(data=means,aes(xintercept=mu,color=sex),linetype="dashed");p


# Se pueden elegir otros colores
p+scale_fill_manual(values=c("red","blue"))
p+scale_fill_brewer(palette="Dark2")
p+scale_fill_grey()

# Modificar la posición de la leyenda top, bottom, left, right none
p+theme(legend.position="left")
p+theme(legend.position="bottom")
p+theme(legend.position="right")
p+theme(legend.position="none")




# Colores de relleno
y+geom_density(aes(fill=sex),alpha=.4)+
  xlim(min(wdata$weight)-1,max(wdata$weight)+1)+
  geom_vline(data=means,aes(xintercept=mu,color=sex),linetype="dashed")+
  scale_color_manual(values=c("red","blue"))

# Seperarlars en paneles
ggplot(wdata,aes(x=weight))+
  geom_density()+facet_grid(sex~.)+
  geom_vline(data=means,aes(xintercept=mu,color="red"),
             linetype="dashed")

# Histogramas
y+geom_histogram()
y+geom_histogram(alpha=.5,binwidth = .1,aes(color=sex),fill="white",position = "identity")+
  theme_linedraw()+
  theme(legend.position="top")

# Histogramas y gráficas de densidad
ggplot(wdata,aes(x=weight))+
  geom_histogram(binwidth =.1,aes(y=..density..),color="black",fill="white")+
  geom_density(alpha=.2,fill="lightblue")+
  xlim(50,64)+
  theme_void()+
  labs(x="",y="")

# Cololearlas por genero
ggplot(wdata,aes(x=weight,color=sex,fill=sex))+
  geom_histogram(binwidth =.1,aes(y=..density..),alpha=.5,position="identity")+
  geom_density(alpha=.2)+
  xlim(50,64)


# Diagrama de caja y bigotes
ggplot(wdata, aes(weight)) +
  geom_boxplot(outlier.alpha=.75)+
  theme_minimal()+
  labs(x="Peso (kg)")+
  ylim(-.75,.75)

# Diagrama de caja y bigotes seperada por género
ggplot(wdata, aes(x=weight,color=sex)) +
  geom_boxplot(outlier.alpha=.75)+
  theme_minimal()+
  ylim(-1,1)


# Función de distribución
y+stat_ecdf(aes(color=sex))

##############################################################################


##############################################################################
# Datos categóricos

# Toothgrowth data
ToothGrowth$dose=as.factor(ToothGrowth$dose)

# Gráfica de barras
df=data.frame(dose=c("D0.5","D1","D2"),
              len=c(4,10,29))

b=ggplot(df,aes(x=dose,y=len))
b+geom_bar(stat="identity",width=.5,color="blue",fill="white")+
  theme_linedraw()

# Introducir los conteos
ggplot(data=df,aes(x=dose,y=len))+
  geom_bar(stat="identity",fill="steelblue")+
  geom_text(aes(label=len),vjust=2.3,size=5)+
  theme_minimal()

# Usando la variable de llenado para usar diferentes colores
b=ggplot(df,aes(x=dose,y=len,fill=dose))
b+geom_bar(stat="identity",width=.5)+
  theme_linedraw()+
  labs(x="",y="",fill="Dosis (mg)")

# Separar las barras usando otras categorías
df2=data.frame(supp=rep(c("VC","OJ"),each=3),
               dose=rep(c(.5,1,2),2),
               len=c(6.8,15,33,4.2,10,29.5))

# Grafíca de barras acumulada
ggplot(data=df2,aes(x=dose,y=len,fill=supp))+
  geom_bar(stat="identity")

# Gráfica de barras separadas
ggplot(data=df2,aes(x=dose,y=len,fill=supp))+
  geom_bar(stat="identity",color="black",width=.3,position=position_dodge())+
  scale_fill_brewer(palette="Blues")+
  geom_text(aes(label=len),vjust=2.3,size=3.5,position=position_dodge(width=.3))+
  theme_linedraw()


# Gráfica de pie
data <- data.frame(
  category=c("D0.5","D1","D2"),
  count=c(4, 10, 29)
)

# Obtener porcentajes
data$fraction <- data$count / sum(data$count)

# Obtener porcentajes acumulados
data$ymax <- cumsum(data$fraction)

# Obtener el fondo de cada rectángulo
data$ymin <- c(0, head(data$ymax, n=-1))

# Obtener la posicion de las lyendas
data$labelPosition <- (data$ymax + data$ymin) / 2

# Se obtienen las etiquetas
data$label <- paste0(data$count)

# Se hace la gráfica
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label(show.legend=F,x=3.5, aes(y=labelPosition, label=label), size=3.5) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  labs(x="",y="",fill="Dosis (mg)")
##############################################################################




