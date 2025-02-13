################################################################################
# Algunas pruebas no paramétricas                                            
# Autor: Jose Antonio Perusquia Cortes
# Afil: Facultad de Ciencias - UNAM
# Curso: Modelos No Paramétricos y de Regresión
################################################################################

################################################################################
# Librerías requeridas
library(ggplot2)        # Versión 3.4.4
library(ggthemes)       # Versión 5.0.0
library(nortest)        # Versión 1.0.4 (Para pruebas de normalidad)
library(DescTools)      # Versión 0.99.56 (Para prueba de rachas)
################################################################################


################################################################################
# Prueba de rachas para aleatoriedad

# Densidad del número de rachas
druns = function(r,n1,n2){
  if(r%%2==0){
    return(2*choose(n1-1,r/2-1)*choose(n2-1,r/2-1)/choose(n1+n2,n1))
  }else{
    return((choose(n1-1,(r-1)/2)*choose(n2-1,(r-3)/2)+
            choose(n1-1,(r-3)/2)*choose(n2-1,(r-1)/2))/choose(n1+n2,n1))
  }
}

# Distribución del número de rachas
pruns = function(r,n1,n2,lower.tail=F){
  if(!lower.tail){
    res=0
    for(i in 2:r){
      res=res+druns(i,n1,n2)
    }
  }else{
    res=0
    for(i in r:(n1+n2)){
      res=res+druns(i,n1,n2)
    }
  }
  return(res)
}


# Prueba de dos colas para probar aleatoriedad vs no aleatoriedad
runs = function(x,val=NULL,exact=F,alpha=.05,alpha.g=F,correct=F){
  
  # Si no se especifica el valor de referencia se utiliza la mediana
  if(is.null(val)){
    aux=x-median(x)
    r=sign(aux)
    
    n1=sum(r==1)
    n2=sum(r==-1)

    no_runs=1
    
    for(i in 2:length(r)){
      if(r[i]!=r[i-1]){
        no_runs=no_runs+1
      }
    }
    
  }else{
    aux=x-val
    r=sign(aux)
    
    n1=sum(r==1)
    n2=sum(r==-1)

    no_runs=1
    
    for(i in 2:length(r)){
      if(r[i]!=r[i-1]){
        no_runs=no_runs+1
      }
    }
    
  }
  
  
  if(!exact){
    
    
    s=c(2:(n1+n2))
    alphaS=0
    
    if(n1==n2){
      
      m=(length(s)-1)/2
      flag=numeric(m)
      
      for(i in 1:m){
        alphaS=alphaS+(2*druns(s[i],n1,n2))
        if(alphaS>=alpha){
          flag[i]=1
          break
        }
      }
      
      if(alpha.g){
        indice=which(flag==1)
        rdown=s[indice]
        rup=s[length(s)-indice+1]
      }else{
        indice=which(flag==1)-1
        rdown=s[indice]
        rup=s[length(s)-indice+1]
        alphaS=alphaS-(2*druns(s[indice+1],n1,n2))
      }

    }else{

    }
    
    L=list('n1'=n1,'n2'=n2,'R'=no_runs,'c1'=rdown,'c2'=rup,'alpha'=alphaS)
    
    
  }else{

  }
  
  return(L)
}

# Ejemplo pesos de las latas de pintura

pesos_latas_pintura = c(68.2,71.6,69.3,71.6,70.4,65.0,63.6,64.7,
                        65.3,64.2,67.6,68.6,66.8,68.9,66.8,70.1)

# Por default regresa la región más chica tal que la significancia es mayor
# a la alpha especificada
res = runs(pesos_latas_pintura);res

# Se puede pedir que regrese la región más grande tal que la significancia 
# es menor a la alpha especificada
res = runs(pesos_latas_pintura,alpha.g=T);res

# La prueba asintótica aún no está funcional

# El caso n1!=n2 aún no está funcional

# Alternativamente se puede resolver con la función RunsTest
RunsTest(pesos_latas_pintura)

################################################################################


################################################################################
# Prueba de signos 

#########################      Prueba de mediana       ######################### 

# Ejemplo de rentas en la Ciudad de México
rentas = c(4,15,14,8.5,15,14,5,19.5,14.9,6.7,7,
          12,6.5,14,9.5,15,15.5,17,28,6,8,20.5)

# Verificar cuales están arriba del valor de referencia de la mediana
x = sum(sign(rentas-16)==1)
n = length(rentas)

# Se utiliza la prueba de la paquetería stats
binom.test(x,n)

# Los intervalos de la prueba son de Clopper - Pearson
qbeta(.025,x,n-x+1)
qbeta(.975,x+1,n-x)

# Si la prueba es de cola izquierda el intervalo inferior es 0 y el superior
binom.test(x,n,alternative = 'less')
qbeta(.95,x+1,n-x)

# Si la prueba es de cola derecha el intervalo superior es 1 y el inferior
binom.test(x,n,alternative = 'greater')
qbeta(.05,x,n-x+1)

#########################    2 muestras apareadas    #########################      

# Prueba de mediana = 0 en las diferencias

# Ejemplo de la sensibilidad a la hipnósis antes y después del tratamiento
antes = c(10,16,7,4,7,2)
despues = c(18,19,11,3,5,3)

x = sum(sign(despues-antes)==1)
n = length(antes)

binom.test(x,n,alternative='greater')

# Como alternativa se tiene la prueba de rangos signados de Wilcoxon
# Ejemplo accidentes antes y después de implementar programa de seguridad
antes=c(51.2,46.5,24.1,10.2,65.3,92.1,30.3,49.2)
despues=c(45.8,41.3,15.8,11.1,58.5,70.3,31.6,35.4)

# A un nivel alpha=.05 se rechaza con signos, y no se rechaza con wilcoxon
binom.test(6,8,alternative = 'g') 
wilcox.test(antes,despues,alternative='greater',paired=T)

################################################################################


################################################################################
# Prueba Mann-Whitney-Wilcoxon para 2 muestras independientes

# Medición de la variable MSCE para gente nativa y caucásicos
nativos = c(8.5,9.48,8.65,8.16,8.83,7.76,8.63)
caucasicos = c(8.27,8.20,8.25,8.14,9,8.1,7.2,8.32,7.7)

# La prueba regresa el estadístico U de Mann Whitney
res = wilcox.test(nativos,caucasicos);res

# El estadístico original de Wilcoxon está dado por
m = length(nativos)
res$statistic+(m*(m+1)/2)

################################################################################


################################################################################
# Prueba de Kruskal Wallis para k muestras independientes

# Tratamientos para un desorden mental
g1 = c(19,22,25,24,29,26,37,23,27,28)   # Electroshocks  
g2 = c(14,21,2,6,10,16,17,11,18,7)      # Psicoterapia
g3 = c(12,1,5,8,4,13,9,15,3,20)         # Electroshocks + Psicoterapia
g4 = c(38,39,40,30,31,32,33,36,34,35)   # Sin tratamiento

tratamientos = list(g1,g2,g3,g4)

# Se calcula el estadístico H y su p-valor
ni=c(10,10,10,10)
Ri_bar = unlist(lapply(tratamientos,sum))/ni;Ri_bar
N=sum(ni)

H=sum(12*ni*(Ri_bar-((N+1)/2))^2/(N*(N+1)));H
p_value=pchisq(H,df=3,lower.tail=F);p_value

#Alternativamente, se puede comparar con el percentil 1-alpha de la 
# distribución ji-cuadrada de 3 grados de libertad
qchisq(.95,3)


# En R se utiliza el comando kruskal.test el cual recibe una lista con las
# observaciones por grupo
kruskal.test(tratamientos)

################################################################################


################################################################################
# Prueba de Friedman para k muestras apareadas

# Ejemplo de preguntas por salón de diferentes tamaños
p1 = c(14,19,17,17,16,15,18,16)
p2 = c(23,25,22,21,24,23,26,22)
p3 = c(26,25,29,28,28,27,27,30)
p4 = c(30,33,28,27,32,36,26,32)

datos = cbind(p1,p2,p3,p4)

friedman.test(datos)

# Se compara contra una ji cuadrada de k-1 grados de libertad
qchisq(.95,3)

################################################################################


################################################################################
# Medidas de asociación

# Ejemplo Gibbons pag. 440.
juez1 = c(1,5,9,7,4,6,8,2,3)
juez2 = c(4,3,6,8,2,7,9,1,5)

#########################  Correlación de Pearson     #########################
cor.test(juez1,juez2)

# Los intervalos se obtienen con la transformación Z de Fisher donde
# z=.5*log((1+r)/(1-r)) ~ N(0,1/(n-3))
r_p = cor(juez1,juez2)
z = .5*log((1+r_p)/(1-r_p))
n = length(juez1)

z_inf = z-qnorm(.975)/sqrt(n-3)
z_sup = z+qnorm(.975)/sqrt(n-3)

r_inf = (exp(2*z_inf)-1)/(exp(2*z_inf)+1);r_inf
r_sup = (exp(2*z_sup)-1)/(exp(2*z_sup)+1);r_sup


#########################       tau de Kendall        #########################

# con p valor exacto si n<50 y sin empates, de otra forma
# se usa teoría asintótica para que se distribuya normal estándar
cor.test(juez1,juez2,method='k')

# Si se puede asumir normalidad se pueden crear intervalos de confianza
# con la transformación Z de Fisher (Bonett and Wright,2000) aunque existen
# otras formas de crear los intervalos (e.g. teoría as)
r_k = cor(juez1,juez2,method='k')
z = .5*log((1+r_k)/(1-r_k))
n = length(juez1)

z_inf = z-qnorm(.975)*sqrt(.437/(n-4))
z_sup = z+qnorm(.975)*sqrt(.437/(n-4))

r_inf = (exp(2*z_inf)-1)/(exp(2*z_inf)+1);r_inf
r_sup = (exp(2*z_sup)-1)/(exp(2*z_sup)+1);r_sup


#########################       rho de Spearman        #########################
# con p-valor usando algoritmo AS 89 si n<1290, de otra forma
# se puede obtener mediante la aproximación t 
cor.test(juez1,juez2,method='s',exact = T)
cor.test(juez1,juez2,method='s',exact = F)

r_s=cor(juez1,juez2,method='s')
tstat = r_s*(sqrt((n-2)/(1-r_s^2)));tstat
2*pt(tstat,7,lower.tail = F)

# Si se puede asumir normalidad se pueden crear intervalos de confianza
# con la transformación Z de Fisher (Bonett and Wright, 2000) aunque existen
# otras formas de crear los intervalos

r_s = cor(juez1,juez2,method='s')
z = .5*log((1+r_s)/(1-r_s))
n = length(juez1)

z_inf = z-qnorm(.975)*sqrt((1+(r_s^2/2))/(n-3))
z_sup = z+qnorm(.975)*sqrt((1+(r_s^2/2))/(n-3))

r_inf = (exp(2*z_inf)-1)/(exp(2*z_inf)+1);r_inf
r_sup = (exp(2*z_sup)-1)/(exp(2*z_sup)+1);r_sup


################################################################################


################################################################################
# Prueba ji cuadrada

#########################   Tablas de contingencia     #########################

# Ejemplo de tipo de vacunas contra enfermedad o no
x = matrix(c(43,237,52,198,25,245,48,212,57,233),nrow=5,byrow=T)
N = sum(x)

# Probabilidades marginales
p_row = apply(x,1,sum)/N
p_col = apply(x,2,sum)/N

# Bajo Ho las probabilidades son el producto de las marginales
probs = matrix(nrow=5,ncol=2)

probs[,1] = p_col[1]*p_row
probs[,2] = p_col[2]*p_row

# El estadístico de prueba
esperados = probs*N
Q = sum(((x-esperados)^2)/esperados);Q

# Coeficiente de contingencia
C = (Q/(Q+N))^(1/2);C

# Coeficiente phi
phi = sqrt(Q/N);phi

# Se compara contra un cuantil de una ji-cuadrada
qchisq(.95,4)

# Equivalentemente existe la función chisq.test
chisq.test(x)


################ Prueba exacta de Fisher (muestras pequeñas)  #################

# Ejemplo de la señora del té

x = matrix(c(3,1,1,3),byrow=T,nrow=2)
fisher.test(x,alternative='greater')


################  Bondad de ajuste  ################# 

# Ejemplo de número de defectos para 0,1,2,3,4,5,6 o más
defectos = c(0,1,2,3,4,5,6)
no_obs = c(10,24,10,4,1,1,0)

# Se estima la media
xbarra = sum(defectos*no_obs)/sum(no_obs)

# Se juntan las últimas celdas
defectos = c(0,1,2,3,4,5)
no_obs = c(10,24,10,4,1,1)

# Para la distribución Poisson
probs = numeric(length(defectos))
for(i in 1:(length(defectos))){
  probs[i] = dpois(defectos[i],xbarra)
}
probs[6] = ppois(4,xbarra,lower.tail = F)
sum(probs)

# Valores esperados
esperados = probs*sum(no_obs);esperados

# Se juntan las dos últimas celdas debido a que hay un esperado menor a 1
defectos = c(0,1,2,3,4)
no_obs = c(10,24,10,4,2)

# Para la distribución Poisson
probs=numeric(length(defectos))
for(i in 1:(length(defectos))){
  probs[i] = dpois(defectos[i],xbarra)
}
probs[5] = ppois(3,xbarra,lower.tail = F)
sum(probs)

# Valores esperados
esperados = probs*sum(no_obs);esperados

# Estadístico de prueba
Q = sum(((no_obs-esperados)^2)/esperados);Q

# Comparamos contra una ji cuadrada de 3 grados de libertad
qchisq(.95,3)
################################################################################


################################################################################
# Prueba de McNemar

# Ejemplo del dolor en pacientes con cáncer May y Johnson (1997)
datos = matrix(c(18,4,12,5),nrow=2,byrow=T)

# Correct=True regresa la corrección de Yates
mcnemar.test(datos)
mcnemar.test(datos,correct=F)

################################################################################


################################################################################
# Prueba de Kolmogorov - Smirnov
pesos = c(156,162,168,182,186,190,190,196,202,210,
          214,220,226,230,230,236,236,242,246,270)

locations=unique(pesos);locations
jumps=numeric(length(locations));jumps

for(i in 1:length(jumps)){
  jumps[i]=sum(pesos==locations[i])/length(pesos)
}

jumps=cumsum(jumps)

df=data.frame(x=locations,y=jumps)

locations=c(min(locations)-10,locations)
jumps=c(0,jumps)

p=ggplot(data=df,aes(x=x,y=y))+
  geom_point()+
  geom_segment(aes(x=locations[-1],xend=locations[-18],
                   y=jumps[-18],yend=jumps[-18]))+
  theme_minimal()

p

x=seq(min(locations),max(locations)+10,by=.1)
Fx=pnorm(x,mean=200,sd=sqrt(1225))
df1=data.frame(x=x,y=Fx)

p+geom_line(data=df1,aes(x=x,y=y),col='red')


# Prueba de KS

Fn=seq(0,1,by=1/20)
Fx=pnorm(pesos,mean=200,sd=sqrt(1225))

Dn1=max(Fn[-1]-Fx);Dn1
Dn2=max(Fx-Fn[-21]);Dn2

ks.test(pesos,'pnorm',200,sqrt(1225))


################################################################################


################################################################################
# Pruebas de normalidad

# Métodos gráficos

# Histograma
df=data.frame(pesos)

ggplot(data=df,aes(x=pesos))+
  geom_histogram(breaks=hist(pesos,plot=F)$breaks,col='black',fill='lightblue')+
  theme_minimal()+
  labs(x='',y='')

# qqplot
emp_quantiles=quantile(pesos,probs=seq(0.01,.99,length.out = length(pesos)))
teo_quantiles=qnorm(seq(.025,.975,length.out = length(pesos)))

x1=qnorm(.25)
x2=qnorm(.75)
y1=quantile(pesos,.25)
y2=quantile(pesos,.75)

m=(y2-y1)/(x2-x1)
b=y1-m*x1

quantiles=data.frame(emp_quantiles,teo_quantiles)
names(quantiles)=c('Empirical','Theoretical')

q=ggplot(quantiles,aes(x=Theoretical,y=Empirical))+
  geom_point(col='black',size=2)+
  geom_abline(intercept = b ,slope =m,col="red")+
  labs(title='',x='Cuantiles Teóricos',y='Cuantiles Empíricos')+
  theme_minimal()+
  theme(axis.title =element_text(size=8))

q

# Lilliefors 
lillie.test(pesos)

# Anderson - Darling
ad.test(pesos)

# Cramer - von Mises 
cvm.test(pesos)

################################################################################