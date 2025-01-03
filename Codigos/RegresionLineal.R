########################################################################
# Regresión lineal                                          
# Autor: Jose Antonio Perusquia Cortes
# Afil : Facultad de Ciencias - UNAM
# Curso : Modelos no Paramétricos y de Regresión
##########################################################################

##########################################################################
# Librerías (se tienen que instalar)
library(ggplot2)
library(ggthemes) 
library(nortest)
library(ellipse)
library(car)
library(lmtest)
library(MASS)
library(here)
library(corrplot)
library(GGally)
##########################################################################

##########################################################################
# Ejemplo con datos simulados para el modelo y=beta0+beta1*x+error
beta0 = .2
beta1 = .3
sigma = .5

# Se obtienen los residuales normales
set.seed(314159)
errores = rnorm(30,sd=sigma)

# La variable predictora son los enteros del 1 al 30
x = c(1:30)

# La variable respuesta
y = beta0+beta1*x+errores;y

# Se genera data frame para guardar los datos y poder graficarlos
df = data.frame(x,y)

# Se grafican los puntos
p=ggplot(data=df,aes(x=x,y=y))+
  geom_point()+
  labs(x='',y='')+
  theme_minimal()
p

# Se añade la recta teórica
p=p+geom_abline(slope=beta1,intercept = beta0,col='red')
p


# Estimación

# Matriz de diseño
X=cbind(rep(1,length(x)),x);X

# Estimadores de los coeficientes
H = solve((t(X)%*%X))%*%t(X)
beta_hat = H%*%y;beta_hat

# Graficamos la recta estimada
y_hat=beta_hat[1]+beta_hat[2]*x^2
p=p+geom_abline(slope=beta_hat[2],intercept = beta_hat[1],col='skyblue')
p

# Residuales
y_hat = X%*%beta_hat
e = y-y_hat

# Estimador de la varianza
SSres = t(e)%*%e;SSres
MSres = SSres/(length(y)-2);MSres
sqrt(MSres)

# Error estándar de los coeficientes de la regresión
Sxx = sum((x-mean(x))^2);Sxx

se_beta1 = sqrt(MSres/Sxx);se_beta1
se_beta0 = sqrt(MSres*((1/length(y))+((mean(x)^2)/Sxx)));se_beta0

# Estadísticos t's para H0: beta_i=0 vs H1: beta_i !=0
t0 = beta_hat[1]/se_beta0;t0
t1 = beta_hat[2]/se_beta1;t1

# Cuantil de la distribución t para probar a un nivel alpha del 5%
# las hipótesis nulas mencionadas arriba. En este ejemplo no se rechaza
# que beta0 sea cero y sí rechazamos para beta1
qt(.975,length(y)-2)


# Prueba F
SSr = sum(((y_hat-mean(y))^2));SSr
F0 = SSr/MSres;F0

# Comparamos contra el cuantil de una distribución F de 1,n-2 grados de lib
# en este caso rechazamos la hipótesis nula de que beta1=0
qf(.95,1,length(y)-2)

# Otras propiedaes 

# 1. La suma de residuales es cero
sum(e)

# 2. La suma de los valores observados y los ajustados es igual
sum(y)
sum(y_hat)

# 3. La regresión pasa por el centroide
centroide = data.frame(x=mean(x),y=mean(y))
p+geom_point(data=centroide,aes(x=x,y=y),col='blue',shape=8,size=4)

# 4. La suma ponderada de las variables predictoras es cero
sum(x*e)

# 5. La suma ponderada de los valores ajustados es cero
sum(y_hat*e)


# Intervalos de confianza

# El intervalo de beta1 no contiene al cero
beta1_U = beta_hat[2]+se_beta1*qt(.975,length(y)-2);beta1_U
beta1_L = beta_hat[2]-se_beta1*qt(.975,length(y)-2);beta1_L

# El intervalo de beta0 contiene al cero
beta0_U = beta_hat[1]+se_beta0*qt(.975,length(y)-2);beta0_U
beta0_L = beta_hat[1]-se_beta0*qt(.975,length(y)-2);beta0_L

# Intervalo para sigma^2
sigma_U = SSres/qchisq(.025,length(y)-2);sigma_U
sigma_L = SSres/qchisq(.975,length(y)-2);sigma_L

# R^2
SSt = sum((y-mean(y))^2);SSt
R2 = 1-(SSres/SSt);R2

# En R la función lm ajusta el modelo de regresión
res=lm(y~x,data=df)
summary(res)
plot(res)

# Región de confianza
reg_con = as.data.frame(ellipse::ellipse(res,npoints=1000))
names(reg_con)=c('beta0','beta1')

ggplot(reg_con,aes(x=beta0,y=beta1))+
  geom_point(size=.1)+
  theme_minimal()+
  labs(x=expression(beta[0]),y=expression(beta[1]))

# Estimación de la respuesta media
x0=seq(min(x),max(x),by=.1)
mu_hat = beta_hat[1]+x0*beta_hat[2]
mu_hat_U = mu_hat + qt(.975,length(y)-2)*sqrt(MSres*((1/length(y))+(((x0-mean(x))^2)/Sxx)))
mu_hat_L = mu_hat - qt(.975,length(y)-2)*sqrt(MSres*((1/length(y))+(((x0-mean(x))^2)/Sxx)))

df=data.frame(x,y)
df1=data.frame(x0,mu_hat,mu_hat_U,mu_hat_L)

p=ggplot(data=df,aes(x=x,y=y))+
  geom_point()+
  geom_line(data=df1,aes(x=x0,y=mu_hat),col='skyblue2')+
  geom_line(data=df1,aes(x=x0,y=mu_hat_U),col='darkblue')+
  geom_line(data=df1,aes(x=x0,y=mu_hat_L),col='darkblue')+
  labs(x='',y='',title='Intervalo de la respuesta media al 95%')+
  theme_minimal()
p

# Nuevas predicciones
x0=seq(min(x),max(x),by=.1)
y0_hat = beta_hat[1]+x0*beta_hat[2]
y0_hat_U = mu_hat + qt(.975,length(y)-2)*sqrt(MSres*(1+(1/length(y))+(((x0-mean(x))^2)/Sxx)))
y0_hat_L = mu_hat - qt(.975,length(y)-2)*sqrt(MSres*(1+(1/length(y))+(((x0-mean(x))^2)/Sxx)))

df=data.frame(x,y)
df1=data.frame(x0,y0_hat,y0_hat_U,y0_hat_L)

p=ggplot(data=df,aes(x=x,y=y))+
  geom_point()+
  geom_line(data=df1,aes(x=x0,y=y0_hat),col='skyblue2')+
  geom_line(data=df1,aes(x=x0,y=y0_hat_U),col='darkblue')+
  geom_line(data=df1,aes(x=x0,y=y0_hat_L),col='darkblue')+
  labs(x='',y='',title='Intervalo de predicción al 95%')+
  theme_minimal()
p
##########################################################################

##########################################################################
# Ejemplo de como la R^2 no mide la adecuación del modelo
x=c(1:30)
y=(x^2)-(8*x)+10
df=data.frame(x,y)

p=ggplot(data=df,aes(x=x,y=y))+
  geom_point()+
  theme_minimal()
p

# Ajustamos la regresión lineal
res=lm(y~x,df)
summary(res)

# Se genera una R^2 de 0.8985 lo cual pareciera una buena medida
# pero claramente el ajuste no es bueno
p+geom_abline(slope=res$coefficients[2],
              intercept = res$coefficients[1],col='skyblue')

# Regresión lineal con término cuadrático
res1=lm(y~I(x^2),df)
summary(res1)

df1=data.frame('x'=x,'y'=res1$coefficients[1]+res1$coefficients[2]*x^2)
p+geom_line(data=df1,aes(x=x,y=y),col='skyblue')
##########################################################################

##########################################################################
# Ejemplo de análisis de residuales y regresión polinomial
datos=read.table(here('Datos/datos.txt'),header = F)
names(datos)=c('y','x')

# Se grafican los datos
p=ggplot(data=datos,aes(x=x,y=y))+
  geom_point()+
  theme_minimal()
p

# Se ajusta el modelo básico
res=lm(y~x,datos)
summary(res)

x = seq(min(datos$x),max(datos$x),by=.1)
y_hat=res$coefficients[1]+res$coefficients[2]*x
ajuste=data.frame(x,y_hat)

p+geom_line(data=ajuste,aes(x=x,y=y_hat),col='red')

# Análisis de residuales
residuales = data.frame(x=res$fitted.values,y=res$residuals)

# Supuestos de normalidad

# histograma
ggplot(data=residuales,aes(x=y))+
  geom_histogram(breaks=hist(res$residuals,plot=F)$breaks,col='black',
                 fill='skyblue2')+
  labs(x='',y='')+
  theme_minimal()

# qqplot
emp_quantiles=quantile(res$residuals,
                       probs=seq(0.01,.99,length.out = length(res$residuals)))
teo_quantiles=qnorm(seq(.025,.975,length.out = length(res$residuals)))

x1=qnorm(.25)
x2=qnorm(.75)
y1=quantile(res$residuals,.25)
y2=quantile(res$residuals,.75)

m=(y2-y1)/(x2-x1)
b=y1-m*x1

quantiles=data.frame(emp_quantiles,teo_quantiles)
names(quantiles)=c('Empirical','Theoretical')

ggplot(quantiles,aes(x=Theoretical,y=Empirical))+
  geom_point(col='black',size=2)+
  geom_abline(intercept = b ,slope =m,col="red")+
  labs(title='',x='Cuantiles Teóricos',y='Cuantiles Empíricos')+
  theme_minimal()+
  theme(axis.title =element_text(size=8))

# Pruebas de normalidad
lillie.test(res$residuals)
ad.test(res$residuals)
cvm.test(res$residuals)


# Varianza constante
ggplot(data=residuales,aes(x=x,y=y))+
  geom_point()+
  labs(x='Fitted values',y='Residuals')+
  geom_hline(yintercept =0,col='red')+
  theme_minimal()

ncvTest(res)

# No correlación
dwtest(res)


# Transformar los datos con Box Cox
bc = boxcox(datos$y ~ datos$x)
lambda = bc$x[which.max(bc$y)];lambda
yT=((datos$y^lambda)-1)/lambda

datosT=data.frame(datos$x,yT)
names(datosT)=c('x','y')

# TransACC against SCT 
p=ggplot(datosT,aes(x=x,y=y))+
  geom_point(col='black',size=1)+
  labs(x='x',y='Transformed y')+
  theme_minimal()
p

# Se ajusta el modelo básico
res=lm(y~x,datosT)
summary(res)

x = seq(min(datosT$x),max(datosT$x),by=.1)
y_hat=res$coefficients[1]+res$coefficients[2]*x
ajuste=data.frame(x,y_hat)

p+geom_line(data=ajuste,aes(x=x,y=y_hat),col='red')

# Análisis de residuales
residuales = data.frame(x=res$fitted.values,y=res$residuals)

# Supuestos de normalidad

# histograma
ggplot(data=residuales,aes(x=y))+
  geom_histogram(breaks=hist(res$residuals,plot=F)$breaks,col='black',
                 fill='skyblue2')+
  labs(x='',y='')+
  theme_minimal()

# qqplot
emp_quantiles=quantile(res$residuals,
                       probs=seq(0.01,.99,length.out = length(res$residuals)))
teo_quantiles=qnorm(seq(.025,.975,length.out = length(res$residuals)))

x1=qnorm(.25)
x2=qnorm(.75)
y1=quantile(res$residuals,.25)
y2=quantile(res$residuals,.75)

m=(y2-y1)/(x2-x1)
b=y1-m*x1

quantiles=data.frame(emp_quantiles,teo_quantiles)
names(quantiles)=c('Empirical','Theoretical')

ggplot(quantiles,aes(x=Theoretical,y=Empirical))+
  geom_point(col='black',size=2)+
  geom_abline(intercept = b ,slope =m,col="red")+
  labs(title='',x='Cuantiles Teóricos',y='Cuantiles Empíricos')+
  theme_minimal()+
  theme(axis.title =element_text(size=8))

# Pruebas de normalidad
lillie.test(res$residuals)
ad.test(res$residuals)
cvm.test(res$residuals)

# Varianza constante
ggplot(data=residuales,aes(x=x,y=y))+
  geom_point()+
  labs(x='Fitted values',y='Residuals')+
  geom_hline(yintercept =0,col='red')+
  theme_minimal()

ncvTest(res)

# No correlación
dwtest(res)

# Se ajusta un modelo polinomial de orden 3
res=lm(datosT$y~poly(datosT$x,3,raw=T),datosT)
summary(res)

x = seq(min(datosT$x),max(datosT$x),by=.1)
y_hat=res$coefficients[1]+res$coefficients[2]*x+
  res$coefficients[3]*x^2+res$coefficients[4]*x^3
ajuste=data.frame(x,y_hat)

p+geom_line(data=ajuste,aes(x=x,y=y_hat),col='red')

# Análisis de residuales
residuales = data.frame(x=res$fitted.values,y=res$residuals)

# Supuestos de normalidad

# histograma
ggplot(data=residuales,aes(x=y))+
  geom_histogram(breaks=hist(res$residuals,plot=F)$breaks,col='black',
                 fill='skyblue2')+
  labs(x='',y='')+
  theme_minimal()

# qqplot
emp_quantiles=quantile(res$residuals,
                       probs=seq(0.01,.99,length.out = length(res$residuals)))
teo_quantiles=qnorm(seq(.025,.975,length.out = length(res$residuals)))

x1=qnorm(.25)
x2=qnorm(.75)
y1=quantile(res$residuals,.25)
y2=quantile(res$residuals,.75)

m=(y2-y1)/(x2-x1)
b=y1-m*x1

quantiles=data.frame(emp_quantiles,teo_quantiles)
names(quantiles)=c('Empirical','Theoretical')

ggplot(quantiles,aes(x=Theoretical,y=Empirical))+
  geom_point(col='black',size=2)+
  geom_abline(intercept = b ,slope =m,col="red")+
  labs(title='',x='Cuantiles Teóricos',y='Cuantiles Empíricos')+
  theme_minimal()+
  theme(axis.title =element_text(size=8))

# Pruebas de normalidad
lillie.test(res$residuals)
ad.test(res$residuals)
cvm.test(res$residuals)

# Varianza constante
ggplot(data=residuales,aes(x=x,y=y))+
  geom_point()+
  labs(x='Fitted values',y='Residuals')+
  geom_hline(yintercept =0,col='red')+
  theme_minimal()

ncvTest(res)

# No correlación
dwtest(res)

# Respuesta media al 95%
MeanRespCI=predict(res,data.frame(datosT$x),interval="confidence",level=.95)
MeanRespCI=data.frame(MeanRespCI[,1],MeanRespCI[,2],MeanRespCI[,3],datosT$y,datosT$x)
names(MeanRespCI)=c('Fit','Lower','Upper','y','x')

ggplot(MeanRespCI,aes(x=x,y=y))+
  geom_point(col='black',size=1)+
  geom_line(aes(x=x,y=Fit),col="red")+
  geom_line(aes(x=x,y=Lower),col="blue",linetype='dashed')+
  geom_line(aes(x=x,y=Upper),col="blue",linetype='dashed')+
  labs(x='x',y='Transformed y',title='95% Confidence Intervals')+
  theme_minimal()+
  theme(plot.title=element_text(size=10),axis.title = element_text(size=8))


# Prediction Intervals 95%
MeanRespPred=predict(res,data.frame(datosT$x),interval="prediction",level=.95)
MeanRespPred=data.frame(MeanRespPred[,1],MeanRespPred[,2],MeanRespPred[,3],datosT$y,datosT$x)
names(MeanRespPred)=c('Fit','Lower','Upper','y','x')

ggplot(MeanRespPred,aes(x=x,y=y))+
  geom_point(col='black',size=1)+
  geom_line(aes(x=x,y=Fit),col="red")+
  geom_line(aes(x=x,y=Lower),col="blue",linetype='dashed')+
  geom_line(aes(x=x,y=Upper),col="blue",linetype='dashed')+
  labs(x='x',y='Transformed y',title='95% Prediction Intervals')+
  theme_minimal()+
  theme(plot.title=element_text(size=10),axis.title = element_text(size=8))

# Mean Response and Confidence Intervals on Original Data
MeanRespOriginalCI=(MeanRespCI*lambda+1)^(1/lambda)
MeanRespOriginalCI=data.frame(MeanRespOriginalCI[,1],MeanRespOriginalCI[,2],MeanRespOriginalCI[,3],datos$y,datos$x)
names(MeanRespOriginalCI)=c('Fit','Lower','Upper','y','x')

ggplot(MeanRespOriginalCI,aes(x=x,y=y))+
  geom_point(col='black',size=1)+
  geom_line(aes(x=x,y=Fit),col="red")+
  geom_line(aes(x=x,y=Lower),col="blue",linetype='dashed')+
  geom_line(aes(x=x,y=Upper),col="blue",linetype='dashed')+
  labs(x='x',y='y',title='')+
  theme_minimal()+
  theme(plot.title=element_text(size=10),axis.title = element_text(size=8))


# Mean Response and Prediction Intervals on Original Data
MeanRespOriginalPred=(MeanRespPred*lambda+1)^(1/lambda)
MeanRespOriginalPred=data.frame(MeanRespOriginalPred[,1],MeanRespOriginalPred[,2],MeanRespOriginalPred[,3],datos$y,datos$x)
names(MeanRespOriginalPred)=c('Fit','Lower','Upper','y','x')

ggplot(MeanRespOriginalPred,aes(x=x,y=y))+
  geom_point(col='black',size=1)+
  geom_line(aes(x=x,y=Fit),col="red")+
  geom_line(aes(x=x,y=Lower),col="blue",linetype='dashed')+
  geom_line(aes(x=x,y=Upper),col="blue",linetype='dashed')+
  labs(x='x',y='y',title='95% Prediction Intervals')+
  theme_minimal()+
  theme(plot.title=element_text(size=10),axis.title = element_text(size=8))
##########################################################################

##########################################################################
# Selección de variables 
data(swiss)

# Graficamos por pares
ggpairs(swiss)

# Selección hacia adelante no considera Examination
res= lm(Fertility~1,swiss)
summary(res)
step.model = stepAIC(res, direction = "forward",
                     scope=list(upper=~Agriculture+Education+Examination+
                                  Catholic+Infant.Mortality))

# Selección hacia atrás (coincide con el de selección hacia adelante)
res= lm(Fertility~.,swiss)
summary(res)
step.model = stepAIC(res, direction = "backward")

# Selección por segmentos (coincide también con los otros métodos)
res= lm(Fertility~1,swiss)
step.model = stepAIC(res, direction = "both",
                     scope=list(upper=~Agriculture+Education+Examination+
                                  Catholic+Infant.Mortality))

# El mejor modelo
res= lm(Fertility~Education+Catholic+Infant.Mortality+Agriculture,swiss)
summary(res)

# Análisis de residuales
residuales = data.frame(x=res$fitted.values,y=res$residuals)

# Supuestos de normalidad

# histograma
ggplot(data=residuales,aes(x=y))+
  geom_histogram(breaks=hist(res$residuals,plot=F)$breaks,col='black',
                 fill='skyblue2')+
  labs(x='',y='')+
  theme_minimal()

# qqplot
emp_quantiles=quantile(res$residuals,
                       probs=seq(0.01,.99,length.out = length(res$residuals)))
teo_quantiles=qnorm(seq(.025,.975,length.out = length(res$residuals)))

x1=qnorm(.25)
x2=qnorm(.75)
y1=quantile(res$residuals,.25)
y2=quantile(res$residuals,.75)

m=(y2-y1)/(x2-x1)
b=y1-m*x1

quantiles=data.frame(emp_quantiles,teo_quantiles)
names(quantiles)=c('Empirical','Theoretical')

ggplot(quantiles,aes(x=Theoretical,y=Empirical))+
  geom_point(col='black',size=2)+
  geom_abline(intercept = b ,slope =m,col="red")+
  labs(title='',x='Cuantiles Teóricos',y='Cuantiles Empíricos')+
  theme_minimal()+
  theme(axis.title =element_text(size=8))

# Pruebas de normalidad
lillie.test(res$residuals)
ad.test(res$residuals)
cvm.test(res$residuals)

# Varianza constante
ggplot(data=residuales,aes(x=x,y=y))+
  geom_point()+
  labs(x='Fitted values',y='Residuals')+
  geom_hline(yintercept =0,col='red')+
  theme_minimal()

ncvTest(res)

# No correlación
dwtest(res)
##########################################################################

################################################################################
# Multicolinealidad
data(cement)

# Graficamos por pares
ggpairs(cement)

# VIF
X=cement%>%dplyr::select(x1,x2,x3,x4)
W=scale(X)
Z=t(W)%*%W/12
C=solve(Z);C
VIFs = diag(C);VIFs

res=lm(y~x1+x2+x3+x4,data =cement)
vif(res)

# Número de condición
evd = eigen(Z)
evd$values

kappa = max(evd$values)/min(evd$values);kappa
kappa_j = evd$values/min(evd$values);kappa_j

# Descomposición en valores singulares
SVD = svd(W/sqrt(12))
SVD$d

eta_j = max(SVD$d)/SVD$d;eta_j

# Posible solución eliminar variables

# Selección por segmentos (coincide también con los otros métodos)
res= lm(y~1,cement)
step.model = stepAIC(res, direction = "both",
                     scope=list(upper=~x1+x2+x3+x4))

# VIF
res=lm(y~x1+x2+x4,data =cement)
vif(res)

# Número de condición
X=cement%>%select(x1,x2,x4)
W=scale(X)
Z=t(W)%*%W/12;Z

evd = eigen(Z)
evd$values

kappa = max(evd$values)/min(evd$values);kappa
kappa_j = evd$values/min(evd$values);kappa_j

# Descomposición en valores singulares
SVD = svd(W/sqrt(12))
SVD$d

eta_j = max(SVD$d)/SVD$d;eta_j

# Modelo
res=lm(y~x1+x2+x4,cement)
summary(res) # x4 no es significativa

res = lm(y~x1+x2,cement)
summary(res)


# Análisis de residuales
residuales = data.frame(x=res$fitted.values,y=res$residuals)

# Supuestos de normalidad

# histograma
ggplot(data=residuales,aes(x=y))+
  geom_histogram(breaks=hist(res$residuals,plot=F)$breaks,col='black',
                 fill='skyblue2')+
  labs(x='',y='')+
  theme_minimal()

# qqplot
emp_quantiles=quantile(res$residuals,
                       probs=seq(0.01,.99,length.out = length(res$residuals)))
teo_quantiles=qnorm(seq(.025,.975,length.out = length(res$residuals)))

x1=qnorm(.25)
x2=qnorm(.75)
y1=quantile(res$residuals,.25)
y2=quantile(res$residuals,.75)

m=(y2-y1)/(x2-x1)
b=y1-m*x1

quantiles=data.frame(emp_quantiles,teo_quantiles)
names(quantiles)=c('Empirical','Theoretical')

ggplot(quantiles,aes(x=Theoretical,y=Empirical))+
  geom_point(col='black',size=2)+
  geom_abline(intercept = b ,slope =m,col="red")+
  labs(title='',x='Cuantiles Teóricos',y='Cuantiles Empíricos')+
  theme_minimal()+
  theme(axis.title =element_text(size=8))

# Pruebas de normalidad
lillie.test(res$residuals)
ad.test(res$residuals)
cvm.test(res$residuals)

# Varianza constante
ggplot(data=residuales,aes(x=x,y=y))+
  geom_point()+
  labs(x='Fitted values',y='Residuals')+
  geom_hline(yintercept =0,col='red')+
  theme_minimal()

ncvTest(res)

# No correlación
dwtest(res)

################################################################################


