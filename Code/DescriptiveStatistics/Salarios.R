##########################################################################
# Descriptive statistics                                              
# Author : Jose Antonio Perusquia Cortes
# Afil   : Facultad de Ciencias - UNAM
# Module : Statistical Inference
##########################################################################

##########################################################################
# Required libraries
library(here)           # Version 1.0.2
library(ggplot2)        # Version 4.0.2
library(ggthemes)       # Version 5.2.0
library(tidyr)          # Version 1.3.2
library(dplyr)          # Version 1.2.0
library(DescTools)      # Version 0.99.60
##########################################################################

##########################################################################
# Data

# 1171 observations with 7 variables
salaries = read.csv(here("Code/DescriptiveStatistics/datascience_salaries.csv"))
N = nrow(salaries)
P = ncol(salaries)

# Allows to see what kind of variables we have (e.g. integers,
# numeric, characters, etc.)
glimpse(salaries)

# Check for missing values
colSums(is.na(salaries))
##########################################################################

##########################################################################
# job_title descriptive analysis

# Frequency table
job = salaries %>%
  count(job_title, name = "freqs_abs") %>%
  mutate(freqs_rel = freqs_abs / N)
job

# Bar plot
ggplot(data=job,aes(x=job_title,y=freqs_abs,fill=job_title))+
  geom_col(col='black',show.legend = F)+
  geom_text(aes(label=freqs_abs),vjust=-.5,size=3)+
  labs(x="",y="Frecuencias")+
  theme_minimal()

# We can also change the order and the direction of the bar plot
job = salaries %>%
  count(job_title, name = "freqs_abs") %>%
  mutate(freqs_rel = freqs_abs / N) %>%
  arrange(freqs_abs)

ggplot(job,
       aes(x = reorder(job_title, freqs_abs),
           y = freqs_abs,
           fill = job_title)) +
  geom_col(col = "black", show.legend = FALSE) +
  geom_text(aes(label = freqs_abs), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(x = "", y = "Frecuencias") +
  theme_minimal()
##########################################################################

##########################################################################
# job_type descriptive analysis

# Frequency table
job_type = salaries %>%
  count(job_type, name = "freqs_abs") %>%
  mutate(freqs_rel = freqs_abs / N)
job_type

# Bar plot using relative frequencies
ggplot(job_type, aes(x = job_type, y = freqs_rel, fill = job_type)) +
  geom_col(col = "black", show.legend = FALSE) +
  geom_text(aes(label = scales::percent(freqs_rel, accuracy = 0.1)),
            vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "Frecuencias relativas") +
  theme_minimal()
##########################################################################

##########################################################################
# experience_level descriptive analysis

# Frequency table
experience = salaries %>%
  count(experience_level, name = "freqs_abs") %>%
  mutate(freqs_rel = freqs_abs / N)
experience

# Bar plot
ggplot(data=experience,aes(x=experience_level,y=freqs_abs,
                         fill=experience_level))+
  geom_col(col='black',show.legend = F)+
  geom_text(aes(label=freqs_abs),vjust=-.5,size=3)+
  labs(x="",y="")+
  theme_minimal()
##########################################################################

##########################################################################
# salary_currency descriptive analysis

# Frequency table
currency = salaries %>%
  count(salary_currency, name = "freqs_abs") %>%
  mutate(freqs_rel = freqs_abs / N)
currency

# Bar plot
ggplot(data=currency,aes(x=salary_currency,y=freqs_abs,
                           fill=salary_currency))+
  geom_col(col='black',show.legend = F)+
  geom_text(aes(label=freqs_abs),vjust=-.5,size=3)+
  labs(x="",y="")+
  theme_minimal()
##########################################################################

##########################################################################
# Salaries

# We filter the salaries in USD (we are not filtering because USD
# observations are inherently preferable; we are doing it because
# salaries need a common monetary scale before numerical comparison)
salariesUSD = salaries%>%filter(salary_currency=='USD')

# Obtain descriptive statistics
stats_salaries = salariesUSD %>%
  summarise(
    n = n(),
    media = mean(salary),
    mediana = median(salary),
    desv_est = sd(salary),
    Min = min(salary),
    q1 = quantile(salary, .25),
    q3 = quantile(salary, .75),
    Max = max(salary),
    IQR = IQR(salary),
    sesgo = Skew(salary),
    curtosis = Kurt(salary)
  )
stats_salaries

# Histogram using frequencies
ggplot(salariesUSD, aes(x = salary)) +
  geom_histogram(col = "black", fill = "darkred", bins = 25,alpha=.5) +
  geom_vline(aes(xintercept = mean(salary)),linetype = "dashed") +
  geom_vline(aes(xintercept = median(salary)),linetype = "dotted") +
  theme_minimal() +
  labs(x = "Salario", y = "Frecuencia")

# Boxplot
ggplot(data=salariesUSD,aes(x=salary))+
  geom_boxplot(col='black',fill='lightblue3')+
  theme_minimal()+
  labs(x="",y="")
##########################################################################

##########################################################################
# Salaries stratified by job_title

# Obtain descriptive statistics
salariesJobTitle = salariesUSD%>%
  group_by(job_title)%>%
  summarise(n = n(),
            media = mean(salary),
            mediana = median(salary),
            desv_est = sd(salary),
            Min = min(salary),
            q1 = quantile(salary, .25),
            q3 = quantile(salary, .75),
            Max = max(salary),
            IQR = IQR(salary),
            sesgo = Skew(salary),
            curtosis = Kurt(salary))
salariesJobTitle

# Boxplot
ggplot(data=salariesUSD,aes(y=job_title,x=salary,fill=job_title))+
  geom_boxplot(col='black',show.legend = F)+
  theme_minimal()+
  labs(x="",y="")
##########################################################################

##########################################################################
# Salaries stratified by job_type

# Obtain descriptive statistics
salariesJobType = salariesUSD%>%
  group_by(job_type)%>%
  summarise(n = n(),
            media = mean(salary),
            mediana = median(salary),
            desv_est = sd(salary),
            Min = min(salary),
            q1 = quantile(salary, .25),
            q3 = quantile(salary, .75),
            Max = max(salary),
            IQR = IQR(salary),
            sesgo = Skew(salary),
            curtosis = Kurt(salary))
salariesJobType

# Boxplot
ggplot(data=salariesUSD,aes(y=job_type,x=salary,fill=job_type))+
  geom_boxplot(col='black',show.legend = F)+
  theme_minimal()+
  labs(x="",y="")
##########################################################################

##########################################################################
# Salaries stratified by experience level

# Obtain descriptive statistics
salariesExpLevel = salariesUSD%>%
  group_by(experience_level)%>%
  summarise(n = n(),
            media = mean(salary),
            mediana = median(salary),
            desv_est = sd(salary),
            Min = min(salary),
            q1 = quantile(salary, .25),
            q3 = quantile(salary, .75),
            Max = max(salary),
            IQR = IQR(salary),
            sesgo = Skew(salary),
            curtosis = Kurt(salary))
salariesExpLevel

# Boxplot
ggplot(data=salariesUSD,aes(y=experience_level,x = salary,
                         fill=experience_level))+
  geom_boxplot(col='black',show.legend = F)+
  theme_minimal()+
  labs(x="",y="")
##########################################################################
