library(RColorBrewer)
library(stringr)
library(tidyr)
library(lubridate)
library(dplyr)
library(ggplot2)

#Para os nomes dos meses aparecerem em português
Sys.setlocale("LC_TIME", "Portuguese")

#Preparando o ambiente e importando o dataset. 
#Baixe os dados daqui: https://opendatasus.saude.gov.br/dataset/bd-srag-2020

rm(list=ls())
setwd("C:/Users/lucam/Downloads")
data=read.csv("INFLUD-23-11-2020.csv",sep=";")

#Caso queira filtrar por estado inclua a linha a seguir
#data=data[data$SG_UF_NOT=="PR",]

#Preparando o df

df=data[,c("DT_NOTIFIC","NU_IDADE_N","CLASSI_FIN")]
df=na.omit(df)
df=rename(df,Date=1,Age=2,Class=3)

#Filtra para classificação = 5, em que 5 é COVID 19 confirmado. 
#4 é SRAG não identificada; se quiser incluir também, troque 5 na linha abaixo por 4|5

df=df[df$Class==5,]
df=df[,c(1,2)]

#Remover dados que pareçam ter erro no input de idade
df=df[df$Age>0,]
df=df[df$Age<98,]

#Agrupa as idades
df=df %>% mutate(cuts = cut(Age, c(0,10,20,30,40,50,60,70,80, Inf))) %>% 
  group_by(cuts,Date) %>% 
  summarize(n=n()) %>%
  rename(Age=1,Date=2,Count=3)


levels(df$Age)= c("Menos de 10 anos", "Entre 10 e 19 anos","Entre 20 e 29 anos","Entre 30 e 39 anos","Entre 40 e 49 anos","Entre 50 e 59 anos","Entre 60 e 69 anos","Entre 70 e 79 anos","80 anos ou mais")

#Formata as datas
dates=str_split_fixed(df$Date,"/",n=3) %>% 
  data.frame() %>%
  rename(day=1,mon=2,year=3) 

df = paste(dates$year, dates$mon, dates$day, sep="-") %>% ymd() %>% as.Date() %>%
  data.frame(df$Age,df$Count) %>%
  rename(Date=1,Age=2,Count=3)

df=df[df$Date>as.Date("2020-04-01",format="%Y-%m-%d"),]

#Agrupa as datas em intervalos semanais
df=df %>% mutate(cuts = cut(Date,"7 days"))  %>%
  group_by(cuts,Age,Count) %>% 
  summarize(n=n()) %>% select(c(1,2,3)) %>%
  rename(Date=1,Age=2,Count=3)   

df$Date = format(strptime(as.character(df$Date),"%Y-%m-%d"),"%Y/%B/%d")
df$Date = as.Date(df$Date,format="%Y/%B/%d")

#Plota o gráfico
ggplot(df, aes(Date, Age, fill= Count)) + 
  geom_tile(color="white")+ scale_fill_distiller(palette = "RdYlGn",direction=-1,name="Casos")+theme(plot.title=element_text(size=16,face="bold"),legend.title = element_text(size=10),plot.caption = element_text(hjust = 0.25,size=7))+ylab(NULL)+xlab("Mês")+
labs(title="Casos confirmados de COVID-19 no Brasil por idade*",caption = "Fonte: SVS SRAG OpenDATASUS. * Inclui apenas os casos registrados no sistema, cerca de 10% do total")+coord_fixed(ratio=10)
 

