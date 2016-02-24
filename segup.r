library(dplyr)
library(lubridate)
library(chron)
library(ggplot2)
library(arules)
library(DT) 
library(dygraphs)
library(zoo)
library(leaflet)
library(UsingR)
library(rCharts)
library(ggmap)

dados <- read.csv("/home/stark/MEGAsync/SEGUP/BD GERAL 2012 HOM LATR Estado.csv",header = T,na.strings = c("(null)") )
dados<- dados[,1:31]

dados$Data.do.Registro <- mdy(dados$Data.do.Registro )
dados$DATA.FATO <- mdy(dados$DATA.FATO  )

dados <- dados%>%
  mutate( DATA_HORA_FATO = ymd_hm(paste( dados$DATA.FATO, " ",dados$HORA.FATO)) )%>%
  mutate( DIF_FATO_REGISTO = as.factor(difftime( dados$Data.do.Registro,dados$DATA.FATO, units = "days")))

boo1 <- dados$Bairros == "centro"
boo2 <- dados$Bairros == "Centro"
boo3 <- dados$Bairros == "CENTRO"
boo4 <- dados$Bairros == "Centro "

dados[boo1,]$Bairros <- as.factor("CENTRO")
dados[boo2,]$Bairros <- as.factor("CENTRO")
dados[boo3,]$Bairros <- as.factor("CENTRO")
dados[boo4,]$Bairros <- as.factor("CENTRO")

dados$Registros <- as.character(dados$Registros)
dados[dados$Registros=="Latrocínio",]$Registros <- "Latrocinio"
dados[dados$Registros=="Homicídio",]$Registros = "Homicidio"
dados$Registros <- as.factor(dados$Registros)

levels(dados$Registros)

total_ocorrencias <- dados%>%
  group_by(Registros)%>%
  summarise(total=n() )
total_ocorrencias


limiar <- 30
ocorrencias <- dados%>%
  group_by(Registros,Bairros,Municípios)%>%
  summarise(total=n() )

ocorrencias$nome <- paste( ocorrencias$Bairros,"-",ocorrencias$Municípios )

homicidios <- ocorrencias%>%
  filter(total>limiar)%>%
  arrange(desc(total))

homicidios


ggplot(homicidios, aes(nome,total , fill=Registros  ) )+
  geom_bar(stat="identity")+
  xlab("Bairros")+ylab("Quantidade")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))



latrocinio <- ocorrencias%>%
  filter(Registros=="Latrocinio")%>%
  filter(total>2)%>%
  arrange(desc(total))

latrocinio

ggplot(latrocinio, aes(nome,total , fill=Registros) ) +
  geom_bar(stat="identity")+
  xlab("Bairros")+ylab("Quantidade")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

ocorrencias_DIA <- dados%>%
  group_by( DATA.FATO) %>%
  summarise(count = n())

sum(ocorrencias_DIA$count)
media <- mean(ocorrencias_DIA$count)
media

ggplot(ocorrencias_DIA, aes(DATA.FATO, count)) + geom_line()+  geom_smooth()  + geom_hline(aes(yintercept=media,colour="red")  )

ocd <- zoo(ocorrencias_DIA$count, order.by=ocorrencias_DIA$DATA.FATO)
mn = mean(ocd, na.rm = TRUE)
std = sd(ocd, na.rm = TRUE)
dygraph(ocd, main = "Ocorrências por dia", ylab = "Total de Ocorrências")  %>%
  dySeries("V1" ,label="Mortes")%>%
  dyRangeSelector(dateWindow = c("2012-01-01", "2012-12-31"))%>%
  dyShading(from = mn - std, to = mn + std, axis = "y")

dobroMedia <- 2 * media
maiorMedia <- filter( ocorrencias_DIA, count > dobroMedia)
dim(maiorMedia)

ggplot(maiorMedia, aes(DATA.FATO, count)) + geom_line()+  geom_smooth(method="lm")

metadeMedia <-  media/2
menorMedia <- filter( ocorrencias_DIA, count <= metadeMedia)
ggplot(menorMedia, aes(DATA.FATO, count)) + geom_line()+  geom_smooth(method="lm")

ggplot( dados,aes(FX.12.HOR,fill=is.weekend(DATA_HORA_FATO)) )+
  geom_bar()+
  xlab("Horários")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

dadosMes <- dados
dadosMes$mes <-month(dadosMes$DATA.FATO)
resumoMes <- dadosMes %>% group_by(mes) %>%summarise(total=n())

sum( resumoMes[1:6,]$total)

sum( resumoMes[7:12,]$total)

ggplot( resumoMes,aes(x=mes,total) )+
  geom_bar(stat="identity")+
  xlab("Meses") + geom_vline(xintercept = 6.5)

source("/home/stark/MEGAsync/SEGUP/calendarHeat.R")
calendarHeat(ocorrencias_DIA$DATA.FATO, ocorrencias_DIA$count, varname="Calendar Heat Map of Violence in Pará",color = "g2r")
calendarHeat(maiorMedia$DATA.FATO, maiorMedia$count, varname="Calendar Heat Map of Most Violent days in Pará",color = "g2r")
calendarHeat(menorMedia$DATA.FATO, menorMedia$count, varname="Calendar Heat Map of least Violent days in Pará",color = "g2r")

transacoes <- dados
transacoes$AISP <- NULL
transacoes$SUPC <- NULL
transacoes$IDENTIFICAÇÃO.DO.FATO <- NULL
transacoes$RUA.DO.FATO <-NULL
transacoes$ANO.FATO<-NULL
transacoes$ANO.REGISTRO<-NULL
transacoes$Mês.do.Registro<-NULL
transacoes$Data.do.Registro<-NULL
transacoes$DATA.FATO<-NULL
transacoes$DIA.SEMANA<-NULL
transacoes$HORA.FATO<-NULL
transacoes$Registros.2<-NULL
transacoes$DATA_HORA_FATO<-NULL
transacoes$DIF_FATO_REGISTO<-NULL

tr <- as(transacoes,"transactions")

summary(tr)

itemFrequencyPlot(tr, support = 0.5, cex.names=0.5)

rules <- apriori(tr, parameter= list(supp=0.1, conf=0.5))

tab <-as(rules, "data.frame")
DT::datatable(tab)




