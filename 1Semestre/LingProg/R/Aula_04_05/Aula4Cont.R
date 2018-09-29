#------------------------------------------------------------------------------------------
##DADOS DO GAPMINDER (Fonte boa de bases de dados)
#------------------------------------------------------------------------------------------
url<-"http://leg.ufpr.br/~fernandomayer/data/pib_gapminder.csv"
dados<- read.table(url, header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)

str(dados)

summary(dados)

unique(dados$pais)
table(dados$pais)

unique(dados$ano)
length(unique(dados$ano))

unique(dados$continente)
table(dados$continente)
addmargins(table(dados$continente))
prop.table(table(dados$continente))
table(dados$ano, dados$continente)

barplot(table(dados$continente))

mean(dados$expVida)
var(dados$expVida)
is.na(dados)

#Inserir aleatoriamente 100 NA na coluna expVida
x <- dados$expVida
x[sample(1:length(x), size = 100)] <- NA
summary(x)
mean(x)

#Remover NA e calcular a média...
mean(x,na.rm = TRUE)
var(x,na.rm = TRUE)

#Graficos
boxplot(dados$expVida)
boxplot(dados$expVida[dados$pais == "Brazil"])
with(dados,boxplot(expVida == "Brazil")) #Usar a função with evita ter quue usar $ (dolar)...

boxplot(expVida ~ pais, data = dados)


##Boxplot para 3 países

boxplot(expVida ~ pais, data = dados, subset = pais %in% c("Brazil", "Argentina", "Chile"))

dados2<-subset(dados, pais %in% c("Brazil", "Argentina", "Chile"))
str(dados2)

boxplot(expVida ~ pais, data=dados2)

#Apply

apply(dados2[,5:6], 2, mean)
apply(dados2[,5:6], 2, summary)

#sapply e lapply fazem a mesma coisa. A diferença a saída do resultado. lappy () retorna uma lista na saída. sapply retorna uma saida mais simplificada.
lapply(dados2[,5:6], 2, summary)
sapply(dados2[,5:6], 2, summary)


tapply(dados$expVida, dados$pais, mean)

with(dados2,tapply(expVida, pais, mean))
with(dados2,tapply(expVida, list(ano, pais), mean))
with(dados2,tapply(expVida, list(continente, ano, pais), mean)) #Retorna um array...

with(dados2, tapply(data.frame(expVida,pibPercap), pais, mean)) # Não funciona...Usar aggregate

#Aggregate

aggregate(expVida ~ pais, data=dados2, mean) #Retorna um data.frame
with(dados2, tapply(expVida,pais,mean)) #Retorna um vetor sempre...

aggregate(cbind(expVida, pibPercap)~ pais, data = dados2, mean)
aggregate(cbind(expVida, pibPercap)~ pais + ano, data = dados2, mean)
aggregate(expVida ~ ano + pais, data=dados2, mean)

library(plyr)
#Dataframe p/ dataframe...

ddply(dados2, .(pais), summarize, media = mean(expVida))
ddply(dados2, .(pais), summarize, var = var(expVida))
ddply(dados2, .(pais, ano), summarize, var = var(expVida))
ddply(dados2, .(pais), summarize, 
      media.expVida = mean(expVida),
      var.expVida = var(expVida),
      media.pib = mean(pibPercap),
      var.pib = var(pibPercap))

