#--------------------------------------------------------------------------------------
# Indexa��o e sele��o condicional
#--------------------------------------------------------------------------------------
dados<-c(5,15,42,28,79,4,7,14)

#Usando os cochetes para indexa��o retorna os valores requeridos.
dados[dados>15]
dados[dados>=15]
dados[dados>15 & dados<=35]
dados[dados>15 | dados<=35]

#Sem usar cochete retorna FALSE ou TRUE (uma resposta l�gica)
dados>15
dados>15 & dados<=35
dados>15 | dados<=35

#Vetor condicionado a outro vetor...
cara<-letters[1:length(dados)]
cara
dados[cara == "a"]
cara == "a"

dados[cara == "a" & cara == "c"]
cara == "a" & cara == "c"

dados[cara == "a" | cara == "c"]

#Podemos otimizar isso?
dados[cara == c("a","c")] #Nesse caso, retorna s� o primeiro...

#Agora sim, retorna os valores esperados!
dados[cara %in% c("a","c")] #O operador "in" � bastante utilizado para fazer sele��o em objetos do R. 
#Este operador consegue fazer a verifica��o das condi�oes uma a uma. Usar concatenate n�o faz isso.

cara %in% c("a","c")

##Adendo

c(1,2,3) == c(3,2,1)

1  %in% c(3,4,5) #Existe o n. 1 no vetor c(3,4,5)? A id�ia do %in% � de igual.

c(1,2) %in% c(3,4,5) #Existe 1 ou 2 em qualquer lugar do vetor c(3,4,5)?

c(1,2,3,4) %in% c(3,4,5)

#Retornando os valores (colocar nos colchetes)
x <- c(1,2,3,4)
x[x %in% c(3,4,5)]

#Invertendo os vetores cara e dados? Dai retorna as letras...
cara[dados == 15]
cara[dados > 15]

## Fun��o Which ()

dados[dados>15]
which(dados > 15) #Retorna a posi��o dos valores que satisfazem a condi��o passada...

dados[cara == "a"]
which(cara == "a")

dados[cara %in% c("a","b")]

#--------------------------------------------------------------------------------------
# Sele��o condicional em data frames
#--------------------------------------------------------------------------------------
dados<-data.frame(ano=c(2001,2002,2003,2004,2005),
                  captura = c(26,18,25,32,NA),
                  porto = c("SP","RS","SC","SC","RN"))
str(dados)
dados[dados$ano == 2004, ]
dados[dados$porto == "SC", ]

dados[dados$ano == 2004, "captura"]
dados[dados$ano == 2004, 2]

dados[dados$captura > 20 & dados$porto == "SP",]

dados[dados$captura > 20,] #Retrna uma linha final com NAs, devido a um NA presente.
is.na # Verifica onde existe NA.

dados[dados$captura > 20 & !is.na(dados$captura),] # Retorna dados de captura > 20, exceto a que tiver NA.
dados[dados$captura > 20 & complete.cases(dados),]

#Fun��o subset
dados[dados$porto == "SP",]
subset(dados, porto == "SP")

dados[dados$captura > 20,]
subset(dados, captura > 20) # A fun��o subset a lida com NA, ou seja j� entende que n�o deve retorna o NA.

dados[dados$captura > 20, "captura"]
subset(dados, captura > 20, select = "captura")
subset(dados, captura > 20, select = c("ano", "captura"))

#--------------------------------------------------------------------------------------
# Exerc�cio 4
#--------------------------------------------------------------------------------------
#1. Voc� contou 42 carangueijos na Joaquina, 

car<-data.frame(local= c("Joaquina", "Campeche", "Arma��o", "Praia Mole"),
                   n = c(42,34,59,18))
str(car)

#2
car[car$n < 30,]
car[car$n < 30, "local"] #Quero que retorne s� o nome da praia?
car$local[car$n < 30]
subset(car, n <30, select="local")

#3
car$regi�o<-NA #Posso criar a coluna e inserir NA
car

#4
car$regi�o[car$local %in% c("Joaquina", "Praia Mole")] <- "leste"
car
car$regi�o[car$local %in% c("Campeche", "Arma��o")] <- "sul"
car

#Alternativa
car$regi�o<-c("leste","sul","sul","leste")
car[car$regi�o == "leste" & car$n < 20,]

#5
car$local[car$regi�o == "sul" & car$n > 40]

#6
car$regi�o[car$n > 50]

#-----------------------------------------------------------------------------------------------
#Programando com dados...
#-----------------------------------------------------------------------------------------------
for(i in 1:10){
  print(i)
}

#Cria vetor x e retorna os valores nas posi�oes de 1 a 10...
x <- 100:200
for(j in 1:10){
  print(x[j])
}

#
for(i in c(2,9,4,6)){
  print(i^2)
}

#
for(veiculos in c("carro","onibus","bicicleta")){
  print(veiculos)
}

#-------------------------------------------------------------------------------------------------
url<-"http://leg.ufpr.br/~fernandomayer/data/notas.csv"

dados<-read.table(url, header = TRUE, sep = ";", dec = ",", stringsAsFactors = FALSE)


#header = Ler cabe�alho;
#sep = separador de colunas;
#dec = separador de decimais;
#stringsAsFactors = Para n�o transformar os nomes em fatores (manter como character) quando = FALSE

str(dados)
summary(dados)
head(dados)
tail(dados)

head(dados,10)
tail(dados,10)

dados[1:10,]

#Usando for()
for(i in 1:30){
  print(dados[i, c("prova1","prova2","prova3")])
}

#Calculando a m�dia para cada linha
dados$media <- 0
for (i in 1:30){
  dados$media[i]<-sum(dados[i, c("prova1","prova2","prova3")])/3
}
head(dados)

#Deixando o for() mais gen�rico
nlinhas<-nrow(dados)
provas<-c("prova1","prova2","prova3")
nprovas<-length(provas)

dados$media2<- 0
for(i in 1:nlinhas){
  dados$media2[i]<-sum(dados[i,provas])/nprovas
}
head(dados)

dados$media3<- 0
for(i in 1:nlinhas){
  dados$media3[i]<-mean(as.numeric(dados[i,provas])/nprovas)
}
head(dados)

#-----------------------------------------------------------------------------------------------------
#Estrutura de sele��o if()
#-----------------------------------------------------------------------------------------------------
x<-100:200
for(j in 1:10){
  if(x[j] <=105){
    print("Menor ou igual a 105")
  } else{
    print("Maior do que a 105")
  }
}

# Criando uma coluna de situacao (aprovado ou reprovado) usando if() dentro do for()
dados$situacao <- NA

for(i in 1:nlinhas){
  if(dados$media[i]>=7){
    dados$situacao[i] <- "Aprovado"
  }else{
    dados$situacao[i] <- "Reprovado"
  } 
}

# Modo R
x <- 1:1000000

#Elevar ao quadrado usando for()
y1<-numeric(length(x))

for(i in 1:length(x)){
  y1[i] <- x[i]^2
}

head(y1)

y2<-x^2

identical(y1, y2)


#Tempo que leva para executar uma opera��o (system.time)
y1<-numeric(length(x))
system.time(
for(i in 1:length(x)){
  y1[i] <- x[i]^2
}
)

system.time(
  y2<-x^2
  )

#Duas operacoes no system.time (usar chaves{})
system.time({
y1<-numeric(length(x))
  for(i in 1:length(x)){
    y1[i] <- x[i]^2
  }
})


## Fun�oes da fam�lia apply() (fun�oes vetorizadas)
dados$media.apply <- apply(x = dados[,provas], MARGIN = 1, FUN = mean) #Aplique a m�dia para a linhas das colunas prova
head(dados)

dados$situacao2<-ifelse(dados$media >= 7, "Aprovado", "ReprovadO")


## Exportando
write.table(dados, "dados.csv", sep = ",", dec = ".", row.names = FALSE)






