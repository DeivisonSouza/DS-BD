#6 Outras classes
#Como mencionado na seção anterior, o R possui 6 tipos básicos de estrutura de dados, mas diversas #classes podem ser construídas a partir destes tipos básicos. Abaixo, veremos algumas das mais #importantes.

## Fator
fat<-factor(c("alta","baixa","baixa","media","alta","media","baixa","media","media"))
fat #Observar que os fatores não vem com média...
class(fat)
typeof(fat) #interessante que internamente considera números inteiros.
unclass(fat) #Como é que o vetor é internamente? Mostra que alta=1, baixa=2,media=3
as.character(fat) #Posso tranformar para um vetor de caracter...

#Observar que factor colocar os níveis em ordem alfabetica...
#Forçar que os fatores obedeçam a ordem desejada?
fat<-factor(c("alta","baixa","baixa","media","alta","media","baixa","media","media"),
            levels=c("alta","media","baixa"))
fat

fat<-factor(c("alta","baixa","baixa","media","alta","media","baixa","media","media"),
            levels=c("baixa","media","alta"),
            ordered=TRUE)
fat
class(fat) #O objeto passa a ter 2 classes: "ordered" "factor" 
typeof(fat)

#Extrair os níveis de um objeto factor...
levels(fat)
nlevels(fat)
attributes(fat)
attributes(fat)$levels

#MATRIZES
#Todos os elementos de uma mmatriz devem ser da mesma classe...
m<-matrix(1:12, nrow=3, ncol=4) #Preenchimento default é por colunas...
m

m<-matrix(1:12, nrow=3, ncol=4,byrow=TRUE) #Altera a preenchimento para linha...
m

class(m)
typeof(m)
dim(m)
length(m)

#Colar linhas ou colunas?
rbind(m,rep(99,4))
m #Reparar que a matriz m continua sendo a mesmo. Para alterar a matriz original tenho que fazer isso explicitamente...

cbind(m,rep(99,3))

m<-1:10
m
dim(m)<-c(2,5)
m #Uma matriz no R é um vetor, mais que tem um atributo dimensão...

#Operação matemática com matriz...
#Multiplicação por um escalar:
m<-matrix(1:12, nrow=3, ncol=4, byrow=TRUE)
m*2

#Multiplicação de matrizes
m2<-matrix(1, nrow=4, ncol=3, byrow=TRUE)
m*m2 #Operador * não funciona para matrizes
m%*%m2 #Então, deve-se usar o operador * entre %%...

#-------------------------------------------------------------------
##ARRAY
#-------------------------------------------------------------------
#O array pode ter n dimensões. Só pode ter elementos da mesma classe.
ar<-array(1:12,dim = c(2,2,3)) #linhas, colunas e dimensões...
ar
class(ar)
typeof(ar)

#-------------------------------------------------------------------
##LISTA
#-------------------------------------------------------------------
#VETOR RECURSIVO:Pode ter lista dentro de lista, dentro de lista...
lista<-list(1:30, "R", list(TRUE, FALSE))
lista
class(lista)
typeof(lista)
str(lista)
dim(lista) #Não existe. Pois, a lista é unidimensional.
length(lista) ## Possui 3 componentes

lista<-list(m,fat) #Primeiro componente é uma matriz e o segundo é um fator...
lista
str(lista)

#-------------------------------------------------------------------
##DATAFRAME
#-------------------------------------------------------------------
#É a estrutura mais genérica que existe para armazenar dados.
da<-data.frame(nome= c("João", "José", "Maria"),
               sexo = c("M","M","F"),
               idade = c(32,34,30))
da

class(da)
typeof(da) #Data frame nada mais do que uma lista, cujos os objetos possuem o mesmo comprimento.
str(da)

#O padrão da função data.frame é converter character para factor.
#O que fazer? Usar o argumento stringsAsFactors, para não converter para fator...
da<-data.frame(nome= c("João", "José", "Maria"),
               sexo = c("M","M","F"),
               idade = c(32,34,30),
               stringsAsFactors = F)
da
str(da)


#Unindo fatores de difrentes comprimentos...
num<-c(10,5,2,4,8,9)
length(num)
length(fat)
db<-data.frame(numerico = c(num, NA,NA,NA),
               fator = fat)
db
str(db)

#converter para matriz...
as.matrix(db)
data.matrix(db) #Converte o fator para classe de números...


#-------------------------------------------------------------------
##ATRIBUTOS DE OBJETOS
#-------------------------------------------------------------------
#Atribuir
x<-1:6
names(x)<-c("um","dois","tres","quatro","cinco","seis")
x
names(x)
attributes(x)
class(x)
typeof(x)
x*2

#Remover
names(x)<-NULL
x

#Alterar tamanho do vetor...
length(x)<-10
x
length(x)<-6
dim(x)<-c(3,2)
dim(x) <- NULL

#data.frame
da
attributes(da)
row.names(da)














