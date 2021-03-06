#6 Outras classes
#Como mencionado na se��o anterior, o R possui 6 tipos b�sicos de estrutura de dados, mas diversas #classes podem ser constru�das a partir destes tipos b�sicos. Abaixo, veremos algumas das mais #importantes.

## Fator
fat<-factor(c("alta","baixa","baixa","media","alta","media","baixa","media","media"))
fat #Observar que os fatores n�o vem com m�dia...
class(fat)
typeof(fat) #interessante que internamente considera n�meros inteiros.
unclass(fat) #Como � que o vetor � internamente? Mostra que alta=1, baixa=2,media=3
as.character(fat) #Posso tranformar para um vetor de caracter...

#Observar que factor colocar os n�veis em ordem alfabetica...
#For�ar que os fatores obede�am a ordem desejada?
fat<-factor(c("alta","baixa","baixa","media","alta","media","baixa","media","media"),
            levels=c("alta","media","baixa"))
fat

fat<-factor(c("alta","baixa","baixa","media","alta","media","baixa","media","media"),
            levels=c("baixa","media","alta"),
            ordered=TRUE)
fat
class(fat) #O objeto passa a ter 2 classes: "ordered" "factor" 
typeof(fat)

#Extrair os n�veis de um objeto factor...
levels(fat)
nlevels(fat)
attributes(fat)
attributes(fat)$levels

#MATRIZES
#Todos os elementos de uma mmatriz devem ser da mesma classe...
m<-matrix(1:12, nrow=3, ncol=4) #Preenchimento default � por colunas...
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
m #Uma matriz no R � um vetor, mais que tem um atributo dimens�o...

#Opera��o matem�tica com matriz...
#Multiplica��o por um escalar:
m<-matrix(1:12, nrow=3, ncol=4, byrow=TRUE)
m*2

#Multiplica��o de matrizes
m2<-matrix(1, nrow=4, ncol=3, byrow=TRUE)
m*m2 #Operador * n�o funciona para matrizes
m%*%m2 #Ent�o, deve-se usar o operador * entre %%...

#-------------------------------------------------------------------
##ARRAY
#-------------------------------------------------------------------
#O array pode ter n dimens�es. S� pode ter elementos da mesma classe.
ar<-array(1:12,dim = c(2,2,3)) #linhas, colunas e dimens�es...
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
dim(lista) #N�o existe. Pois, a lista � unidimensional.
length(lista) ## Possui 3 componentes

lista<-list(m,fat) #Primeiro componente � uma matriz e o segundo � um fator...
lista
str(lista)

#-------------------------------------------------------------------
##DATAFRAME
#-------------------------------------------------------------------
#� a estrutura mais gen�rica que existe para armazenar dados.
da<-data.frame(nome= c("Jo�o", "Jos�", "Maria"),
               sexo = c("M","M","F"),
               idade = c(32,34,30))
da

class(da)
typeof(da) #Data frame nada mais do que uma lista, cujos os objetos possuem o mesmo comprimento.
str(da)

#O padr�o da fun��o data.frame � converter character para factor.
#O que fazer? Usar o argumento stringsAsFactors, para n�o converter para fator...
da<-data.frame(nome= c("Jo�o", "Jos�", "Maria"),
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
data.matrix(db) #Converte o fator para classe de n�meros...


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














