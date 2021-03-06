#Linguagem de Programa��o#
##########################
#R b�sico
#Estrutura de controles
#Leitura e manipula��o de dados I
#Visualiza��o I
#Leitura e manipula��o de dados II

#Refer�ncia base: R for Data Science e Hands-On Programing with R (introdu��o b�sica)

#------------------------------------------------------------------------------------------------------------
##Fun��es e argumentos
#Formas de especificar os argumentos de uma fun��o
runif(10)
runif(10,min=2,max=10)
runif(n=10,min=2,max=10)
runif(min=2,n=10,max=10) #Pode-se mudar a ordem desde que se coloque o nome dos argumentos...
runif(min=2,max=10,10)
runif(10,2,10)

#Saber os argumentos de uma fun��o
args(runif) #observar que o �nico argumento obrigat�rio � "n". Caso n�o especifique "min" e "max" � usado o default.

#Fun��o sample (gerar amostras aleat�rias de um vetor)
args(sample)

#Acessando ajuda (quando sabe-se o nome da fun��o)
help(sample)
?sample

#E quando eu n�o sei o nome da fun��o?
apropos("test")
help.search("test") #Apenas para os pacotes instalados no sistema...
help.start() #Manuais do R, Refer�ncias...

#Sites para busca:
#RDocumentation
#R Package Documentation
#R Contribuited Documentation

#29 pacotes j� vem instalados no R, por�m apenas 6 s�o carregados...

#Como saber os pacotes que est�o carregados?
search()

#Carregar um pacote..
library(caret)
search() #Verifica agora que o caret est� carregado!

#Para instalar um pacote..
install.packages("mvtnorm") #Esse ser� usado...
packageStatus()             #Tem q instalar. 

##Criando uma fun��o no R, por�m sem argumento...
ola.mundo <- function(){
  writeLines("Ol� mundo")
}

ola.mundo()

##Criando uma fun��o no R, com argumento...Estou dizendo para a fun��o escrever o texto fornecido a function.
ola.mundo <- function(texto){
  writeLines(texto)
}

ola.mundo(texto="alguma coisa")
ola.mundo("alguma coisa")


ola.mundo <- function(texto="Ol� mundo"){
  writeLines(texto)
}
ola.mundo()

#Exerc�cio 1
#1. Usando a fun��o runif() gere 30 n�meros aleat�rios entre:
#0 e 1
args(runif)
runif(n=30,min=0,max=1)
runif(30)

#-5 e 5
runif(n=30,min=-5,max=5)

#10 e 500
runif(n=30,min=10,max=500)

#alternando a posic�o dos argumentos da fun��o.

#2. Veja o help da fun��o (?)


#3. Crie uma fun��o para fazer a soma de dois n�meros: x e y
Soma <- function(x,y){
  soma<-x+y
  return(soma)
}

Soma #Aparece o c�digo fonte da fun��o...

Soma(3,4)
Soma(x=3,y=4)

#4. Crie uma fun��o para simular a jogada de um dado.
x<-seq(1:6)
Jog.dado <- function(x){
  sample(x, size=1, replace = T)
}

Jog.dado(x)

#Outra forma...
Jog.dado1 <- function(){
  sample.int(6, size = 1, replace = T)
}

Jog.dado1()

#5. Crie uma fun��o para simular a jogada de dois dados.
#Nesse caso, � obrigat�rio o uso da amostragem com reposi��o

Jog.dado2 <- function(){
  sample(x=1:6, size = 2, replace = T) 
}

#Define a quantidade de dados
Jog.dado3 <- function(n=2){
  sample(x=1:6, size = n, replace = T) 
}

Jog.dado3()
Jog.dado3(n=5) #Jogada de 5 dados...
Jog.dado3(n=8) #Jogada de 8 dados...


#Usando (...) em function (deixa a cargo do usu�rio especificar os demais argumentos da fun��o)
Jog.dado4 <- function(n=2,...){
  sample(x=1:6, size = n,...) 
}

Jog.dado4()
Jog.dado4(replace=T)

#Criando objetos
#Tudo no R � um objeto...
#Todo Objeto no R tem uma classe...
#As classes ser�o manipuladas atrav�s de m�todos definidos para a classe espec�fica...

#Gerenciando a �rea de trabalho...
ls #Observa os objetos criados
rm(dados) #remove um objeto espec�fico
rm(list=ls()) #remove todos os objetos

#OS SEIS TIPOS B�SICOS DE VETORES NO R (Vetores at�micos)
#double
#interger
#character
#logical
#complex
#raw

## Vetor recursivo
#list

## DOUBLE
x <- c(2,4,6)
typeof(x) #Qual tipo de vetor?
class(x) #Qual a classe?

## INTERGER
x<-c(2L,4L,6L)
typeof(x)
class(x)

## CHARACTER
x<- c("a","b","c")
typeof(x)
class(x)

## COMPLEX
x<- c(2+1i, 4+1i, 6+1i)
typeof(x)
class(x)

## RAW
x<- raw(3)
typeof(x)
class(x)
x

## Vetores Num�ricos
num<-c(10,5,2,4,8,9) #A fun��o concatenar automaticamente identifica com double, se n�o expressar que � inteiro...
typeof(num)
class(num)

#Converter p interger...
x<-c(10L,5L,2L,4L,8L,9L)

#Qual a importancia de diferenciar entre double e interger
object.size(num)
object.size(x)

#Reprodutibilidade
set.seed (123)
x <- runif(10)
x

#Perceber que os n�meros s�o gerados com 7 casas decimais depois da v�rgula...
getOption("digits") #Para vizualizar o n�mero de d�gitos

#No entanto, pode-se ter n�meros imprimidos com mais de 7, e isso tem haver com a precis�o que R considera...
print(x,digits=2) # n�o obedece
print(x,digits=7)
print(x,digits=22)

format(x, scientific = T)

#----------------------------------------------------------------------------------------------
##Commo gerar sequ�ncias e repeti��es de n�meros
#Func�o seq()
seq(1,10)
1:10
seq(1,10,by=2)

seq(1,10,length.out = 15) #Gera 15 n�meros igualmente espa�ados de 1 at� 10...
diff(seq(1,10,length.out = 15)) #Mostra a diferen�a entre os valores gerados...

#Func�o rep()
rep(1,10)
rep(c(1,2,3),times=5)
rep(c(1,2,3),each=5)
#----------------------------------------------------------------------------------------------
##Opera�oes matem�ticas com vetores
num #contem 6 elementos
num*2
num*num
num + c(2,4,1) #soma vetor num com 6 elementos com outro vetor de 3...O que acontece? O R recicla (Regra da Reciclagem) o vetor concatenado com 3 n�meros e soma aos 3 �ltimos n�meros...
num + c(2,4,1,3) # Da erro...Pq nesse caso o comprimento do maior vetor n�o � m�ltiplo do vetor de comprimento menor...

##Outros tipos de vetores
#CHARACTER
character<-c("a","b","c")
typeof(character)
class(character)

#L�GICO
logico<-character == "a"
logico

##Coer��o
w <- c(5L,"a") #Concatena��o de um inteiro + character...O que acontece?
typeof(w)
class(w)
# Todo vetor atomico no R s� pode ser de um tipo de objeto...
#Qual a regra? Se voc� tem qualquer coisa junto com character o vetor ser� transformado em um vetor de character...

y <- c(TRUE,2) #Concatena��o de um l�gico + n�mero...O que acontece?
typeof(y)
class(y)

z <- c("a",T) #Concatena��o de um character + l�gico...O que acontece?
typeof(z)
class(z)
z

x<-0:6
as.logical(x) #Zero vai sempre ser FALSE, qualquer outro n�mero vai ser TRUE...
as.character(x)
as.factor(x)

x <- c("a","b","c")
as.numeric(x) #N�o � poss�vel...
as.logical(x) #N�o � poss�vel...

##Valores perdidos e especiais dentro do R
perd <- c(3,5,NA,2) #O NA n�o interfere na classe do vetor. A fun��o � ocupar um espa�o e indicar que existe um dado perdido...
perd
typeof(perd)
class(perd)
is.na(perd) #Retorna uma resposta l�gica � existencia ou n�o de NA no vetor...
any(is.na(perd))

perd<-c(-1,0,1)/0
perd
#Inf = infinito, NaN = N�o � um n�mero
is.na(perd)
is.infinite(perd)


