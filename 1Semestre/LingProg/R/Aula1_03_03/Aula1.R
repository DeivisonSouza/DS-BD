#Linguagem de Programação#
##########################
#R básico
#Estrutura de controles
#Leitura e manipulação de dados I
#Visualização I
#Leitura e manipulação de dados II

#Referência base: R for Data Science e Hands-On Programing with R (introdução básica)

#------------------------------------------------------------------------------------------------------------
##Funções e argumentos
#Formas de especificar os argumentos de uma função
runif(10)
runif(10,min=2,max=10)
runif(n=10,min=2,max=10)
runif(min=2,n=10,max=10) #Pode-se mudar a ordem desde que se coloque o nome dos argumentos...
runif(min=2,max=10,10)
runif(10,2,10)

#Saber os argumentos de uma função
args(runif) #observar que o único argumento obrigatório é "n". Caso não especifique "min" e "max" é usado o default.

#Função sample (gerar amostras aleatórias de um vetor)
args(sample)

#Acessando ajuda (quando sabe-se o nome da função)
help(sample)
?sample

#E quando eu não sei o nome da função?
apropos("test")
help.search("test") #Apenas para os pacotes instalados no sistema...
help.start() #Manuais do R, Referências...

#Sites para busca:
#RDocumentation
#R Package Documentation
#R Contribuited Documentation

#29 pacotes já vem instalados no R, porém apenas 6 são carregados...

#Como saber os pacotes que estão carregados?
search()

#Carregar um pacote..
library(caret)
search() #Verifica agora que o caret está carregado!

#Para instalar um pacote..
install.packages("mvtnorm") #Esse será usado...
packageStatus()             #Tem q instalar. 

##Criando uma função no R, porém sem argumento...
ola.mundo <- function(){
  writeLines("Olá mundo")
}

ola.mundo()

##Criando uma função no R, com argumento...Estou dizendo para a função escrever o texto fornecido a function.
ola.mundo <- function(texto){
  writeLines(texto)
}

ola.mundo(texto="alguma coisa")
ola.mundo("alguma coisa")


ola.mundo <- function(texto="Olá mundo"){
  writeLines(texto)
}
ola.mundo()

#Exercício 1
#1. Usando a função runif() gere 30 números aleatórios entre:
#0 e 1
args(runif)
runif(n=30,min=0,max=1)
runif(30)

#-5 e 5
runif(n=30,min=-5,max=5)

#10 e 500
runif(n=30,min=10,max=500)

#alternando a posicão dos argumentos da função.

#2. Veja o help da função (?)


#3. Crie uma função para fazer a soma de dois números: x e y
Soma <- function(x,y){
  soma<-x+y
  return(soma)
}

Soma #Aparece o código fonte da função...

Soma(3,4)
Soma(x=3,y=4)

#4. Crie uma função para simular a jogada de um dado.
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

#5. Crie uma função para simular a jogada de dois dados.
#Nesse caso, é obrigatório o uso da amostragem com reposição

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


#Usando (...) em function (deixa a cargo do usuário especificar os demais argumentos da função)
Jog.dado4 <- function(n=2,...){
  sample(x=1:6, size = n,...) 
}

Jog.dado4()
Jog.dado4(replace=T)

#Criando objetos
#Tudo no R é um objeto...
#Todo Objeto no R tem uma classe...
#As classes serão manipuladas através de métodos definidos para a classe específica...

#Gerenciando a área de trabalho...
ls #Observa os objetos criados
rm(dados) #remove um objeto específico
rm(list=ls()) #remove todos os objetos

#OS SEIS TIPOS BÁSICOS DE VETORES NO R (Vetores atômicos)
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

## Vetores Numéricos
num<-c(10,5,2,4,8,9) #A função concatenar automaticamente identifica com double, se não expressar que é inteiro...
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

#Perceber que os números são gerados com 7 casas decimais depois da vírgula...
getOption("digits") #Para vizualizar o número de dígitos

#No entanto, pode-se ter números imprimidos com mais de 7, e isso tem haver com a precisão que R considera...
print(x,digits=2) # não obedece
print(x,digits=7)
print(x,digits=22)

format(x, scientific = T)

#----------------------------------------------------------------------------------------------
##Commo gerar sequências e repetições de números
#Funcão seq()
seq(1,10)
1:10
seq(1,10,by=2)

seq(1,10,length.out = 15) #Gera 15 números igualmente espaçados de 1 até 10...
diff(seq(1,10,length.out = 15)) #Mostra a diferença entre os valores gerados...

#Funcão rep()
rep(1,10)
rep(c(1,2,3),times=5)
rep(c(1,2,3),each=5)
#----------------------------------------------------------------------------------------------
##Operaçoes matemáticas com vetores
num #contem 6 elementos
num*2
num*num
num + c(2,4,1) #soma vetor num com 6 elementos com outro vetor de 3...O que acontece? O R recicla (Regra da Reciclagem) o vetor concatenado com 3 números e soma aos 3 últimos números...
num + c(2,4,1,3) # Da erro...Pq nesse caso o comprimento do maior vetor não é múltiplo do vetor de comprimento menor...

##Outros tipos de vetores
#CHARACTER
character<-c("a","b","c")
typeof(character)
class(character)

#LÓGICO
logico<-character == "a"
logico

##Coerção
w <- c(5L,"a") #Concatenação de um inteiro + character...O que acontece?
typeof(w)
class(w)
# Todo vetor atomico no R só pode ser de um tipo de objeto...
#Qual a regra? Se você tem qualquer coisa junto com character o vetor será transformado em um vetor de character...

y <- c(TRUE,2) #Concatenação de um lógico + número...O que acontece?
typeof(y)
class(y)

z <- c("a",T) #Concatenação de um character + lógico...O que acontece?
typeof(z)
class(z)
z

x<-0:6
as.logical(x) #Zero vai sempre ser FALSE, qualquer outro número vai ser TRUE...
as.character(x)
as.factor(x)

x <- c("a","b","c")
as.numeric(x) #Não é possível...
as.logical(x) #Não é possível...

##Valores perdidos e especiais dentro do R
perd <- c(3,5,NA,2) #O NA não interfere na classe do vetor. A função é ocupar um espaço e indicar que existe um dado perdido...
perd
typeof(perd)
class(perd)
is.na(perd) #Retorna uma resposta lógica à existencia ou não de NA no vetor...
any(is.na(perd))

perd<-c(-1,0,1)/0
perd
#Inf = infinito, NaN = Não é um número
is.na(perd)
is.infinite(perd)


