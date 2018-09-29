#############################LINGUAGEM DE PROGRAMAÇÃO############################################
#################################################################################################
#4.3 Exercícios 2
#################################################################################################
#Armazene o resultado da equação 32+162???253 no objeto x
x<-32+162-253

#Divida x por 345 e armazene em y
y<-x/345

#Crie um objeto (com o nome que você quiser) para armazenar 30 valores aleatórios de uma distribuição uniforme entre 10 e 50
z<-runif(n=30, min=10, max=50)

#Remova o objeto y
rm(y)

#Remova os demais objetos de uma única vez
rm(list = ls())

#Procure a função utilizada para gerar numeros aleatórios de uma distribuição de Poisson, e gere #100 valores para a VA X???Poisson(5).
apropos("Poisson") #Procura nome da função
help.search("Poisson") #Procura termos na página de ajuda
?rpois
#Pacote stats...
rpois(n = 100, lambda = 5) #Encontrei na web

#################################################################################################
#5.5 Exercícios 3
#################################################################################################
#Crie um objeto com os valores 54, 0, 17, 94, 12.5, 2, 0.9, 15.
x<-c(54, 0, 17, 94, 12.5, 2, 0.9, 15)
length(x)

#Some o objeto acima com os valores 5, 6, e depois com os valores 5, 6, 7.
x+c(5, 6) #Executa a regra da reciclagem...
x+c(5, 6, 7) #Mensagem de erro, pois o maior vetor não é multiplo do menor...

#Construa um único objeto com as letras: A, B, e C, repetidas cada uma 15, 12, e 8 vezes, #respectivamente.
?rep
y<-rep(LETTERS[1:3], times=c(15,12,8))
y<-rep(c("A", "B", "C"),times=c(15,12,8))

#Mostre na tela, em forma de verdadeiro ou falso, onde estão as letras B nesse objeto.
y == "B"

#Veja a página de ajuda da função sum() e descubra como fazer para contar o número de letras B #neste vetor (usando sum()).
?sum
#Logical true values are regarded as one, false values as zero. For historical reasons, NULL is #accepted and treated as if it were integer(0).
sum(y == "B") #Soma de vetores lógicos...

#Crie um objeto com 100 valores aleatórios de uma distribuição uniforme U(0,1). Conte quantas #vezes aparecem valores maiores ou iguais a 0,5.
k<-runif(n=100, min=0, max=1)
k<-runif(100)
table(k>=0.5)
sum(k>=0.5)

#Calcule as 50 primeiras potências de 2, ou seja, 2,2^2,2^3,.,2^50.
H<-seq(1,50,1)
int <-1:50
G<-print(signif(2^H), 1)

#Calcule o quadrado dos números inteiros de 1 a 50, ou seja, 1^2,2^2,3^2,.,50^2.
L<-H^2

#Quais pares são iguais, ou seja, quais números inteiros dos dois exercícios anteriores satisfaze#m a condição 2n=n2?
G==L

#Quantos pares existem?
table(G==L)
sum(G == L)

#Quais são estes vetores?
G[G == L]

#Saber a posição?
which(G == L)

#Calcule o seno, coseno e a tangente para os números variando de 0 a 2Pi, com distância de 0.1 #entre eles. (Use as funções sin(), cos(), tan()).
?sin
x <- seq(0, 2*pi, by = 0.1)
sin(x)
cos(x)
tang1<-tan(x)

#Calcule a tangente usando a relação tan(x)=sin(x)/cos(x).
tang<-sin(x)/cos(x)
tang

#Calcule as diferenças das tangentes calculadas pela função do R e pela razão acima.
diff(tan(x)-tang)

#Quais valores são exatamente iguais?
identical(tan(x),tang) #Mais rigorosa do que all.equal(), testa todas as casas decimais.
tan(x)==tang
table(tan(x)==tang)
tang1[tang1 == tang]

#Qual a diferença máxima (em módulo) entre eles? Qual é a causa dessa diferença?
abs(tan(x)-tang)
max(abs(tan(x)-tang))
