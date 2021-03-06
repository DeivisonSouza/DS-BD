#############################LINGUAGEM DE PROGRAMA��O############################################
#################################################################################################
#4.3 Exerc�cios 2
#################################################################################################
#Armazene o resultado da equa��o 32+162???253 no objeto x
x<-32+162-253

#Divida x por 345 e armazene em y
y<-x/345

#Crie um objeto (com o nome que voc� quiser) para armazenar 30 valores aleat�rios de uma distribui��o uniforme entre 10 e 50
z<-runif(n=30, min=10, max=50)

#Remova o objeto y
rm(y)

#Remova os demais objetos de uma �nica vez
rm(list = ls())

#Procure a fun��o utilizada para gerar numeros aleat�rios de uma distribui��o de Poisson, e gere #100 valores para a VA X???Poisson(5).
apropos("Poisson") #Procura nome da fun��o
help.search("Poisson") #Procura termos na p�gina de ajuda
?rpois
#Pacote stats...
rpois(n = 100, lambda = 5) #Encontrei na web

#################################################################################################
#5.5 Exerc�cios 3
#################################################################################################
#Crie um objeto com os valores 54, 0, 17, 94, 12.5, 2, 0.9, 15.
x<-c(54, 0, 17, 94, 12.5, 2, 0.9, 15)
length(x)

#Some o objeto acima com os valores 5, 6, e depois com os valores 5, 6, 7.
x+c(5, 6) #Executa a regra da reciclagem...
x+c(5, 6, 7) #Mensagem de erro, pois o maior vetor n�o � multiplo do menor...

#Construa um �nico objeto com as letras: A, B, e C, repetidas cada uma 15, 12, e 8 vezes, #respectivamente.
?rep
y<-rep(LETTERS[1:3], times=c(15,12,8))
y<-rep(c("A", "B", "C"),times=c(15,12,8))

#Mostre na tela, em forma de verdadeiro ou falso, onde est�o as letras B nesse objeto.
y == "B"

#Veja a p�gina de ajuda da fun��o sum() e descubra como fazer para contar o n�mero de letras B #neste vetor (usando sum()).
?sum
#Logical true values are regarded as one, false values as zero. For historical reasons, NULL is #accepted and treated as if it were integer(0).
sum(y == "B") #Soma de vetores l�gicos...

#Crie um objeto com 100 valores aleat�rios de uma distribui��o uniforme U(0,1). Conte quantas #vezes aparecem valores maiores ou iguais a 0,5.
k<-runif(n=100, min=0, max=1)
k<-runif(100)
table(k>=0.5)
sum(k>=0.5)

#Calcule as 50 primeiras pot�ncias de 2, ou seja, 2,2^2,2^3,.,2^50.
H<-seq(1,50,1)
int <-1:50
G<-print(signif(2^H), 1)

#Calcule o quadrado dos n�meros inteiros de 1 a 50, ou seja, 1^2,2^2,3^2,.,50^2.
L<-H^2

#Quais pares s�o iguais, ou seja, quais n�meros inteiros dos dois exerc�cios anteriores satisfaze#m a condi��o 2n=n2?
G==L

#Quantos pares existem?
table(G==L)
sum(G == L)

#Quais s�o estes vetores?
G[G == L]

#Saber a posi��o?
which(G == L)

#Calcule o seno, coseno e a tangente para os n�meros variando de 0 a 2Pi, com dist�ncia de 0.1 #entre eles. (Use as fun��es sin(), cos(), tan()).
?sin
x <- seq(0, 2*pi, by = 0.1)
sin(x)
cos(x)
tang1<-tan(x)

#Calcule a tangente usando a rela��o tan(x)=sin(x)/cos(x).
tang<-sin(x)/cos(x)
tang

#Calcule as diferen�as das tangentes calculadas pela fun��o do R e pela raz�o acima.
diff(tan(x)-tang)

#Quais valores s�o exatamente iguais?
identical(tan(x),tang) #Mais rigorosa do que all.equal(), testa todas as casas decimais.
tan(x)==tang
table(tan(x)==tang)
tang1[tang1 == tang]

#Qual a diferen�a m�xima (em m�dulo) entre eles? Qual � a causa dessa diferen�a?
abs(tan(x)-tang)
max(abs(tan(x)-tang))
