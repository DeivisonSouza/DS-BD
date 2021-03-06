#############################################################################################
##### BOOTSTRAP PARA ESTIMATIVA DA M�DIA POPULACIONAL #####
##Autor: Deivison Venicio Souza##
#############################################################################################
# O bootstrap � uma t�cnica de estima��o de par�metros desconhecidos de uma popula��o, baseada 
# em amostragem aleat�ria com reposi��o. A ideia � relativamente simples: com a amostra
# observada (de tamanho n), fazemos um grande n�mero (r) de reamostragens (e.g. 1000) com 
# reposi��o de tamanho m ??? n, e calculamos a estat�stica de interesse. Dessa forma, teremos
# r estimativas diferentes da estat�stica que temos interesse, e a partir disso, podemos obter
# a distribui��o amostral dessa estat�stica e outras medidas pontuais (e.g. m�dia, mediana),
# e medidas de varia��o (e.g. vari�ncia, intervalos de confian�a)
#############################################################################################
########## OBJETIVO ##########
#O objetivo deste exerc�cio � programar um bootstrap para a estimativa da m�dia de uma popula��o
#simulada. O objetivo � programar o m�todo sem ser rigoroso com os detalhes te�ricos.
#############################################################################################

## Insira aqui o n�mero da sua matr�cula para fixar uma semente
matricula <- 1016323       # O n�mero de matricula �: 40001016323E1. Por�m, quando tentei usar
                           # o n�mero completo (sem E1) set.seed n�o aceitou.Por isso, mantive
                           # a parte final do n�mero apenas...
                           

#############################################################################################
## Gera 1 milh�o de n�meros aleat�rios de uma distribui��o normal (popula��o)
#############################################################################################
set.seed(matricula)                                  # Semente de reprodutibilidade

pop <- rnorm(n = 1e6, mean = 100, sd = sqrt(200))    # m�dia = 100 e sd = sqrt(200)

#############################################################################################
## Histograma da distribui��o da populacional...
#############################################################################################
hist.pop<-hist(pop, main = "Histograma", 
               xlab = "Distribui��o populacional",
               ylab = "Frequ�ncia",
               col = "peachpuff")

#Ajusta um curva normal aos dados populacionais...
xfit<-seq(min(pop),max(pop))                      #Sequ�ncia com base na popula��o...
yfit<-dnorm(xfit,mean=mean(pop),sd=sd(pop))       # Ajusta uma curva normal aos dados populacionais.
yfit <- yfit*diff(hist.pop$mids[1:2])*length(pop) # estima  os valores p/ ponto m�dio...
lines(xfit, yfit, lwd=2, col="red", lty=2, type="l") #adiciona curva normal ao histograma...

# Insere linhas verticais identificadoras dos par�metros da popula��o..
abline(v = mean(pop), col = "forestgreen",lwd =3)      # M�dia populacional
abline(v = mean(pop)+1.96*sd(pop), col = "blue",lwd =3) # M�dia +1,96 desvios
abline(v = mean(pop)-1.96*sd(pop), col = "blue",lwd =3) # M�dia -1,96 desvios

#############################################################################################
## Retira uma amostra aleat�ria de tamanho n = 1000 da popula��o
#############################################################################################
am <- pop[sample(1:length(pop), size = 1000)]

#############################################################################################
# Histograma da distribui��o amostral...
#############################################################################################
hist.a<-hist(am, main = "Histograma", 
             xlab = "Distribui��o amostral",
             ylab = "Frequ�ncia",
             col ="slategray1",
             ylim=c(0,300))

#Ajusta um curva normal � distribui��o amostral...
xfit.am<-seq(min(am),max(am))                        # Sequ�ncia com base na amostra...
yfit.am<-dnorm(xfit.am,mean=mean(am),sd=sd(am))      # Ajusta uma curva normal aos dados amostrais...
yfit.am <- yfit.am*diff(hist.a$mids[1:2])*length(am) # estima  os valores p/ ponto m�dio...
lines(xfit.am, yfit.am, lwd=2, col="red", lty=2, type="l") # adiciona curva normal ao histograma...

# Insere linhas verticais identificadoras dos par�metros da popula��o..
abline(v = mean(am), col = "red",lwd =1, lty=3)     # M�dia amostra
abline(v = mean(pop), col = "forestgreen",lwd =1)   # M�dia populacional

legend(120,300,legend="M�dia (amostra)",col="red",lwd=1,lty=3,bty="n")
legend(120,280,legend="M�dia (Popula��o)",col="forestgreen",lwd=1,bty="n")

#############################################################################################
##BOOTSTRAP - Elementos essenciais
#############################################################################################
#Para ambos exerc�cios, voc� deve fazer/criar:
# 1. Um objeto com a classe e dimens�o apropriados para armazenar os resultados de cada reamostragem.
# 2. Uma fun��o que calcule a diferen�a absoluta entre dois n�meros, e us�-la para calcular as diferen�as 
# entre a m�dia da popula��o (verdadeira) e as m�dias obtidas via bootstrap.
# 3. Um histograma das r estimativas (para cada valor de m no exerc�cio 2), com uma linha vertical 
# indicando a m�dia populacional (verdadeira).

#############################################################################################
##EXERC�CIO 1
#############################################################################################
#Algoritmo geral para desenvolver o m�todo, considerando o tamanho da amostra (m) fixo:
# 1. Com os dados da amostra, gere uma nova amostra aleat�ria (com reposi��o) de tamanho m = 500.
# 2. Calcule a m�dia dessa nova amostra.
# 3. Repita esse procedimento r = 100000 vezes.
# 4. Fa�a um histograma das r estimativas, calcule a m�dia e compare com a m�dia verdadeira.
#############################################################################################
#------------------------------------------------------------------------------------------
# 1. Gera uma amostra aleat�ria com repeti��o com base na amostra "am"...
set.seed(500)
am.1 <- am[sample(x=1:length(am), size = 500, replace = TRUE)]

#------------------------------------------------------------------------------------------
# 2. M�dia da nova amostra "am.1"...
mean(am.1)

#------------------------------------------------------------------------------------------
# 3. Repetindo o procedimento r = 100000 vezes...
r<-100000                # Cria um vetor de tamanho igual a 100000...
list.boot<-list()        # Cria uma lista vazia para receber dados da reamostragem...

for(i in 1:r){ #Cria um sequencia de inteiros ao longo do vetor r...
  #gera 100000 amostras aleatorias da amostra "am" (tamanho = 500) com reposi��o 
  #e adiciona na lista vazia...
  set.seed(i)
  list.boot[[i]] <- am[sample(x=1:length(am), size = 500, replace = TRUE)]
}

#------------------------------------------------------------------------------------------
# Cria uma fun��o p/ calcular m�dia de cada bootstrap e adiciona no vetor "resultados"...

funcao.boot <- function(x, funcao){  # fun��o que recebe um objeto e uma fun��o espec�fica...
  
  n <- length(x)                     # cria um vetor de comprimento igual ao do objeto...
  resultado <- numeric(n)            # cria um vetor para armazenar resultados das m�dias bootstrap...
  names(resultado) <- paste0("boot", 1:100000) # imprime nomes �s m�dias do bootstrap...
  
  for(i in seq_along(x)){            # cria um vetor de inteiros para acompanhar o objeto bootstrap...
    # aplica uma fun��o espec�fica a cada elemento do objeto e armazena no vetor de resultados...
    resultado[i] <- funcao(x[[i]])
  }
  return(resultado)                  # Quando aplicada a "funcao.boot" ser� retornado o vetor "resultados"
}

# calcula a m�dia p/ cada amostra bootstrap...
mean.boot<-funcao.boot(list.boot, mean)

# calcula diferen�a absoluta entre cada m�dia bootstrap e a m�dia populacional...
abs(mean.boot - mean(pop))

#------------------------------------------------------------------------------------------
# 4. Fa�a um histograma das r estimativas, calcule a m�dia e compare com a m�dia verdadeira.
h.boot<-hist(mean.boot, main = "Histrograma da distribui��o \n bootstrap",
             xlab = "Estimativas da m�dia",
             ylab = "Frequ�ncia",
             col ="whitesmoke") # Cria um histograma das m�dias estimadas na distribui��o bootstrap...
abline(v = mean(mean.boot), col = "red",lwd = 2, lty=1) # Adiciona a m�dia da dist. bootstrap...
abline(v = mean(pop), col = "blue",lwd = 2, lty=1) # Adiciona m�dia da popula��o...

legend(100.5,30000,legend="M�dia (bootstrap)",col="red",lwd=2,lty=1,bty="n")
legend(100.5,28000,legend="M�dia (Popula��o)",col="blue",lwd=2,lty=1,bty="n")

#############################################################################################
##EXERC�CIO 2
#############################################################################################
#Algoritmo geral para desenvolver o m�todo, considerando o tamanho da amostra (m) 
#variando entre quatro valores diferentes:

# 1. Com os dados da amostra, gere uma nova amostra aleat�ria (com reposi��o) com tamanhos:
# m = 100, 300, 500, 700.
# 2. Calcule a m�dia dessa nova amostra, para cada valor de m.
# 3. Repita esse procedimento r = 100000 vezes, para cada valor de m.
# 4. Fa�a um histograma das r estimativas, calcule a m�dia e compare com a m�dia verdadeira
# (para cada valor de m).
#############################################################################################
r2 <- 100000                      # Cria um vetor de tamanho igual a 100000...

# Cria uma lista "list.boot2" com 4 outras listas para que cada uma receba dados da reamostragem
# de tamanho m (100, 300, 500, 700)...
list.boot2 <- list(m100 = list(), m300 = list(), m500 = list(), m700 = list())

for(i in 1:r2){ # Cria um sequencia de inteiros ao longo do vetor r2...
  # gera 400000 amostras aleatorias da amostra "am" (tamanhos = 100, 300, 500, 7000)
  # com reposi��o e adiciona os resultados as diferentes listas conforme o tamanho m...
  list.boot2$m100[[i]] <- am[sample(x=1:length(am), size = 100, replace = TRUE)]
  list.boot2$m300[[i]] <- am[sample(x=1:length(am), size = 300, replace = TRUE)]
  list.boot2$m500[[i]] <- am[sample(x=1:length(am), size = 500, replace = TRUE)]
  list.boot2$m700[[i]] <- am[sample(x=1:length(am), size = 700, replace = TRUE)]
}

#------------------------------------------------------------------------------------------
# Cria uma fun��o p/ calcular m�dia de cada bootstrap, em fun��o de cada tamanho "m", e adiciona em "resultados2"...

funcao.boot2 <- function(x, funcao){     # fun��o que recebe um objeto e uma fun��o espec�fica...
resultado2 <- vector('list', length(x))  # cria um objeto com 4 listas...
names(resultado2) <- names(x)

for(i in seq_along(x)){                  # cria um vetor de inteiros para acompanhar as 4 listas dentro do objeto...
  for(j in seq_along(x[[i]])){           # cria um vetor de inteiros para acompanhar cada amostra bootstrap dentro de cada tamanho "m"...
    # aplica uma fun��o espec�fica a cada amostra bootstrap de tamano "m" e armazena separadamente no vetor "resultados2"...
    resultado2[[i]][[j]] <- funcao(x[[i]][[j]])
  }
}
return(resultado2)                 
}

# calcula a m�dia p/ cada amostra bootstrap para cada tamanho "m"
mean.boot2<-funcao.boot2(list.boot2, mean)

#-----------------------------------------------------------------------------------------------------------
# 4. Fa�a um histograma das r estimativas, calcule a m�dia e compare com a m�dia verdadeira
# (para cada valor de m).
#-----------------------------------------------------------------------------------------------------------
layout(matrix(c(1, 2,
                3, 4),
              nr=2, byrow=T))
layout.show(n=4)

# Histograma para amostras bootstrap de tamanho igual a 100....
h.m100 <- hist(mean.boot2$m100, main = "Histrograma da distribui��o bootstrap \n (Size = 100)",
             xlab = "Estimativas da m�dia",
             ylab = "Frequ�ncia",
             col ="whitesmoke",
             ylim=c(0,16000))

abline(v = mean(mean.boot2$m100), col = "red",lwd = 2, lty=1) # Adiciona a m�dia da dist. bootstrap...
abline(v = mean(pop), col = "blue",lwd = 2, lty=1) # Adiciona m�dia da popula��o...

legend(93,16000,legend="M�dia (bootstrap)",col="red",lwd=2,lty=1,bty="n")
legend(93,15000,legend="M�dia (Popula��o)",col="blue",lwd=2,lty=1,bty="n")

#-----------------------------------------------------------------------------------------------------------
# Histograma para amostras bootstrap de tamanho igual a 300....
h.m300 <- hist(mean.boot2$m300, main = "Histrograma da distribui��o bootstrap \n (Size = 300)",
               xlab = "Estimativas da m�dia",
               ylab = "Frequ�ncia",
               col ="whitesmoke",
               ylim=c(0,25000))

abline(v = mean(mean.boot2$m300), col = "red",lwd = 2, lty=1) # Adiciona a m�dia da dist. bootstrap...
abline(v = mean(pop), col = "blue",lwd = 2, lty=1) # Adiciona m�dia da popula��o...

legend(95,25000,legend="M�dia (bootstrap)",col="red",lwd=2,lty=1,bty="n")
legend(95,23500,legend="M�dia (Popula��o)",col="blue",lwd=2,lty=1,bty="n")

#-----------------------------------------------------------------------------------------------------------
# Histograma para amostras bootstrap de tamanho igual a 500....
h.m500 <- hist(mean.boot2$m500, main = "Histrograma da distribui��o bootstrap \n (Size = 500)",
               xlab = "Estimativas da m�dia",
               ylab = "Frequ�ncia",
               col ="whitesmoke") # Cria um histograma das m�dias estimadas na distribui��o bootstrap...
abline(v = mean(mean.boot2$m500), col = "red",lwd = 2, lty=1) # Adiciona a m�dia da dist. bootstrap...
abline(v = mean(pop), col = "blue",lwd = 2, lty=1) # Adiciona m�dia da popula��o...

legend(96.3,30000,legend="M�dia (bootstrap)",col="red",lwd=2,lty=1,bty="n")
legend(96.3,28000,legend="M�dia (Popula��o)",col="blue",lwd=2,lty=1,bty="n")

#-----------------------------------------------------------------------------------------------------------
# Histograma para amostras bootstrap de tamanho igual a 700....
h.m700 <- hist(mean.boot2$m700, main = "Histrograma da distribui��o bootstrap \n (Size = 700)",
               xlab = "Estimativas da m�dia",
               ylab = "Frequ�ncia",
               col ="whitesmoke") # Cria um histograma das m�dias estimadas na distribui��o bootstrap...
abline(v = mean(mean.boot2$m700), col = "red",lwd = 2, lty=1) # Adiciona a m�dia da dist. bootstrap...
abline(v = mean(pop), col = "blue",lwd = 2, lty=1) # Adiciona m�dia da popula��o...

legend(96.5,15000,legend="M�dia (bootstrap)",col="red",lwd=2,lty=1,bty="n")
legend(96.5,14000,legend="M�dia (Popula��o)",col="blue",lwd=2,lty=1,bty="n")

#-----------------------------------------------------------------------------------------------------------
#END.......
#-----------------------------------------------------------------------------------------------------------
