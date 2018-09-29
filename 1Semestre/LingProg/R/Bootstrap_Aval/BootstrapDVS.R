#############################################################################################
##### BOOTSTRAP PARA ESTIMATIVA DA MÉDIA POPULACIONAL #####
##Autor: Deivison Venicio Souza##
#############################################################################################
# O bootstrap é uma técnica de estimação de parâmetros desconhecidos de uma população, baseada 
# em amostragem aleatória com reposição. A ideia é relativamente simples: com a amostra
# observada (de tamanho n), fazemos um grande número (r) de reamostragens (e.g. 1000) com 
# reposição de tamanho m ??? n, e calculamos a estatística de interesse. Dessa forma, teremos
# r estimativas diferentes da estatística que temos interesse, e a partir disso, podemos obter
# a distribuição amostral dessa estatística e outras medidas pontuais (e.g. média, mediana),
# e medidas de variação (e.g. variância, intervalos de confiança)
#############################################################################################
########## OBJETIVO ##########
#O objetivo deste exercício é programar um bootstrap para a estimativa da média de uma população
#simulada. O objetivo é programar o método sem ser rigoroso com os detalhes teóricos.
#############################################################################################

## Insira aqui o número da sua matrícula para fixar uma semente
matricula <- 1016323       # O número de matricula é: 40001016323E1. Porém, quando tentei usar
                           # o número completo (sem E1) set.seed não aceitou.Por isso, mantive
                           # a parte final do número apenas...
                           

#############################################################################################
## Gera 1 milhão de números aleatórios de uma distribuição normal (população)
#############################################################################################
set.seed(matricula)                                  # Semente de reprodutibilidade

pop <- rnorm(n = 1e6, mean = 100, sd = sqrt(200))    # média = 100 e sd = sqrt(200)

#############################################################################################
## Histograma da distribuição da populacional...
#############################################################################################
hist.pop<-hist(pop, main = "Histograma", 
               xlab = "Distribuição populacional",
               ylab = "Frequência",
               col = "peachpuff")

#Ajusta um curva normal aos dados populacionais...
xfit<-seq(min(pop),max(pop))                      #Sequência com base na população...
yfit<-dnorm(xfit,mean=mean(pop),sd=sd(pop))       # Ajusta uma curva normal aos dados populacionais.
yfit <- yfit*diff(hist.pop$mids[1:2])*length(pop) # estima  os valores p/ ponto médio...
lines(xfit, yfit, lwd=2, col="red", lty=2, type="l") #adiciona curva normal ao histograma...

# Insere linhas verticais identificadoras dos parâmetros da população..
abline(v = mean(pop), col = "forestgreen",lwd =3)      # Média populacional
abline(v = mean(pop)+1.96*sd(pop), col = "blue",lwd =3) # Média +1,96 desvios
abline(v = mean(pop)-1.96*sd(pop), col = "blue",lwd =3) # Média -1,96 desvios

#############################################################################################
## Retira uma amostra aleatória de tamanho n = 1000 da população
#############################################################################################
am <- pop[sample(1:length(pop), size = 1000)]

#############################################################################################
# Histograma da distribuição amostral...
#############################################################################################
hist.a<-hist(am, main = "Histograma", 
             xlab = "Distribuição amostral",
             ylab = "Frequência",
             col ="slategray1",
             ylim=c(0,300))

#Ajusta um curva normal à distribuição amostral...
xfit.am<-seq(min(am),max(am))                        # Sequência com base na amostra...
yfit.am<-dnorm(xfit.am,mean=mean(am),sd=sd(am))      # Ajusta uma curva normal aos dados amostrais...
yfit.am <- yfit.am*diff(hist.a$mids[1:2])*length(am) # estima  os valores p/ ponto médio...
lines(xfit.am, yfit.am, lwd=2, col="red", lty=2, type="l") # adiciona curva normal ao histograma...

# Insere linhas verticais identificadoras dos parâmetros da população..
abline(v = mean(am), col = "red",lwd =1, lty=3)     # Média amostra
abline(v = mean(pop), col = "forestgreen",lwd =1)   # Média populacional

legend(120,300,legend="Média (amostra)",col="red",lwd=1,lty=3,bty="n")
legend(120,280,legend="Média (População)",col="forestgreen",lwd=1,bty="n")

#############################################################################################
##BOOTSTRAP - Elementos essenciais
#############################################################################################
#Para ambos exercícios, você deve fazer/criar:
# 1. Um objeto com a classe e dimensão apropriados para armazenar os resultados de cada reamostragem.
# 2. Uma função que calcule a diferença absoluta entre dois números, e usá-la para calcular as diferenças 
# entre a média da população (verdadeira) e as médias obtidas via bootstrap.
# 3. Um histograma das r estimativas (para cada valor de m no exercício 2), com uma linha vertical 
# indicando a média populacional (verdadeira).

#############################################################################################
##EXERCÍCIO 1
#############################################################################################
#Algoritmo geral para desenvolver o método, considerando o tamanho da amostra (m) fixo:
# 1. Com os dados da amostra, gere uma nova amostra aleatória (com reposição) de tamanho m = 500.
# 2. Calcule a média dessa nova amostra.
# 3. Repita esse procedimento r = 100000 vezes.
# 4. Faça um histograma das r estimativas, calcule a média e compare com a média verdadeira.
#############################################################################################
#------------------------------------------------------------------------------------------
# 1. Gera uma amostra aleatória com repetição com base na amostra "am"...
set.seed(500)
am.1 <- am[sample(x=1:length(am), size = 500, replace = TRUE)]

#------------------------------------------------------------------------------------------
# 2. Média da nova amostra "am.1"...
mean(am.1)

#------------------------------------------------------------------------------------------
# 3. Repetindo o procedimento r = 100000 vezes...
r<-100000                # Cria um vetor de tamanho igual a 100000...
list.boot<-list()        # Cria uma lista vazia para receber dados da reamostragem...

for(i in 1:r){ #Cria um sequencia de inteiros ao longo do vetor r...
  #gera 100000 amostras aleatorias da amostra "am" (tamanho = 500) com reposição 
  #e adiciona na lista vazia...
  set.seed(i)
  list.boot[[i]] <- am[sample(x=1:length(am), size = 500, replace = TRUE)]
}

#------------------------------------------------------------------------------------------
# Cria uma função p/ calcular média de cada bootstrap e adiciona no vetor "resultados"...

funcao.boot <- function(x, funcao){  # função que recebe um objeto e uma função específica...
  
  n <- length(x)                     # cria um vetor de comprimento igual ao do objeto...
  resultado <- numeric(n)            # cria um vetor para armazenar resultados das médias bootstrap...
  names(resultado) <- paste0("boot", 1:100000) # imprime nomes às médias do bootstrap...
  
  for(i in seq_along(x)){            # cria um vetor de inteiros para acompanhar o objeto bootstrap...
    # aplica uma função específica a cada elemento do objeto e armazena no vetor de resultados...
    resultado[i] <- funcao(x[[i]])
  }
  return(resultado)                  # Quando aplicada a "funcao.boot" será retornado o vetor "resultados"
}

# calcula a média p/ cada amostra bootstrap...
mean.boot<-funcao.boot(list.boot, mean)

# calcula diferença absoluta entre cada média bootstrap e a média populacional...
abs(mean.boot - mean(pop))

#------------------------------------------------------------------------------------------
# 4. Faça um histograma das r estimativas, calcule a média e compare com a média verdadeira.
h.boot<-hist(mean.boot, main = "Histrograma da distribuição \n bootstrap",
             xlab = "Estimativas da média",
             ylab = "Frequência",
             col ="whitesmoke") # Cria um histograma das médias estimadas na distribuição bootstrap...
abline(v = mean(mean.boot), col = "red",lwd = 2, lty=1) # Adiciona a média da dist. bootstrap...
abline(v = mean(pop), col = "blue",lwd = 2, lty=1) # Adiciona média da população...

legend(100.5,30000,legend="Média (bootstrap)",col="red",lwd=2,lty=1,bty="n")
legend(100.5,28000,legend="Média (População)",col="blue",lwd=2,lty=1,bty="n")

#############################################################################################
##EXERCÍCIO 2
#############################################################################################
#Algoritmo geral para desenvolver o método, considerando o tamanho da amostra (m) 
#variando entre quatro valores diferentes:

# 1. Com os dados da amostra, gere uma nova amostra aleatória (com reposição) com tamanhos:
# m = 100, 300, 500, 700.
# 2. Calcule a média dessa nova amostra, para cada valor de m.
# 3. Repita esse procedimento r = 100000 vezes, para cada valor de m.
# 4. Faça um histograma das r estimativas, calcule a média e compare com a média verdadeira
# (para cada valor de m).
#############################################################################################
r2 <- 100000                      # Cria um vetor de tamanho igual a 100000...

# Cria uma lista "list.boot2" com 4 outras listas para que cada uma receba dados da reamostragem
# de tamanho m (100, 300, 500, 700)...
list.boot2 <- list(m100 = list(), m300 = list(), m500 = list(), m700 = list())

for(i in 1:r2){ # Cria um sequencia de inteiros ao longo do vetor r2...
  # gera 400000 amostras aleatorias da amostra "am" (tamanhos = 100, 300, 500, 7000)
  # com reposição e adiciona os resultados as diferentes listas conforme o tamanho m...
  list.boot2$m100[[i]] <- am[sample(x=1:length(am), size = 100, replace = TRUE)]
  list.boot2$m300[[i]] <- am[sample(x=1:length(am), size = 300, replace = TRUE)]
  list.boot2$m500[[i]] <- am[sample(x=1:length(am), size = 500, replace = TRUE)]
  list.boot2$m700[[i]] <- am[sample(x=1:length(am), size = 700, replace = TRUE)]
}

#------------------------------------------------------------------------------------------
# Cria uma função p/ calcular média de cada bootstrap, em função de cada tamanho "m", e adiciona em "resultados2"...

funcao.boot2 <- function(x, funcao){     # função que recebe um objeto e uma função específica...
resultado2 <- vector('list', length(x))  # cria um objeto com 4 listas...
names(resultado2) <- names(x)

for(i in seq_along(x)){                  # cria um vetor de inteiros para acompanhar as 4 listas dentro do objeto...
  for(j in seq_along(x[[i]])){           # cria um vetor de inteiros para acompanhar cada amostra bootstrap dentro de cada tamanho "m"...
    # aplica uma função específica a cada amostra bootstrap de tamano "m" e armazena separadamente no vetor "resultados2"...
    resultado2[[i]][[j]] <- funcao(x[[i]][[j]])
  }
}
return(resultado2)                 
}

# calcula a média p/ cada amostra bootstrap para cada tamanho "m"
mean.boot2<-funcao.boot2(list.boot2, mean)

#-----------------------------------------------------------------------------------------------------------
# 4. Faça um histograma das r estimativas, calcule a média e compare com a média verdadeira
# (para cada valor de m).
#-----------------------------------------------------------------------------------------------------------
layout(matrix(c(1, 2,
                3, 4),
              nr=2, byrow=T))
layout.show(n=4)

# Histograma para amostras bootstrap de tamanho igual a 100....
h.m100 <- hist(mean.boot2$m100, main = "Histrograma da distribuição bootstrap \n (Size = 100)",
             xlab = "Estimativas da média",
             ylab = "Frequência",
             col ="whitesmoke",
             ylim=c(0,16000))

abline(v = mean(mean.boot2$m100), col = "red",lwd = 2, lty=1) # Adiciona a média da dist. bootstrap...
abline(v = mean(pop), col = "blue",lwd = 2, lty=1) # Adiciona média da população...

legend(93,16000,legend="Média (bootstrap)",col="red",lwd=2,lty=1,bty="n")
legend(93,15000,legend="Média (População)",col="blue",lwd=2,lty=1,bty="n")

#-----------------------------------------------------------------------------------------------------------
# Histograma para amostras bootstrap de tamanho igual a 300....
h.m300 <- hist(mean.boot2$m300, main = "Histrograma da distribuição bootstrap \n (Size = 300)",
               xlab = "Estimativas da média",
               ylab = "Frequência",
               col ="whitesmoke",
               ylim=c(0,25000))

abline(v = mean(mean.boot2$m300), col = "red",lwd = 2, lty=1) # Adiciona a média da dist. bootstrap...
abline(v = mean(pop), col = "blue",lwd = 2, lty=1) # Adiciona média da população...

legend(95,25000,legend="Média (bootstrap)",col="red",lwd=2,lty=1,bty="n")
legend(95,23500,legend="Média (População)",col="blue",lwd=2,lty=1,bty="n")

#-----------------------------------------------------------------------------------------------------------
# Histograma para amostras bootstrap de tamanho igual a 500....
h.m500 <- hist(mean.boot2$m500, main = "Histrograma da distribuição bootstrap \n (Size = 500)",
               xlab = "Estimativas da média",
               ylab = "Frequência",
               col ="whitesmoke") # Cria um histograma das médias estimadas na distribuição bootstrap...
abline(v = mean(mean.boot2$m500), col = "red",lwd = 2, lty=1) # Adiciona a média da dist. bootstrap...
abline(v = mean(pop), col = "blue",lwd = 2, lty=1) # Adiciona média da população...

legend(96.3,30000,legend="Média (bootstrap)",col="red",lwd=2,lty=1,bty="n")
legend(96.3,28000,legend="Média (População)",col="blue",lwd=2,lty=1,bty="n")

#-----------------------------------------------------------------------------------------------------------
# Histograma para amostras bootstrap de tamanho igual a 700....
h.m700 <- hist(mean.boot2$m700, main = "Histrograma da distribuição bootstrap \n (Size = 700)",
               xlab = "Estimativas da média",
               ylab = "Frequência",
               col ="whitesmoke") # Cria um histograma das médias estimadas na distribuição bootstrap...
abline(v = mean(mean.boot2$m700), col = "red",lwd = 2, lty=1) # Adiciona a média da dist. bootstrap...
abline(v = mean(pop), col = "blue",lwd = 2, lty=1) # Adiciona média da população...

legend(96.5,15000,legend="Média (bootstrap)",col="red",lwd=2,lty=1,bty="n")
legend(96.5,14000,legend="Média (População)",col="blue",lwd=2,lty=1,bty="n")

#-----------------------------------------------------------------------------------------------------------
#END.......
#-----------------------------------------------------------------------------------------------------------
