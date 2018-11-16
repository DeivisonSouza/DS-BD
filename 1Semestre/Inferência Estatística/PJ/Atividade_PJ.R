#########################################################################################################################################
###################################################ESTIMATIVA DE MÁXIMA VEROSSIMILHANÇA##################################################
######################################################ALUNO: DEIVISON VENICIO SOUZA######################################################
#########################################################################################################################################

##########################################################
### DESCRIÇÃO DA ATIVIDADE
##########################################################

# Atividade: Obter gráficos das funções L(), l(), LR() e D() e estimativas (analíticas qdo possível e tb numéricas em todos os casos) 
# para os casos a seguir:

# Procire, em cada distribuição, comparar a informação entre os respectivos conjuntos de dados

# Dados de N(theta, 5)
# 20, 28, 39
# < 22, [25-32], > 38
# > 22, [25-32], < 38

# Dados de P(theta)
# 5, 3, 0, 1, 6, 2, 3, 4, 5, 3
# 5, <3, 0, 1, >4, [1-3], 3, 4, >3, [1-6]

#-------------------------------------------------------------------------------------------------------------------------------------
# A funçao de verossimilhanca expressa as informações que existem nos dados. O máximo da função é o melhor valor (que tem mais haver
# com os dados..). Suponha que temos dados de uma distribuição normal (theta, variância). A verossimilhança é o produtório da densidade
# de todos os pontos (isso é calculado pela dnorm). No entanto, é mais vantajoso calcular a log-verossimilhanca, pois trabalha com 
# somatório.

# Para obter ajudas sobre diversas distribuições no R
help(Distributions)

##########################################################
### DISTRIBUIÇÃO NORMAL
##########################################################
dnorm(x, mean = 0, sd = 1, log = FALSE)
pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
rnorm(n, mean = 0, sd = 1)

#------------------------------------------------------------------------------------------------------------------------------------
## A função dnorm

# dnorm = A função 'dnorm' retorna o valor da função de densidade de probabilidade (PDF) para a distribuição normal. A função 'dnorm' 
# leva três argumentos principais (x = ?, mean = 0, sd = 1, log = FALSE). O default da função é mean = 0 e sd = 1 (valores da distribuição 
# normal padrão).

# dnorm(x, mean = 0, sd = 1, log = FALSE)

# Por exemplo, dnorm(1) retorna 0.2419707. Analogamente, dnorm pode receber um vetor de parâmetros, assim dnorm(seq(-1, 1, by=0.1)) 
# gera todos valores da função densidade de probabilidade para os valores entre -1 e 1 (um vetor de 21 valores).

# O histograma também pode ser facilmente obtido usando-se dnorm:

(x <- seq(-1, 1, by=0.1))
(y <- dnorm(x))
plot(x, y, type="l")

#------------------------------------------------------------------------------------------------------------------------------------
## A função pnorm

# pnorm = A 'pnorm' calcula a função de probabilidade acumulada da distribuição normal (CDF). Por exemplo, se desejo encontrar 
# P(x < 2) para distribuição normal padrão (média = 0 e sd = 1), posso fazer pnorm (2), que é 0,977.

# pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
?pnorm
pnorm(2)

(x <- seq(-1, 1, by=0.1))
(y <- pnorm(x))
plot(x, y, type="l")

#------------------------------------------------------------------------------------------------------------------------------------
## A função qnorm

# qnorm = A função 'qnorm' é a inversa de 'pnorm'. Assim, q vem de quartil. Então, para gerar o primeiro quartil de uma normal de 
# média 5 e desvio-padrão 3, basta fazer qnorm(0.25, mean = 5, sd = 3) (o valor é 2.976531). 

# qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
qnorm(0.25, mean = 5, sd = 3)

#------------------------------------------------------------------------------------------------------------------------------------
## A função rnorm

# rnorm = A função 'rnorm' gera números aleatórios de variáveis com distribuição normal. Em que 'n' é o tamanho da amostra 
# (x será um vetor caso n > 1). Ainda, mean e sd são parâmetros (opcionais). Se mean ou sd forem omitidos, serão usados os valores
# da distribuição normal padrão (mean = 0 e sd = 1). Por exemplo:

# gera um vetor com 10 valores independentes e identicamente distribuídos, com média zero e desvio-padrão 1 (normal padrão).
x <- rnorm(10, mean = 0, sd = 1)
hist(x)

x <- rnorm(1000, mean = 0, sd = 1)
hist(x)
#-------------------------------------------------------------------------------------------------------------------------------------

##########################################################
### DISTRIBUIÇÃO POISSON
##########################################################
# Densidade, função de distribuição, função quantílica e geração aleatória para a distribuição de Poisson com o parâmetro lambda.
dpois(x, lambda, log = FALSE)
ppois(q, lambda, lower.tail = TRUE, log.p = FALSE)
qpois(p, lambda, lower.tail = TRUE, log.p = FALSE)
rpois(n, lambda)

#------------------------------------------------------------------------------------------------------------------------------------
## A função 'dpois'


#------------------------------------------------------------------------------------------------------------------------------------
## A função 'ppois'

#------------------------------------------------------------------------------------------------------------------------------------
## A função 'qpois'

#------------------------------------------------------------------------------------------------------------------------------------
## A função 'rpois'


################################################################
### RESOLUÇÃO DA ATIVIDADE - VEROSSIMILHANÇA DISTRIBUIÇÃO NORMAL
################################################################

#L(θ; y) = É a função de verossimilhança. Onde theta pode ser um escalar ou vetor de parâmetros que deseja-se encontrar, e 
# y = um conjunto de valores de uma variável aleatória.

# Atividade: Obter gráficos das funções L(), l(), LR() e D() e estimativas (analíticas qdo possível e tb numéricas em todos os casos) 
# para os casos a seguir:

# Procure, em cada distribuição, comparar a informação entre os respectivos conjuntos de dados.

# a) Dado os conjuntos de dados abaixo com distribuicção normal e variância conhecida igual a 5 N(theta, 5) a estimativa por máxima 
# verossimilhança
# 20, 28, 39
# < 22, [25-32], > 38
# > 22, [25-32], < 38

# Qual a contribuição de cada dado para a verossimilhança? A soma da contribuição de cada dado é o valor máximo da verossimilhança!

#-----------------------------------------------------------------------------------------------------------------------------------
## Solução Analítica
#-----------------------------------------------------------------------------------------------------------------------------------
## a) 20, 28, 39
(x1 <- c(20, 28, 39))                             # Dado um conjunto de dados, com distribuicão normal e variância conhecida = 5

mean(x1)                                          # média do conjunto de dados...  

x<-c(12, 15, 9, 10, 17, 12, 11, 18, 15, 13) 
x
mean(x)
sd(x)
var(x)
sx2 <- sum(x^2)
sx <- sum(x)

(mu.vals <- seq(11, 15, length.out = 100))
length(mu.vals)
?seq
(lmu <- -5 * log(8 * pi) - (sx2 - 2 * mu.vals * sx + 10 * (mu.vals^2))/8)

plot(mu.vals, lmu, type = "l", xlab = expression(mu), ylab = expression(l(mu)))
#Função de verossimilhança para o parâmetro μ da distribuição normal com variância σ2 = 4 com os dados do Exemplo 1.

logvero <- function(mu, dados){ 
       sum(dnorm(dados, mean = mu, sd = 2, log = TRUE))}

mu.vals <- seq(11, 15.5, l = 100) 
mu.vals[1:10]

lmu <- sapply(mu.vals, logvero, dados = x) 
lmu[1:10]
plot(mu.vals, lmu, type = "l", xlab = expression(mu), ylab = expression(l(mu)))

