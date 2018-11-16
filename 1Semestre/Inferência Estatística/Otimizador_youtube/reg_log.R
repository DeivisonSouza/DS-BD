#Aplicação: Classificador
#Suponha que temos um conjunto de 200 usuários com as informações.
#Salario em reais.
#Anos de experiencia.
#Paga conta premium(SIM ou NÃO).

#Construa um classificar para avaliar se um usuario novo será ou não assinante de uma 
#conta premium.



dados <- read.table("reg_log.txt", header = T, sep="")
names(dados) <- c("Premium", "Renda", "Anos")
names(dados)

head(dados)

#Podemos visualizar os dados plotando as variáveis explanatórias e marcar com diferentes 
#símbolos usuários e não.

attach(dados)
plot(Anos,Renda/1000)
detach(dados)

#Função perda quadrática para variáveis binárias
#A ideia de construir um classificador é similar ao modelo de regressão linear múltipla, 
#ou seja, queremos minizar uma certa distância entre os dados yi e nosso modelo linear 
#β0+β1rendai+β2anosi. Sendo assim, uma primeira tentativa óbvia é minimizar

#Isso pode ser feito facilmente em R uma vez que temos vários otimizadores numéricos 
#disponíveis. O primeiro passo é escrever a função objetivo:

f_ols <- function(par, y, renda, anos) {
  mu <- par[1] + par[2]*renda + par[3]*anos
  SQ <- sum( (y - mu)^2 )
  return(SQ)
}

# Onde: 
# mu: é o valor da previsão;
# par: são os parâmetros da função;
# SQ: Perda quadrática;

#O segundo passo consiste em otimizar a função objetivo. Por simplicidade vamos usar um 
#algoritmo numérico, porém lembre-se que neste caso temos expressões fechadas disponíveis.

fit_ols <- optim(par = c(0,0,0), fn = f_ols, y = dados$Premium, 
                 renda = dados$Renda, anos = dados$Anos)
fit_ols

#O algoritmo convergiu e forneceu o vetor de β’s que minimiza a soma de quadrados. 
#Podemos agora usar estes valores para predizer se um usuário é ou não dado sua renda e 
#anos de experiência. Neste caso, apenas como exemplo vamos predizer os mesmos usuários 
#que foram usados para ajustar/treinar o modelo. Note que isso, também fornece uma indicação 
#se o modelo tem um bom poder de predição. Entretando, na prática é mais usual separar a base 
#em treino e teste e predizer os usuários na base teste. A predição é bastante simples 
#entramos com a renda e anos de estudo e saímos com uma predição para yi.

preditos <- fit_ols$par[1] + fit_ols$par[2]*dados$Renda + fit_ols$par[3]*dados$Anos

# Vamos plotar os preditos contra os observados para verificar a performance do nosso modelo.

plot(preditos, dados$Premium, xlab="Preditos", ylab = "Observado")

#Apesar de ser possível essa abordagem leva a algumas incoveniências. Por exemplo, o ideal 
#seria que a previsão fossem 0 ou 1 refletindo se o novo usuário será ou não um assinante. 
#No entanto, vimos que o modelo fornece preditos maiores que 1 e pode potencialmente fornecer
# preditos negativos para usuários com pouca experiência e renda menor. Neste sentido, 
# a interpretação do modelo não é clara e não reflete de forma fidedigna a realidade.

#1.3 Melhorando o modelo
#Uma vez que nosso modelo não parece uma aproximação razoável para a realidade podemos 
#tentar melhorá-lo fazendo com que aspectos práticos sejam levados em consideração. 
#Note que se a previsão fosse restrita ao intervalo unitário poderíamos interpretar a 
#previsão como uma medida de pertinência aos grupos, o que seria mais intuitivo. O que 
#gostaríamos é que grandes valores de $ _0 + _1 renda_i + _2 anos_i$ resultassem em valores 
#próximos a 1. Da mesma forma, pequenos valores de $ _0 + _1 renda_i + _2 anos_i$ devem 
#resultar em valores próximos a 0. Para incluir em nosso modelo essa intuição podemos usar 
#uma função matemática que tenha essa característica. O que precisamos é uma função que 
#receba um número real qualquer e resulte em um número no intervalo unitário. Em outros 
#termos, precisamos de uma função que tenha domínio sendo os números reais e como imagem o 
#intervalo unitário, ou seja, f(⋅):R→(0,1).

#Existem na literatura diversas funções que tem tal propriedade. Uma escolha popular é a 
#função logística dada por:

#Com a função logística em mãos podemos rescrever nossa função objetivo como:

#Novamente, podemos facilmente otimizar essa função objetivo, ou seja, encontrar β0, β1 e β2 
#que tornam SQlogit o menor possível. Novamente escrevemos a função objetivo:

f_logit <- function(par, y, renda, anos) {
  mu <- 1/(1+ exp(- (par[1] + par[2]*renda + par[3]*anos)))
  SQ_logit <- sum( (y - mu)^2 )
  return(SQ_logit)
}

#Otimizamos numéricamente:

fit_logit_ols <- optim(par = c(0,0,0), fn = f_logit, y = dados$Premium, 
                       renda = dados$Renda, anos = dados$Anos)
fit_logit_ols

#Fazendo a previsão:

preditos2 <- fit_logit_ols$par[1] + fit_logit_ols$par[2]*dados$Renda + fit_logit_ols$par[3]*dados$Anos

#Plotando os preditos contra os observados.
#Neste caso fica claro que nosso modelo só prediz valores no intervalo unitário como 
#gostaríamos. Claramente quanto maior é o preditor maior é a chance dos observados 
#serem 1 como queríamos.

plot(preditos2, dados$Premium, xlab="Preditos", ylab = "Observado")



