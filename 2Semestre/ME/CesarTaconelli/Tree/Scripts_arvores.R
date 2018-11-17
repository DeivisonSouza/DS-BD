########################################################################
########################################################################
########################################################################
### Aplicação 1 - Árvores de classificação.

options(device = 'x11')

### Carregando as bibliotecas necessárias.
require(gamlss) 
require(rpart)
require(rpart.plot)
require(AppliedPredictiveModeling)
require(kernlab)
require(pROC)

help(iris)
head(iris)
summary(iris)
by(iris,iris$Species,summary)

### Ajustando uma árvore para a espécie em função das dimensões da planta.
set.seed(97)
arvore1 <- rpart(Species ~ ., data = iris)
arvore1
summary(arvore1)
rpart.plot(arvore1)

### Vamos ilustrar a partição fornecida pela árvore.

par(cex=1.5,las=1)
with(iris, plot(Petal.Length, Petal.Width, type='n', 
                xlab = 'Comprimento da pétala',ylab = 'Largura da pétala'))
with(iris[which(iris$Species=='setosa'),], 
     points(jitter(Petal.Length), jitter(Petal.Width),pch=20,col='red'))
with(iris[which(iris$Species=='versicolor'),],
     points(jitter(Petal.Length), jitter(Petal.Width),pch=20,col='blue'))
with(iris[which(iris$Species=='virginica'),],
     points(jitter(Petal.Length), jitter(Petal.Width),pch=20,col='green'))
lines(c(2.45,2.45), c(0,3), col='red', lty=2)
lines(c(0,8), c(1.8,1.8), col='red', lty=2)

### Tabela de confusão.
preditos <- predict(arvore1, newdata = iris[,1:4], type='class')
table(preditos, iris$Species) 

########################################################################
########################################################################
########################################################################
### Árvores de classificação e regressão - aplicação 2.
### Vamos retomar os dados sobre aluguéis de imóveis em Munique, 
### analisados anteriormente usando GAMLSS.
??rent
### Parte 1 - Preparação dos dados.
require(gamlss.data)
data(rent)
help(rent)

### Para facilitar a análise e visualização dos resultados, vamos filtrar
### apenas as variáveis que serão usadas. Além disso, vamos renomeá-las
### e atribuir labels adequados às variáveis categóricas.
rent2 <- rent[,c(1,2,3,6,7,8,9)]
names(rent2) <- c('Aluguel', 'Area', 'Ano', 'Banheiro', 'Aquecimento', 
                  'Cozinha', 'Localizacao')

rent2$Banheiro <- factor(rent2$Banheiro, labels = c('Sim', 'Nao'))
rent2$Aquecimento <- factor(rent2$Aquecimento, labels = c('Sim', 'Nao'))
rent2$Cozinha <- factor(rent2$Cozinha, 
                        labels = c('Médio padrao', 'Alto padrao'))
rent2$Localizacao <- factor(rent2$Localizacao, 
                            labels = c('Inferior', 'Media', 'Superior'))

########################################################################
### Parte 2 - Análise descritiva/exploratória.

### Agora, vamos produzir algumas descritivas. Inicialmente, vamos analisar
### a distribuição marginal dos preços de aluguel.

### Alguma análise bivariada.
x11(height = 15, width = 12)
par(mfrow = c(3,2), mar = c(4,4,2,2), las = 1, cex = 1)
plot(Aluguel ~ Area, data = rent2, col = 'blue', cex = 0.8)
plot(Aluguel ~ Ano, data = rent2, col = 'blue', cex = 0.8)
plot(Aluguel ~ Banheiro, data = rent2, col = 'lightblue', cex = 0.8)
plot(Aluguel ~ Aquecimento, data = rent2, col = 'lightblue', cex = 0.8)
plot(Aluguel ~ Cozinha, data = rent2, col = 'lightblue', cex = 0.8)
plot(Aluguel ~ Localizacao, data = rent2, col = 'lightblue', cex = 0.8)

hist(rent2$Aluguel, breaks = 15, main = '', col = 'lightblue')
### A distribuição (marginal) dos preços de aluguel é assimétrica à direita.
### Aplicar uma transformação logarítmica permitiria simetrizar um pouco
### mais a distribuição. No entanto, como os resultados obtidos com e sem
### transformação foram similares, procedemos com os dados na escala original.

########################################################################
### Parte 3 - Análise usando árvores de regressão.

help("rpart") 
help("rpart.plot") 
help("rpart.control") 

### Vamos iniciar fixando a semente, uma vez que a seleção do modelo baseia-se
### em validação cruzada. Desta forma, os resultados são reprodutíveis.

set.seed(2018)
arvore1 <- rpart(Aluguel ~ ., data = rent2)
summary(arvore1) ### Resumo do modelo ajustado.

### Agora, vamos plotar o modelo ajustado. Para isso, usamos a função
### rpart.plot, do pacote de mesmo nome.
x11(width = 15, height = 12)
rpart.plot(arvore1, type = 4, extra = 1, cex = 1.2)

### O modelo resultante tem onze nós finais. Para encontrarmos o número
### "ótimo" de nós, vamos usar validação cruzada.
plotcp(arvore1)
printcp(arvore1)

### A árvore com seis nós finais é a menor cujo erro, avaliado por validação
### cruzada, está abaixo de um erro padrão da árvore com menor erro (11 nós).
### Assim, a árvore com seis nós finais foi selecionada para a sequência da
### análise.

barplot(arvore1$variable.importance, col = 'red', las = 1, 
        xlab = 'Covariável', ylab = 'Importância')
### A importância das covariáveis, em árvores de regressão, é definida pela
### contribuição de cada uma delas na redução da soma de quadrados dos
### resíduos, no conjunto de partições que compõem a árvore. Além das 
### variáveis responsáveis pelas partições, aquelas que fornecem as partições
### substitutas (surrogate splits) também são consideradas. Nesta aplicação, 
### podemos ver que a área do imóvel é, destacadamente, a variável mais
### importante para explicar o valor dos aluguéis, seguida por aquecimento
### e ano de construção.

arvore2 <- prune(arvore1, cp = 0.013)
### Fazendo a poda da árvore, para valores de cp entre 0.012 e 0.015, a
### árvore ótima é a de seis nós.

x11(width = 20, height = 15)
rpart.plot(arvore2, type = 3, extra = 1, cex = 1.2, uniform = FALSE)
### Cada ramo da árvore é definido por um conjunto de regras baseadas nos
### valores das covariáveis. No interior de cada nó temos o preço médio de
### aluguel (acima) e o número de imóveis que compõem o nó (abaixo).

arvore2$where
### Label do nó final ao qual cada imóvel foi alocado. Para avaliar melhor
### o modelo resultante, vamos analisar a distribuição dos preços de aluguel
### dos imóveis em cada nó.

boxplot(rent2$Aluguel ~ arvore2$where, ylab = 'Valor de Aluguel', 
        xlab = 'Nó', col = 'lightblue', cex = 1.2, las = 1)

### A predição para novos imóveis é feita simplesmente verificando o nó final
### ao qual são alocados, conforme os valores de suas covariáveis. A média
### dos valores de aluguel dos imóveis que compõem o nó é usado como valor
### predito.
novos_imoveis <- data.frame(Area = c(50,80), Ano = c(1950, 1975), 
                            Aquecimento = c('Nao', 'Sim'),
                            Banheiro = c('Sim', 'Sim'), 
                            Cozinha = c('Médio padrao', 'Médio padrao'),
                            Localizacao = c('Media', 'Superior'))
novos_imoveis
predict(arvore2, newdata = novos_imoveis)

### Podemos extrair resíduos e valores ajustados para uma checagem adicional 
### do ajuste.
predict(arvore2) ### Valores ajustados.
residuals(arvore2) ### Resíduos (ordinários)
plot(residuals(arvore2) ~ predict(arvore2), col = 'blue', xlab = 'Valores ajustados',
     ylab = 'Resíduos')

########################################################################
########################################################################
########################################################################
### Aplicação 3 - Árvores de classificação. Vamos usar árvores de classificação
### para classificação de e-mails em spam ou não spam.
data(spam)
help(spam)
head(spam)
dim(spam)
summary(spam)

### Alguns gráficos
with(spam, boxplot(log(charExclamation + 0.1) ~ type, ylab = 'log(frequência)',
                   col = 'lightblue', cex = 1.45, las = 1))
with(spam, boxplot(log(capitalTotal + 0.1) ~ type, ylab = 'log(frequência)',
                   col = 'lightblue', cex = 1.45, las = 1))
### Aparentemente, as frequências de pontos de exclamação ou letras maiúsculas
### são importantes para discriminar spams e não spams (spams têm maior
### frequência de ambos).


########################################################################
### Parte 1 - Preparação dos dados.

### Vamos dividir a base em duas: uma para ajuste do modelo e outra para
### validação. A base de validação com 1000 observações e a de ajuste com
### as demais.

set.seed(88) ### Fixando a semente para sorteio das observações.
select <- sample(1:nrow(spam), 1000)
### Linhas da base que serão usadas para validação.

spam_ajuste <- spam[-select,] ### Base de ajuste.
spam_predict <- spam[select,] ### Base de validação.

########################################################################
### Parte 2 - Ajuste do modelo.

arvore1 <- rpart(type ~ ., data = spam_ajuste)
printcp(arvore1) ### Tabela de custo-complexidade.
plotcp(arvore1) ### Curva de custo-complexidade.
### Pela análise da curva de custo-complexidade, a árvore com nove nós
### finais é selecionada.

### Fazendo a poda para obtenção da árvore com nove nós finais.
arvore2 <- prune.rpart(arvore1, cp = 0.011)
x11(width = 25, height = 15)
rpart.plot(arvore2, type = 3, extra = 1, cex = 1.2, uniform = FALSE)

### Agora, vamos dar sequência com a predição dos dados na amostra de validação.
predmat <- predict(arvore2, newdata = spam_predict)
predmat
### As predições correspondem às proporções de spam e não spam nos nós.

pred <- predict(arvore2, newdata = spam_predict, type = 'class')
pred
### Cada e-mail da base de predição é classificado como spam, ou não spam
### com base na maioria de e-mails no correspondente nó.

### Tabela de confusão:
tabcruz <- table(pred, spam_predict$type)
tabcruz

### Sensibilidade:
341/(341+49)

### Especificidade:
561/(561+49)

### Acurácia:
(561+341)/1000

### Vamos fazer a análise da curva ROC para avaliar a performance preditiva
### do modelo e pontos de corte alternativos a p=0.5 para a regra de classificação.

pred2 <- predmat[,2] 
### Probabilidades estimadas de spam.

### Usando os recursos do pacote pRoc.
predroc <- roc(spam_predict$type, pred2, percent = TRUE)
plot(predroc, print.thres = c(0.1, 0.5, 0.8, 0.9), print.thres.pattern.cex = 0.8)
coords(predroc, 'best') 
### A regra de classificação que melhor conjuga sensibilidade e especificidade 
### baseia-se em p=0.435. No entanto, a regra baseada em p = 0.5 é rigorosamente
### a mesma (por que?).

### Conforme visto na aula de regressão logística, custos de má-classificação
### podem ser incorporados na seleção da melhor regra de classificação.
### A função coords, por default, identifica o ponto de corte tal que a soma
### sensibilidade + especificidade seja máxima. Podemos modificar este critério
### através da função best werights, em que declaramos:

### 1 - O custo relativo de um falso negativo quando comparado a um falso
### positivo;

### 2 - A prevalência ou proporção de casos (spams) na população.

### Vamos considerar que a proporção de spams seja 1/3 e obter a regra de
### classificação ótima para diferentes custos.

coords(predroc, 'best', best.weights = c(0.5, 1/3)) 
coords(predroc, 'best', best.weights = c(1, 1/3)) 
coords(predroc, 'best', best.weights = c(2, 1/3)) 
coords(predroc, 'best', best.weights = c(10, 1/3)) 