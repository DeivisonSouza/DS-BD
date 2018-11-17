###################################################################################################################################
# Discente: Deivison Venicio Souza
###################################################################################################################################

# Universidade Federal do Paraná - Departamento de Estatística
# Especialização em Data Sciece e Big Data
# Prof. Cesar Augusto Taconeli
# Avaliação - 19/10/2018

# Vamos considerar a aplicação de uma árvore de regressão à base de dados abalone, do pacote AppliedPredictiveModeling. Os dados 
# referem-se a 4177 espécimes de abalone, tipo de molusco encontrado ao longo das águas costeiras de todos os continentes. A variável
# resposta é a idade do molusco, aferida pelo número de anéis internos, que é um procedimento demorado e pouco adequado. O objetivo é 
# ajustar um modelo que permita estimar a idade a partir de outras medidas, que são obtidas com maior facilidade. Para maiores detalhes
# a respeito da base, consultar a documentação e o link fornecido. Para a análise, as primeiras 3000 linhas deverão ser usadas para 
# ajuste, e as demais para validação.

###################################################################################################################################
# Carregando pacote
###################################################################################################################################
library(AppliedPredictiveModeling)          # contém o conjunto de dados "abalone"
library(PerformanceAnalytics)               # Gráfico da matriz de correlações
library(rpart)                              # Ajustar um modelo de árvore de regressão (CART)
library(rpart.plot)                         # Plotar uma árvore de regressão
library(xlsx)                               # Salvar para excel
library(ggplot2)                            # Vizualização gráfica
library(data.table)                         # manipulação de dados
library(gridExtra)                          # tabela em ggplot2
library(ggthemes)

###################################################################################################################################
# Carregando dados e fazendo manipulações
###################################################################################################################################
?abalone
data(abalone)                                     # carrega conjunto de dados
str(abalone)                                      # estrutura do data frame
head(abalone, 10)                                 # ler as primeiras 10 linhas

###################################################################################################################################
# Dividindo conjunto de dados: Treino e Teste
###################################################################################################################################
train <- abalone[1:3000,]
test <- abalone[(nrow(train)+1):nrow(abalone),]

with(train,table(Rings))    # tabela freq. p/ variável Rings (número de anéis)
with(test,table(Rings))     # tabela freq. p/ variável Rings (número de anéis)

###################################################################################################################################
# Ajuste do modelo - Árvore de Regressão (Package Rpart) - Implementa o CART
###################################################################################################################################

#----------------------------------------------------------------------------------------------------------------------------------
## AJUSTE 1: Usando cp = 1 para ajustar a árvore (sempre resultará em uma árvore sem divisões)
#----------------------------------------------------------------------------------------------------------------------------------
set.seed(1)
(tree.cp1 <- rpart(Rings ~ ., data = train, control = rpart.control(cp = 1)))
print(tree.cp1)
summary(tree.cp1, cp = 0.02) 
rpart.plot(tree.cp1)

#----------------------------------------------------------------------------------------------------------------------------------
## AJUSTE 2: Usando cp = 0.01 (parâmetro default) para ajustar a árvore
#----------------------------------------------------------------------------------------------------------------------------------
set.seed(1)
(tree.cp2 <- rpart(Rings ~ ., data = train, method="anova", control = rpart.control(cp = 0.01)))
print(tree.cp2)
summary(tree.cp2, cp = 0.02)      # um resumo da árvore (imprimindo divisões com cp superiores à 0.02)
rpart.plot(tree.cp2)
cp <- printcp(tree.cp2)           # Tabela de custo-complexidade.

barplot(tree.cp2$variable.importance, col = 'red', las = 1, 
        xlab = 'Covariável', ylab = 'Importância')

#-------------------------------------------------------------------------
# Fazendo os plots a árvore original teria 11 nós terminais e 10 divisões.
plotcp(tree.cp2, upper = "splits",  minline = TRUE)     # Usando "splits" fica mais fácil ver onde cortar a árvore...          
abline(v = 7, lty = "dashed", col = "red")              # visulamente, o ponto de corte seria em 7 divisões (cp = 0.013786)

plotcp(tree.cp2, upper = "size",  minline = TRUE)       # o que correposnde também a 8 nós terminais
abline(v = 7, lty = "dashed", col = "red")

cp <- printcp(tree.cp2)
#------------------------------------------------------------------------
# Agora, fazendo a poda usando a regra de Breiman

# Para essa configuração uma árvore com 7 divisões estaria dentro da regra de Breiman? Ou seja, eu deveria usar cp = 0.013786 para podar
# a arvore (teria uma árvore com 7 divisões e 8 nós terminais)

(prune.cp2 = prune(tree.cp2, cp = 0.013786))     
plotcp(prune.cp2, upper = "splits",  minline = TRUE)
plot(prune.cp2, branch = 0.3, compress = TRUE)
text(prune.cp2)
rpart.plot(prune.cp2, type = 3, extra = 1, cex = 1.2, uniform = FALSE)
#------------------------------------------------------------------------
# Mas, se usar o valor de cp = 0.001 para podar conforme solicitado no trabalho nada mudaria em relação a árvore original!

(prune.cp2 = prune(tree.cp2, cp = 0.001))    # 10 divisões e 11 nós finais
plotcp(prune.cp2, upper = "splits",  minline = TRUE)
plot(prune.cp2, branch = 0.3, compress = TRUE)
text(prune.cp2)
rpart.plot(prune.cp2, type = 3, extra = 1, cex = 1.2, uniform = FALSE)
tree.cp2$where

#------------------------------------------------------------------------
# Residuos para o modelo ajustado
plot(residuals(prune.cp2) ~ predict(prune.cp2), col = 'blue', xlab = 'Valores ajustados', ylab = 'Resíduos')
abline(h=0, lty = "dashed", col = "red")

#------------------------------------------------------------------------
# Predição usando a árvore final podada (cp = 0.001)

#(new_data <- data.frame(ShellWeight = c(0.18, 0.31), 
#                        ShuckedWeight = c(0.25, 0.45)))


(new_data <- data.frame(Type = as.factor(c(NA,NA)),
                        LongestShell = as.numeric(c(NA,NA)),
                        Diameter = as.numeric(c(NA,NA)),
                        Height = as.numeric(c(NA,NA)),
                        WholeWeight = as.numeric(c(NA,NA)),
                        ShuckedWeight = c(0.25, 0.45),
                        VisceraWeight = as.numeric(c(NA,NA)),
                        ShellWeight = c(0.18, 0.31),
                        stringsAsFactors = TRUE))

(pred <- predict(prune.cp2, newdata = new_data))

#------------------------------------------------------------------------
# Resíduo para os novos dados, sabendo que os valores reais para Rings são: 
# Rings=8 e Rings=10, para o primeiro e segundo, respectivamente.

(df <- cbind(new_data[,c(6,8)], observado = c(8,9), predito = pred))

(residuo <- df$predito - df$observado)


#------------------------------------------------------------------------
# Fazendo as previsões para o conjunto de teste

(test$pred <- predict(prune.cp2, newdata = test))

(SSE <- sum((test$Rings - mean(test$Rings))^2))
(MSE <- sum((test$Rings - mean(test$Rings))^2)/length(test$Rings))
(RMSE <- sqrt(MSE))

plot(test$pred,test$Rings,xlab="Predito", ylab="Observado")
abline(a=0, b=1)


#----------------------------------------------------------------------------------------------------------------------------------
## AJUSTE 3: Usando cp = 0.001 para ajustar a árvore
#----------------------------------------------------------------------------------------------------------------------------------

set.seed(1)
(tree.cp3 <- rpart(Rings ~ ., data = train, method="anova", control = rpart.control(cp = 0.001)))

print(tree.cp3)
summary(tree.cp3, cp = 0.02)      # um resumo da árvore (imprimindo divisões com cp superiores à 0.02)
rpart.plot(tree.cp3)
cp <- printcp(tree.cp3)         # Tabela de custo-complexidade.

par(mfrow=c(2,1), mar=c(4,5,5,3)) # mar(bottom, left, top, right)


# Fazendo os plots a árvore original teria 69 nós terminais e 68 divisões (árvore super ajustada)

plotcp(tree.cp3, upper = "splits",  minline = TRUE)     # Usando "splits" fica mais fácil ver onde cortar a árvore...          
abline(v = 15, lty = "dashed", col = "red")

plotcp(tree.cp3, upper = "size",  minline = TRUE)
abline(v = 15, lty = "dashed", col = "red")


# Agora, fazeno a poda usando a regra de Breiman:
# Para essa configuração uma árvore com 16 divisões estaria dentro da regra de Breiman? Ou seja, eu deveria usar cp = 0.0056785
# para podar a arvore (teria uma árvore com 16 divisões e 17 nós terminais)
(prune.cp3 = prune(tree.cp3, cp = 0.0056785))     

# Mas, e usando o valor de cp = 0.001?

(prune.cp3 = prune(tree.cp3, cp = 0.001)) # a poderia seria realizada resultando em uma árvore mais complexa qdo comparada ao ponto
# de poda sugerido pela regra de Breiman.

#----------------------------------------------------------------------------------------------------------------------------------
# Usando a biblioteca "tree"
#----------------------------------------------------------------------------------------------------------------------------------
library(tree)
set.seed(1)
tree.model <- tree(Rings ~ ShellWeight + ShuckedWeight, data = train)
plot(tree.model)
text(tree.model, cex=.75)
plot(train$ShellWeight, train$ShuckedWeight, pch=20, xlab="ShellWeight",ylab="ShuckedWeight")
tree::partition.tree(tree.model, ordvars=c("ShellWeight","ShuckedWeight"), add=TRUE)



