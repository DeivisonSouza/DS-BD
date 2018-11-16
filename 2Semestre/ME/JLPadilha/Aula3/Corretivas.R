########################################################################
########################################################################
########################################################################
### Aula - Transformações, ponderação e regressão robusta.

########################################################################
### Exemplo 1 - Tranformação na resposta - o método de Box-Cox.

require(faraway)
require(car)
help(ozone)

### Vamos fazer uma análise de regressão linear considerando a concentração
### de ozônio como resposta e temperatura, unidade e ibh como preditoras.

ozone2 <- subset(ozone, select = c('O3', 'temp', 'humidity', 'ibh'))
pairs(ozone2, pch = 20, cex = 1.4)
### Podemos observar relações não lineares, evidente variância não constante,
### alguma evidência de assimetria. Claramente são apenas evidências baseadas
### em descritivas bivariadas.

ajuste <- lm(O3 ~ temp + humidity + ibh, data = ozone2)
par(mfrow = c(2,2))
plot(ajuste) ### Há fortes indícios de variância não constante para os erros,
### alguma evidência de não normalidade.
ncvTest(ajuste) ### A hipótese de variância constante é rejeitada.
shapiro.test(rstandard(ajuste)) 

### Vamos usar o método de Box-Cox para identificar uma transformação adequada.
boxCox(ajuste, lambda = seq(0,1,0.1))

### Uma transformação do tipo raiz cúbica é indicada.

ozone2$O3_trans <- ozone2$O3^(1/3) ### Variável transformada.
ajuste2 <- lm(O3_trans ~ temp + humidity + ibh, data = ozone2)
par(mfrow = c(2,2))
plot(ajuste2)
ncvTest(ajuste2)
shapiro.test(rstandard(ajuste2))
### Observe que não há mais evidências contrárias às hipóteses de variância
### constante e normalidade. Os gráficos de resíduos têm um padrão bem mais
### adequado, indicando um bom ajuste.

### Vamos fazer as predições e produzir um gráfico de efeitos. Vamos fixar
### temperatura e ibh na média, e predizer a concentração de ozônio para 
### uma sequência de valores para umidade.

novo_humidity <- seq(min(ozone2$humidity), max(ozone2$humidity), length.out = 100)
### Grid de valores para umidade.

novos_dados <- expand.grid(temp = mean(ozone2$temp), ibh = mean(ozone2$ibh), 
                           humidity = novo_humidity)
### Criando a base para predição.

predic <- predict(ajuste2, newdata = novos_dados, interval = "prediction")
predic_orig <- predic^3 ### Predições na escala original.

plot(novo_humidity, predic_orig[,1], type = 'l', ylim = c(0,30), col = 'red')
lines(novo_humidity, predic_orig[,2], col = 'red', lty = 2)
lines(novo_humidity, predic_orig[,3], col = 'red', lty = 2)
### Gráfico de efeitos.

########################################################################
### Exemplo 2 - Mínimos quadrados ponderados. Vamos utilizar a base de 
### dados cars, disponível na base do R.

require(car) 
require(nlme) 

help(cars) 
head(cars,10) 
summary(cars) 

par(cex = 1.4, las = 1)
plot(cars,pch=20,xlab='Velocidade (mph)',ylab='Distância de frenagem (m)') 
with(cars,lines(lowess(dist~speed),col='red',lwd=2)) 

### A dispersão de y parece aumentar conforme x.


### Ajuste 1: regressão linear via mínimos quadrados ordinários.

ajuste <- lm(dist~speed,data = cars) 
summary(ajuste) 

par(mfrow = c(2,2))
plot(ajuste, pch = 20, cex = 1.4)
### Os resíduos reforçam a evidência de variância não constante.

ncvTest(ajuste) 
### Como a hipótese nula é a de variância constante para os erros, temos evidência
### significativa de variância não constante ao nível de 5%.


### Ajuste 2: regressão linear via mínimos quadrados ponderados.
### vamos assumir que a variância aumenta linearmente conforme a velocidade (x).
### Assim, os pesos vão ser definidos por 1/x.

ajuste2 <- lm(dist ~ speed, weights = 1/speed, data = cars) 
summary(ajuste) 
compareCoefs(ajuste, ajuste2) ### Comparação das estimativas e erros 
### fornecidas pelos dois modelos.

plot(ajuste2, pch = 20, cex = 1.4)
### Os resíduos reforçam a evidência de variância não constante.

par(mfrow = c(1,2))
plot(ajuste, pch = 20, cex = 1.4, which = 3)
plot(ajuste2, pch = 20, cex = 1.4, which = 3)
### Visualmente, o padrão de variância não constante é menos acentuado.

ncvTest(ajuste2) 
### A hipótese de variância constante já não é rejeitada.

### Ajuste 3: regressão linear via mínimos quadrados ponderados.
### Agora, vamos assumir uma forma paramétrica para a relação do desvio
### padrão dos erros com relação à velocidade (x) e estimar os parâmetros.
### Vamos assumir DP(Erros) = theta1 + velocidade^theta2 e usar a função
### gls, pacote nlme, para estimar, conjuntamente e por máxima verossimilhança,
### os betas e os thetas.

ajuste3 <- gls(dist ~ speed, data = cars, weight = varConstPower(form=~speed))
residuos3 <- residuals(ajuste3, type='normalized')
plot(cars$speed, residuos3, xlab = 'Velociadade (mph)', ylab = 'Resíduos',
     pch = 20, cex = 1.5, ylim = c(-2,3))
summary(ajuste3)
### Observe que theta2 (parâmetro de potência) é estimado em 1.022. Assim,
### temos que a relação entre DP(erros) e x (e entre Var(erros) e x) é 
### aproximadamente linear.

compareCoefs(ajuste,ajuste2,ajuste3)