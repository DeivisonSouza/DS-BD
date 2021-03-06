########################################################################
########################################################################
########################################################################
### Aula - Transforma��es, pondera��o e regress�o robusta.

########################################################################
### Exemplo 1 - Tranforma��o na resposta - o m�todo de Box-Cox.

require(faraway)
require(car)
help(ozone)

### Vamos fazer uma an�lise de regress�o linear considerando a concentra��o
### de oz�nio como resposta e temperatura, unidade e ibh como preditoras.

ozone2 <- subset(ozone, select = c('O3', 'temp', 'humidity', 'ibh'))
pairs(ozone2, pch = 20, cex = 1.4)
### Podemos observar rela��es n�o lineares, evidente vari�ncia n�o constante,
### alguma evid�ncia de assimetria. Claramente s�o apenas evid�ncias baseadas
### em descritivas bivariadas.

ajuste <- lm(O3 ~ temp + humidity + ibh, data = ozone2)
par(mfrow = c(2,2))
plot(ajuste) ### H� fortes ind�cios de vari�ncia n�o constante para os erros,
### alguma evid�ncia de n�o normalidade.
ncvTest(ajuste) ### A hip�tese de vari�ncia constante � rejeitada.
shapiro.test(rstandard(ajuste)) 

### Vamos usar o m�todo de Box-Cox para identificar uma transforma��o adequada.
boxCox(ajuste, lambda = seq(0,1,0.1))

### Uma transforma��o do tipo raiz c�bica � indicada.

ozone2$O3_trans <- ozone2$O3^(1/3) ### Vari�vel transformada.
ajuste2 <- lm(O3_trans ~ temp + humidity + ibh, data = ozone2)
par(mfrow = c(2,2))
plot(ajuste2)
ncvTest(ajuste2)
shapiro.test(rstandard(ajuste2))
### Observe que n�o h� mais evid�ncias contr�rias �s hip�teses de vari�ncia
### constante e normalidade. Os gr�ficos de res�duos t�m um padr�o bem mais
### adequado, indicando um bom ajuste.

### Vamos fazer as predi��es e produzir um gr�fico de efeitos. Vamos fixar
### temperatura e ibh na m�dia, e predizer a concentra��o de oz�nio para 
### uma sequ�ncia de valores para umidade.

novo_humidity <- seq(min(ozone2$humidity), max(ozone2$humidity), length.out = 100)
### Grid de valores para umidade.

novos_dados <- expand.grid(temp = mean(ozone2$temp), ibh = mean(ozone2$ibh), 
                           humidity = novo_humidity)
### Criando a base para predi��o.

predic <- predict(ajuste2, newdata = novos_dados, interval = "prediction")
predic_orig <- predic^3 ### Predi��es na escala original.

plot(novo_humidity, predic_orig[,1], type = 'l', ylim = c(0,30), col = 'red')
lines(novo_humidity, predic_orig[,2], col = 'red', lty = 2)
lines(novo_humidity, predic_orig[,3], col = 'red', lty = 2)
### Gr�fico de efeitos.

########################################################################
### Exemplo 2 - M�nimos quadrados ponderados. Vamos utilizar a base de 
### dados cars, dispon�vel na base do R.

require(car) 
require(nlme) 

help(cars) 
head(cars,10) 
summary(cars) 

par(cex = 1.4, las = 1)
plot(cars,pch=20,xlab='Velocidade (mph)',ylab='Dist�ncia de frenagem (m)') 
with(cars,lines(lowess(dist~speed),col='red',lwd=2)) 

### A dispers�o de y parece aumentar conforme x.


### Ajuste 1: regress�o linear via m�nimos quadrados ordin�rios.

ajuste <- lm(dist~speed,data = cars) 
summary(ajuste) 

par(mfrow = c(2,2))
plot(ajuste, pch = 20, cex = 1.4)
### Os res�duos refor�am a evid�ncia de vari�ncia n�o constante.

ncvTest(ajuste) 
### Como a hip�tese nula � a de vari�ncia constante para os erros, temos evid�ncia
### significativa de vari�ncia n�o constante ao n�vel de 5%.


### Ajuste 2: regress�o linear via m�nimos quadrados ponderados.
### vamos assumir que a vari�ncia aumenta linearmente conforme a velocidade (x).
### Assim, os pesos v�o ser definidos por 1/x.

ajuste2 <- lm(dist ~ speed, weights = 1/speed, data = cars) 
summary(ajuste) 
compareCoefs(ajuste, ajuste2) ### Compara��o das estimativas e erros 
### fornecidas pelos dois modelos.

plot(ajuste2, pch = 20, cex = 1.4)
### Os res�duos refor�am a evid�ncia de vari�ncia n�o constante.

par(mfrow = c(1,2))
plot(ajuste, pch = 20, cex = 1.4, which = 3)
plot(ajuste2, pch = 20, cex = 1.4, which = 3)
### Visualmente, o padr�o de vari�ncia n�o constante � menos acentuado.

ncvTest(ajuste2) 
### A hip�tese de vari�ncia constante j� n�o � rejeitada.

### Ajuste 3: regress�o linear via m�nimos quadrados ponderados.
### Agora, vamos assumir uma forma param�trica para a rela��o do desvio
### padr�o dos erros com rela��o � velocidade (x) e estimar os par�metros.
### Vamos assumir DP(Erros) = theta1 + velocidade^theta2 e usar a fun��o
### gls, pacote nlme, para estimar, conjuntamente e por m�xima verossimilhan�a,
### os betas e os thetas.

ajuste3 <- gls(dist ~ speed, data = cars, weight = varConstPower(form=~speed))
residuos3 <- residuals(ajuste3, type='normalized')
plot(cars$speed, residuos3, xlab = 'Velociadade (mph)', ylab = 'Res�duos',
     pch = 20, cex = 1.5, ylim = c(-2,3))
summary(ajuste3)
### Observe que theta2 (par�metro de pot�ncia) � estimado em 1.022. Assim,
### temos que a rela��o entre DP(erros) e x (e entre Var(erros) e x) � 
### aproximadamente linear.

compareCoefs(ajuste,ajuste2,ajuste3)