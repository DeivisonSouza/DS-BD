require(MASS)
help(cats)
data(cats)

x11()
par(cex = 1.4, las = 1)
plot(Hwt ~ Bwt, data = cats, xlab = 'Peso corporal(kg)',
     ylab = 'Peso do cora??o (g)', pch = 20)

ajuste <- lm(Hwt ~ Bwt, data = cats)

ajuste # F?rmula do modelo e estimativas dos par?metros.
summary(ajuste) # Resumo do modelo ajustado.
names(ajuste) # relaciona todos os elementos produzidos pelo ajuste da regress?o linear simples.
coefficients(ajuste) # Estimativas
fitted(ajuste) # Valores ajustados pelo modelo para os indiv?duos da base.
residuals(ajuste) # Res?duos.
plot(fitted(ajuste), residuals(ajuste), pch = 20, xlab = 'Valores ajustados', ylab = 'Res?duos')

plot(Hwt ~ Bwt, data = cats, xlab = 'Peso corporal(kg)',
     ylab = 'Peso do cora??o (g)', pch = 20)
abline(coefficients(ajuste), col='red', lwd=2) 
# adiciona a reta ajustada ao gr?fico de dispers?o.

confint(ajuste) # Intervalos de confian?a (95%) para os par?metros do modelo.
confint(ajuste,level=0.99) # Intervalos de confian?a (99%) para os par?metros do modelo.

anova(ajuste) # Quadro da an?lise de vari?ncia.

predict(ajuste) # Predi??es (valores ajustados) para cada indiv?duo da base.. 
predict(ajuste, interval = 'confidence') 
# Intervalos de confian?a 95% para a resposta m?dia (calculados para cada x na amostra).
predict(ajuste, interval = 'confidence', level = 0.99) 
# Intervalos de confiança 99% para a resposta média (calculados para cada valor de x na amostra).
predict(ajuste, interval = 'prediction') 
# intervalos de predição 95% (calculados para cada valor de x na amostra).
predict(ajuste, interval = 'prediction',level = 0.99) 
# intervalos de predição 99%  (calculados para cada valor de x na amostra).

### Vamos estimar a resposta média e fazer previsões para valores de x diferentes dos valores na amostra.
predict(ajuste, newdata=data.frame(Bwt = 2.65), interval='confidence')
predict(ajuste, newdata=data.frame(Bwt = c(2.65, 2.76, 3.02)), interval='confidence')
predict(ajuste, newdata=data.frame(Bwt = 2.65), interval='prediction')
predict(ajuste, newdata=data.frame(Bwt = c(2.65, 2.76, 3.02)), interval='prediction')


### Agora vamos adicionar ao gráfico as basndas de confiança e de predição.
ndados <- data.frame(Bwt = seq(2, 4, length.out = 100)) 
cbind(ndados, predict(ajuste, newdata = ndados))
# ndados armazena um grid de valores de x para estima??o e predi??o.

conf1 <- predict(ajuste, newdata = ndados, interval = 'confidence') 
pred1 <- predict(ajuste, newdata = ndados, interval = 'prediction') 
lines(ndados$Bwt, conf1[,'lwr'], lty=2) 
lines(ndados$Bwt, conf1[,'upr'], lty=2) 
lines(ndados$Bwt, pred1[,'lwr'], lty=2, col='red') 
lines(ndados$Bwt, pred1[,'upr'], lty=2, col='red') 
