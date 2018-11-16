########################################################################
require(rattle.data)
require(statmod)
require(ISLR)
require(car)
require(hnp)
require(pscl)
require(coefplot)
require(effects)
require(sandwich)
########################################################################
### Exemplo 1

### Dados sobre auditoria na prestação de contas de 3000 indivíduos. 
### As variáveis são as segintes:
### rest: é  a restituição financeira solicitada (em milhares de dólares) 
### audit: e a resposta é o resultado da auditoria (1, o valor
### da restituição estava calculado incorretamente e 0, se estava
### correto). O objetivo é modelar o resultado da auditoria em função
### do valor requisitado para restituição.

auditoria <- read.csv2('auditoria.csv')

x11(width = 10, height = 10)
par(las = 1, mar = c(5,4,2,2), cex = 1.4)
plot(audit ~ rest, data = auditoria, pch = "|", ylim = c(-0.5, 1.5), col = 'blue',
     xlab = 'Valor para restituição', ylab = 'Resultado da auditoria')
### Aparentemente, prestações de contas com maiores valores requeridos
### estão mais propensas a erros.

### Vamos ajustar um modelo de regressão linear. 
ajuste <- lm(audit ~ rest, data = auditoria)
abline(coef(ajuste), col = 'red', lwd = 2)
lines(sort(auditoria$rest), fitted(ajuste)[order(auditoria$rest)], col = 'red', lwd = 2)
### O modelo ajustado claramente não é apropriado. Observe que para determinados
### valores da variável explicativa, temos valor ajustado inferior a zero
### ou superior a 1.

### Vamos contornar isso ajustando um modelo com resposta binomial, o que
### permitirá modelar a probabilidade de uma conta apresentar erros condicional
### ao valor requerido para restituição. Vamos avaliar diferentes funções de
### ligação que podem ser usadas na modelagem de dados binários.

x11(width = 10, height = 10)
par(las = 1, mar = c(5,4,2,2), cex = 1.4)
plot(audit ~ rest, data = auditoria, pch = "|", ylim = c(0,1), col = 'lightblue',
     xlab = 'Valor para restituição', ylab = 'Resultado da auditoria')

ajuste2 <- glm(audit ~ rest, family = binomial(link = logit), data = auditoria)
### Ligação logito.

ajuste3 <- glm(audit ~ rest, family = binomial(link = probit), data = auditoria)
### Ligação probito.

ajuste4 <- glm(audit ~ rest, family = binomial(link = cloglog), data = auditoria)
### Ligação complemento log-log.

ajuste5 <- glm(audit ~ rest, family = binomial(link = cauchit), data = auditoria)
### Ligação Cauchy.

### Vamos adicionar as curvas dos modelos ajustados ao gráfico
lines(sort(auditoria$rest), predict(ajuste2, type = 'response')[order(auditoria$rest)], 
      col = 'black', lwd = 2)
lines(sort(auditoria$rest), predict(ajuste3, type = 'response')[order(auditoria$rest)], 
      col = 'red', lwd = 2)
lines(sort(auditoria$rest), predict(ajuste4, type = 'response')[order(auditoria$rest)], 
      col = 'blue', lwd = 2)
lines(sort(auditoria$rest), predict(ajuste5, type = 'response')[order(auditoria$rest)], 
      col = 'orange', lwd = 2)

legend(x = 'bottomright', lwd = 2, col = c('black', 'red', 'blue', 'orange'),
       legend = c('Logito', 'Probito', 'Cloglog', 'Cauchy'))
### Aparentemente, os modelos com ligação logito e probito proporcionam melhor
### ajuste que os demais. Além disso, os ajustes desses dois modelos são
### bastante semelhantes. Vamos comparar os modelos com base nos respectivos
### AICs.

AIC(ajuste2, ajuste3, ajuste4, ajuste5)
### O ajuste 2 (modelo com ligação logito) produziu menor AIC, sendo preferível.