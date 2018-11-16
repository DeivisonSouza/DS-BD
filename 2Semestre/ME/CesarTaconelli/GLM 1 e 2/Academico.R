########################################################################
### Regressão para dados de contagens - produção acadêmica

data(bioChemists)
help(bioChemists)

### Análise descritiva
x11(width = 12, height = 10)
par(las = 1, mar = c(5,4.5,2,2), cex = 1.4)
plot(table(bioChemists$art), col = 'blue', lwd = 5, xlab = 'Número de artigos',
     ylab = 'Frequência', xaxt = 'n')
axis(1, 0:20) 

d1 <- with(bioChemists, data.frame(table(fem, factor(art, levels = 0:20))))
names(d1) <- c("fem","art","Freq")
ggplot(data = d1, aes(x = art, y = Freq)) + geom_bar(stat = "identity")+ 
    facet_wrap(~ fem) + theme_bw(base_size = 18) + ylab("Frequência") + xlab('Número de artigos')

d2 <- with(bioChemists, data.frame(table(mar, factor(art, levels = 0:20))))
names(d1) <- c("mar","art","Freq")
ggplot(data = d1, aes(x = art, y = Freq)) + geom_bar(stat = "identity")+ 
    facet_wrap(~ mar) + theme_bw(base_size = 18) + ylab("Frequência") + xlab('Número de artigos')

d3 <- with(bioChemists, data.frame(table(kid5, factor(art, levels = 0:20))))
names(d3) <- c("kid5","art","Freq")
ggplot(data = d3, aes(x = art, y = Freq)) + geom_bar(stat = "identity")+ ggtitle('Filhos com menos de cinco anos') +
    facet_wrap(~ kid5) + theme_bw(base_size = 18) + ylab("Frequência") + xlab('Número de artigos')

ggplot(data = bioChemists, aes(x = phd, y = art)) + geom_point() +
    theme_bw(base_size = 18) + ylab("Número de artigos") + xlab('Prestígio') +
    geom_smooth(method = 'loess')

ggplot(data = bioChemists, aes(x = ment, y = art)) + geom_jitter() +
    theme_bw(base_size = 18) + ylab("Número de artigos") + xlab('Artigos - orientador') +
    geom_smooth(method = 'loess')

### Processo de modelagem

### Começamos com o ajuste de um modelo linear (regressão com resposta normal).

ajuste0 <- glm(art ~ fem + mar + kid5 + phd + ment, family = poisson, data = bioChemists)
hnp(ajuste0) ### Modelo linear não se ajusta bem aos dados.

ajuste1 <- glm(art ~ fem + mar + kid5 + phd + ment, family = poisson, data = bioChemists)
hnp(ajuste1) ### Modelo log-linear com resposta Poisson também não se ajusta bem. 

ajuste2 <- glm.nb(art ~ fem + mar + kid5 + phd + ment, data = bioChemists)
hnp(ajuste2) ### Modelo log-linear com resposta binomial negativa tem melhor
### ajuste. Vamos checar com gráficos adicionais para os resíduos.

### Vamos prosseguir com o diagnóstico do ajuste, baseado na análise de resíduos 
par(mfrow = c(2,2), cex = 1.4)
plot(ajuste1)

residuos <- qres.nbinom(ajuste2)
ajustados <- predict(ajuste2)
x11(width = 12, height = 10)
par(las = 1, mar = c(5,4.5,2,2), mfrow = c(1,2), cex = 1.4)
plot(residuos ~ ajustados, col = 'blue', xlab = 'Valores ajustados', ylab = 'Resíduos')
lines(lowess(residuos ~ ajustados), col = 'red', lwd = 2)
qqnorm(residuos, col = 'blue', main = '')
qqline(residuos, lty = 2)
### A curvatura da função suave, presente no gráfico à esquerda, é resultante
### da pequena quantidade de pontos para maiores valores ajustados.

summary(ajuste2)
### O efeito da variável phd é não significativo. Vamos removê-la do modelo.

ajuste3 <- update(ajuste2, ~.-phd)
summary(ajuste3)
### As demais variáveis tem efeito significativo (ao menos ao nível de 10%).

### Vamos avaliar possível efeito quadrático para as variáveis numéricas.
ajuste4 <- update(ajuste3, ~.+I(kid5^2))
anova(ajuste3, ajuste4) ### Efeito quadrático de kid5 não significativo.

ajuste4 <- update(ajuste3, ~.+I(phd^2))
anova(ajuste3, ajuste4) ### Efeito quadrático de phd não significativo.

### Diagnóstico de outliers e influência
influenceIndexPlot(ajuste3,vars=c('Studentized', 'Cook', 'Hat'), id.n=3)
summary(influence.measures(ajuste2))
### Vamos avaliar o efeito de três observações destacadas nesta análise:
### 328, 911, 915. Para isso, vamos ajustar um novo modelo após retirada
### das três observações e avaliar o impacto nas estimativas e erros padrões
### ajustados.

ajuste4 <- update(ajuste3, data = bioChemists[-c(328, 911, 915),])
compareCoefs(ajuste3, ajuste4)
multiplot(ajuste3, ajuste4)
### As alterações nas estimativas e erros são pouco expressivas, resultante,
### possivelmente, da quantidade de dados.

### Vamos proceder com algumas inferências.

confint(ajuste3) 
### Intervalos de confiança para os betas baseados nos perfis de verossimilhança.

confint.default(ajuste3) 
### Intervalos de confiança de Wald.

exp(confint(ajuste3))
### Limites exponenciados, para fins de interpretação.

### Gráficos de efeitos
plot(allEffects(ajuste3)) ### Escala do link.
plot(allEffects(ajuste3), type = 'response') ### Escala da resposta.

### Vamos fazer predições aproveitando cinco linhas da base escolhidas ao
### acaso. 
samp <- sample(1:nrow(bioChemists), size = 5)
basepred <- bioChemists[samp,]
basepred

### Predição na escala do link para posterior construção de ICs 95% na escala
### da resposta.
p_link <- predict(ajuste3, newdata = basepred, type = 'link', se.fit = TRUE)
p_link
int_link <- cbind(p_link$fit - 1.96 * p_link$se.fit, p_link$fit + 1.96 * p_link$se.fit)
int_link
exp(int_link)

### Predição na escala da resposta.
p_resp <- predict(ajuste3, newdata = basepred, type = 'response', se.fit = TRUE)
p_resp

### Agora, como alternativa à distribuição binomial negativa, vamos usar
### modelos de quase verossimilhança.

ajusteq1 <- glm(art ~ fem + mar + kid5 + ment, family = quasipoisson, data = bioChemists)
### Quase Poisson - função de ligação logarítmica, função de variância linear.

ajusteq2 <- glm(art ~ fem + mar + kid5 + ment, family = quasi(link = 'log', variance = 'mu^2'),
                data = bioChemists)
### Função de ligação logaritmica, função de variância quadrática.

sandwich_se <- diag(vcovHC(ajuste3, type = "HC"))^0.5
### Erros padrões robustos, usando estimador sanduíche.


### Vamos comparar os erros padrões gerados pelos modelos ajustados. Para
### efeito de comparação, vamos ajustar novamente o modelo de Poisson,
### desta vez excluindo a variável phd.

ajuste1 <- glm(art ~ fem + mar + kid5 + ment, family = poisson, data = bioChemists)

mat_se <- data.frame(poisson = summary(ajuste1)$coefficients[,2],
                     Nbin = summary(ajuste3)$coefficients[,2],
                     Quasi1 = summary(ajusteq1)$coefficients[,2],
                     Quasi2 = summary(ajusteq2)$coefficients[,2],
                     sandwich = sandwich_se)
round(mat_se, 3)
### No modelo de Poisson os erros são claramente subestimados. Para os demais
### modelos os resultados são bastante semelhantes.