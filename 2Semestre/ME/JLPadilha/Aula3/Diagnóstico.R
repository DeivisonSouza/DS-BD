require(faraway)
require(car)
data("teengamb")
help("teengamb")

x11()
pairs(teengamb, cex = 2, pch = 20)

ajuste <- lm(gamble ~ ., data = teengamb)
summary(ajuste)

### Vamos extrair diferentes tipos de resíduos.

resid(ajuste) ### Resíduos ordinários.

QMRes <- summary(ajuste)$sigma^2 
resid(ajuste)/sqrt(QMRes) ### Resíduos padronizados.

rstandard(ajuste) ### Resíduos studentizados.

rstudent(ajuste) ### Resíduos studentizados externamente.

rstandard(ajuste, type="pred") ### Resíduos PRESS. e_{(i)}=y_i-\hat{y}_{(i)}, em que \hat{y}_{(i)} é o valor predito para a i-ésima observação pelo modelo ajustado somente com as demais n-1 observações.

### Análise de resíduos.

### Gráfico de resíduos versus valores ajustados
fit <- fitted(ajuste)
res <- rstudent(ajuste)
par(cex = 1.4, las = 1)
plot(fit, res, xlab = 'Valores ajustados', ylab = 'Resíduos studentizados', pch = 20)
### Fica bastante evidente que os resíduos não apresentam variância constante.

ncvTest(ajuste)
### Teste da hipótese nula de variância constante para os resíduos.
### Temos evidência altamente significativa de variância não constante.

shapiro.test(rstudent(ajuste))
### teste da hipótese nula de normalidade para os resíduos. A hipótese
### de normalidade é rejeitada.

### Gráfico quantil-quantil com envelope de confiança
qqPlot(ajuste, pch = 20, cex = 1.5, xlab = 'Quantis t-Student', ylab = 'Resíduos studentizados')
### Os resíduos não apresentam boa aderência à distribuição t-Student de 
### referência.

### Resíduos vs variáveis incluídas no modelo
par(mfrow=c(2,2))
plot(teengamb$sex, res, xlab = 'Sexo', ylab = 'Resíduos studentizados', pch = 20)
plot(teengamb$status, res, xlab = 'Status', ylab = 'Resíduos studentizados', pch = 20)
plot(teengamb$income, res, xlab = 'Income', ylab = 'Resíduos studentizados', pch = 20)
plot(teengamb$verbal, res, xlab = 'Verbal', ylab = 'Resíduos studentizados', pch = 20)
lines(lowess(res ~ teengamb$verbal), lwd = 2, col = 'red')

### A variância dos resíduos não aparenta ser constante em relação às variáveis
### sexo e income.

### Gráficos de resíduos parciais
par(mfrow=c(2,2))
termplot(ajuste, partial.resid = TRUE, terms = 1, pch = 20, col.res = 'black')
termplot(ajuste, partial.resid = TRUE, terms = 2, pch = 20, col.res = 'black')
termplot(ajuste, partial.resid = TRUE, terms = 3, pch = 20, col.res = 'black')
termplot(ajuste, partial.resid = TRUE, terms = 4, pch = 20, col.res = 'black')
### Os gráficos de resíduos parciais evidenciam a relação entre sexo e 
### income (linear?) com a resposta (ajustado pelo efeito das demais variáveis).

### Gráficos de diagnóstico (padrão do R)
par(mfrow = c(2,2))
plot(ajuste, which = 1:4)

### Usando recursos de diagnóstico do pacote car:
x11(width = 15, height = 15)
crPlots(ajuste, cex = 2, pch = 20) 
### Gráficos de resíduos parciais

residualPlots(ajuste, cex = 2, pch = 20, tests = FALSE) 
### Gráfico de resíduos versus variáveis no modelo.

marginalModelPlots(ajuste, smooth=FALSE) 
### Gráfico de resíduos marginais (resíduos versus variável não ajustada).

influenceIndexPlot(ajuste, cex.lab = 1.4, cex = 1.4, las = 1, pch = 20, vars=c("Cook", "Studentized", "hat"))
### Diagnóstico de outliers, pontos de alavanca e influência.

######################################################################################
### Análise - pontos atípicos

### Vamos relevar, por um instante, as demais inadequações do ajuste e 
### nos concentrar no efeito da observação 24, indicada como outlier e
### observação influente.

teengamb[24,]
summary(teengamb)
### Trata-se do adolesente com maior valor gasto em apostas. Vamos reajustar
### o modelo desconsiderando este indivíduo.

ajuste_v2 <- update(ajuste, subset = -24)
### Agora vamos comparar estimativas e erros padrões produzidas pelos dois
### modelos.

compareCoefs(ajuste, ajuste_v2)
### Nota-se redução expressiva na estimativa referente ao efeito de sexo.
### Os erros padrões de sexo e income são reduzidos ao desconsiderar o
### dado sob investigação. De qualquer forma, as conclusões se mantêm com
### relação aos efeitos significativos e não significativos na análise.

par(mfrow = c(2,2))
plot(ajuste_v2, which = 1:4)
qqPlot(ajuste_v2)
### Mesmo após a exclusão dos dados do indivíduo 24 os resíduos ainda apresentam
### variância não constante. Ainda há um resíduo com valor absoluto próximo 
### a 3 que mereceria alguma atenção.

dfbetaPlots(ajuste, id.n = 3, cex.lab = 1.4, las = 1, pch = 20)
### Gráfico para os dfbetas. Novamente a observação 24 aparece como potencial
### ponto influente na estimativa de cada parâmetro do modelo.

help("influence.measures")
i_med <- influence.measures(ajuste)
summary(i_med)


######################################################################################
### Análise - multicolinearidade

require(faraway)
require(corrplot)
require(car)
help("seatpos")

### Nosso objetivo aqui é ajustar um modelo de regressão linear considerando 
### a variável hipcenter como resposta e as demais variáveis como explicativas.

### Como diagnóstico preliminar de multicolinearidade vamos analisar as correlações
### bivariadas.

mat_cor <- cor(seatpos)
round(mat_cor, 2)

x11()
corrplot.mixed(mat_cor, upper = 'ellipse', number.cex = 1.2)

### É possível observar correlações lineares bastante fortes entre as covariáveis,
### algumas delas maiores que 0,90. Fortíssimo indicador de multicolinearidade.

modelo <- lm(hipcenter ~ . , data = seatpos)
summary(modelo)

### Embora as correlações bivariadas entre a resposta e as explicativas sejam
### numericamente bastante elevadas, o modelo ajustado não indica significância
### para nenhuma variável explicativa. Esse resultado é contraditório, ainda,
### com o valor do R2 (0.687), que é bastante expressivo e pouco compatível
### com um modelo sem qualquer variável associada à resposta.

### Retornando ao exemplo, vamos calcular os valores de VIF. Primeiro vamos calcular
### um deles na mão:

ajuste_Leg <- lm(Leg ~ Age + Weight + HtShoes + Ht + Seated + Arm + Thigh, data = seatpos)
R2 <- summary(ajuste_Leg)$r.squared
vif_Leg <- (1/(1-R2)) 

vif(modelo)
### Há muita colinearidade nos dados. Se extrairmos a raiz quadrada de qualquer
### um desses valores, teremos quantas vezes o respectivo erro padrão é maior
### do que aquele que teríamos se as variáveis fossem ortogonais.

sqrt(vif(modelo))

### Com exceção da idade, as demais variáveis explicativas estão, em geral,
### fortemente correlacionadas. O que aconteceria se usássemos apenas uma
### delas na especificação do modelo? Vamos considerar a altura aferida
### com o indivíduo descalço.

modelo3 <- lm(hipcenter ~ Ht, data = seatpos)
summary(modelo3)

### A associação entre a altura e a variável resposta é altamente significativa. 
### Além disso, a variação no coeficiente de determinação, resultante da
### eliminação de todas as outras variáveis, foi bem pequena. Algo semelhante
### ocorreria se escolhessemos alguma outra variável ao invés de Ht. Fica
### como exercício.