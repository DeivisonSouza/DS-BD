require(faraway)
require(car)
data("teengamb")
help("teengamb")

x11()
pairs(teengamb, cex = 2, pch = 20)

ajuste <- lm(gamble ~ ., data = teengamb)
summary(ajuste)

### Vamos extrair diferentes tipos de res�duos.

resid(ajuste) ### Res�duos ordin�rios.

QMRes <- summary(ajuste)$sigma^2 
resid(ajuste)/sqrt(QMRes) ### Res�duos padronizados.

rstandard(ajuste) ### Res�duos studentizados.

rstudent(ajuste) ### Res�duos studentizados externamente.

rstandard(ajuste, type="pred") ### Res�duos PRESS. e_{(i)}=y_i-\hat{y}_{(i)}, em que \hat{y}_{(i)} � o valor predito para a i-�sima observa��o pelo modelo ajustado somente com as demais n-1 observa��es.

### An�lise de res�duos.

### Gr�fico de res�duos versus valores ajustados
fit <- fitted(ajuste)
res <- rstudent(ajuste)
par(cex = 1.4, las = 1)
plot(fit, res, xlab = 'Valores ajustados', ylab = 'Res�duos studentizados', pch = 20)
### Fica bastante evidente que os res�duos n�o apresentam vari�ncia constante.

ncvTest(ajuste)
### Teste da hip�tese nula de vari�ncia constante para os res�duos.
### Temos evid�ncia altamente significativa de vari�ncia n�o constante.

shapiro.test(rstudent(ajuste))
### teste da hip�tese nula de normalidade para os res�duos. A hip�tese
### de normalidade � rejeitada.

### Gr�fico quantil-quantil com envelope de confian�a
qqPlot(ajuste, pch = 20, cex = 1.5, xlab = 'Quantis t-Student', ylab = 'Res�duos studentizados')
### Os res�duos n�o apresentam boa ader�ncia � distribui��o t-Student de 
### refer�ncia.

### Res�duos vs vari�veis inclu�das no modelo
par(mfrow=c(2,2))
plot(teengamb$sex, res, xlab = 'Sexo', ylab = 'Res�duos studentizados', pch = 20)
plot(teengamb$status, res, xlab = 'Status', ylab = 'Res�duos studentizados', pch = 20)
plot(teengamb$income, res, xlab = 'Income', ylab = 'Res�duos studentizados', pch = 20)
plot(teengamb$verbal, res, xlab = 'Verbal', ylab = 'Res�duos studentizados', pch = 20)
lines(lowess(res ~ teengamb$verbal), lwd = 2, col = 'red')

### A vari�ncia dos res�duos n�o aparenta ser constante em rela��o �s vari�veis
### sexo e income.

### Gr�ficos de res�duos parciais
par(mfrow=c(2,2))
termplot(ajuste, partial.resid = TRUE, terms = 1, pch = 20, col.res = 'black')
termplot(ajuste, partial.resid = TRUE, terms = 2, pch = 20, col.res = 'black')
termplot(ajuste, partial.resid = TRUE, terms = 3, pch = 20, col.res = 'black')
termplot(ajuste, partial.resid = TRUE, terms = 4, pch = 20, col.res = 'black')
### Os gr�ficos de res�duos parciais evidenciam a rela��o entre sexo e 
### income (linear?) com a resposta (ajustado pelo efeito das demais vari�veis).

### Gr�ficos de diagn�stico (padr�o do R)
par(mfrow = c(2,2))
plot(ajuste, which = 1:4)

### Usando recursos de diagn�stico do pacote car:
x11(width = 15, height = 15)
crPlots(ajuste, cex = 2, pch = 20) 
### Gr�ficos de res�duos parciais

residualPlots(ajuste, cex = 2, pch = 20, tests = FALSE) 
### Gr�fico de res�duos versus vari�veis no modelo.

marginalModelPlots(ajuste, smooth=FALSE) 
### Gr�fico de res�duos marginais (res�duos versus vari�vel n�o ajustada).

influenceIndexPlot(ajuste, cex.lab = 1.4, cex = 1.4, las = 1, pch = 20, vars=c("Cook", "Studentized", "hat"))
### Diagn�stico de outliers, pontos de alavanca e influ�ncia.

######################################################################################
### An�lise - pontos at�picos

### Vamos relevar, por um instante, as demais inadequa��es do ajuste e 
### nos concentrar no efeito da observa��o 24, indicada como outlier e
### observa��o influente.

teengamb[24,]
summary(teengamb)
### Trata-se do adolesente com maior valor gasto em apostas. Vamos reajustar
### o modelo desconsiderando este indiv�duo.

ajuste_v2 <- update(ajuste, subset = -24)
### Agora vamos comparar estimativas e erros padr�es produzidas pelos dois
### modelos.

compareCoefs(ajuste, ajuste_v2)
### Nota-se redu��o expressiva na estimativa referente ao efeito de sexo.
### Os erros padr�es de sexo e income s�o reduzidos ao desconsiderar o
### dado sob investiga��o. De qualquer forma, as conclus�es se mant�m com
### rela��o aos efeitos significativos e n�o significativos na an�lise.

par(mfrow = c(2,2))
plot(ajuste_v2, which = 1:4)
qqPlot(ajuste_v2)
### Mesmo ap�s a exclus�o dos dados do indiv�duo 24 os res�duos ainda apresentam
### vari�ncia n�o constante. Ainda h� um res�duo com valor absoluto pr�ximo 
### a 3 que mereceria alguma aten��o.

dfbetaPlots(ajuste, id.n = 3, cex.lab = 1.4, las = 1, pch = 20)
### Gr�fico para os dfbetas. Novamente a observa��o 24 aparece como potencial
### ponto influente na estimativa de cada par�metro do modelo.

help("influence.measures")
i_med <- influence.measures(ajuste)
summary(i_med)


######################################################################################
### An�lise - multicolinearidade

require(faraway)
require(corrplot)
require(car)
help("seatpos")

### Nosso objetivo aqui � ajustar um modelo de regress�o linear considerando 
### a vari�vel hipcenter como resposta e as demais vari�veis como explicativas.

### Como diagn�stico preliminar de multicolinearidade vamos analisar as correla��es
### bivariadas.

mat_cor <- cor(seatpos)
round(mat_cor, 2)

x11()
corrplot.mixed(mat_cor, upper = 'ellipse', number.cex = 1.2)

### � poss�vel observar correla��es lineares bastante fortes entre as covari�veis,
### algumas delas maiores que 0,90. Fort�ssimo indicador de multicolinearidade.

modelo <- lm(hipcenter ~ . , data = seatpos)
summary(modelo)

### Embora as correla��es bivariadas entre a resposta e as explicativas sejam
### numericamente bastante elevadas, o modelo ajustado n�o indica signific�ncia
### para nenhuma vari�vel explicativa. Esse resultado � contradit�rio, ainda,
### com o valor do R2 (0.687), que � bastante expressivo e pouco compat�vel
### com um modelo sem qualquer vari�vel associada � resposta.

### Retornando ao exemplo, vamos calcular os valores de VIF. Primeiro vamos calcular
### um deles na m�o:

ajuste_Leg <- lm(Leg ~ Age + Weight + HtShoes + Ht + Seated + Arm + Thigh, data = seatpos)
R2 <- summary(ajuste_Leg)$r.squared
vif_Leg <- (1/(1-R2)) 

vif(modelo)
### H� muita colinearidade nos dados. Se extrairmos a raiz quadrada de qualquer
### um desses valores, teremos quantas vezes o respectivo erro padr�o � maior
### do que aquele que ter�amos se as vari�veis fossem ortogonais.

sqrt(vif(modelo))

### Com exce��o da idade, as demais vari�veis explicativas est�o, em geral,
### fortemente correlacionadas. O que aconteceria se us�ssemos apenas uma
### delas na especifica��o do modelo? Vamos considerar a altura aferida
### com o indiv�duo descal�o.

modelo3 <- lm(hipcenter ~ Ht, data = seatpos)
summary(modelo3)

### A associa��o entre a altura e a vari�vel resposta � altamente significativa. 
### Al�m disso, a varia��o no coeficiente de determina��o, resultante da
### elimina��o de todas as outras vari�veis, foi bem pequena. Algo semelhante
### ocorreria se escolhessemos alguma outra vari�vel ao inv�s de Ht. Fica
### como exerc�cio.