########################################################################
### Aula - Regress�o linear com covari�veis categ�ricas.

help(PlantGrowth)
head(PlantGrowth)

PlantGrowth$group <- factor(PlantGrowth$group, levels = c('ctrl', 'trt1', 'trt2'))

### Vamos analisar a produ��o de massa seca sob as tr�s condi��es experimentais
### (controle e dois tratamentos).

### Inicialmente uma an�lise descritiva.
boxplot(weight ~ group, data = PlantGrowth, xlab = 'Grupo', ylab = 'Horas extras',
        las = 1)
with(PlantGrowth, by(weight, group, summary))

########################################################################
### Vamos considerar, num primeiro momento, apenas o grupo controle e o
### grupo com tratamento 1 (trt1)

PlantGrowth2 <- subset(PlantGrowth, group %in% c('ctrl', 'trt1'))

ajuste1 <- lm(weight ~ group, data = PlantGrowth2)
model.matrix(ajuste1)
summary(ajuste1)

### Neste caso, o intercepto corresponde � estimativa do peso m�dio das
### plantas do grupo controle (5.03). A segunda estimativa apresentada
### corresponde � diferen�a no peso m�dio de plantas submetidas ao
### tratamento 1 em rela��o a plantas do grupo controle (-0.37). 
### A estimativa para o peso m�dio de plantas submetidas ao tratamento 1
### � calculada por 5.03 - 0.37 = 4.66.


### Vamos analisar os dados agora usando um teste t de compara��o de m�dias,
### assumindo vari�ncias iguais nos dois grupos.

t.test(weight ~ group, data = PlantGrowth2, var.equal = TRUE)
### Observe que o resultado do teste, bem como as estimativas apresentadas,
### s�o rigorosamente iguais �s produzidas pelo modelo de regress�o linear.


########################################################################
### Vamos continuar a an�lise considerando os tr�s grupos.

ajuste2 <- lm(weight ~ group, data = PlantGrowth)
model.matrix(ajuste2)
summary(ajuste2)

### O intercepto novamente corresponde � estimativa do peso m�dio de plantas
### em condi��es semelhantes �s do grupo controle. As demais estimativas
### correspondem �s diferen�as nas m�dias para cada grupo tratado em rela��o
### ao grupo controle.

### Os resultados n�o apontam diferen�a significativa de peso, ao n�vel de 5%, para
### plantas submetidas ao tratamento 1 (p = 0.1944) e ao tratamento 2 (p = 0.0877)
### em rela��o ao grupo controle.

### Vamos usar o teste F da an�lise de vari�ncia para avaliar a diferen�a
### entre os grupos.

anova(ajuste2)
### O teste F indica diferen�a significativa, ao n�vel de signific�ncia de
### 5%, para as m�dias dos tr�s grupos.

### Mas o que dizer sobre a diferen�a dos efeitos dos tratamentos 1 e 2?

### Podemos redefinir a categoria de refer�ncia da vari�vel grupo:
PlantGrowth$group <- relevel(PlantGrowth$group, ref = 'trt2')

ajuste3 <- lm(weight ~ group, data = PlantGrowth)
model.matrix(ajuste3)
summary(ajuste3)

### H� diferen�a significativa no peso m�dio de plantas sob os tratamentos 1 e 2.
### Estima-se, em m�dia, 0.8650 a menos de peso sob tratamento 1 em rela��o ao
### tratamento 2.

### Ao inv�s de redefinir o intercepto, vamos usar a fun��o glht do pacote 
### multcomp para estimar a diferen�a entre os dois tratamentos.

require(multcomp)
### Vamos retomar o ajuste2, em que o grupo controle � a refer�ncia.

### Estimativa da diferen�a dos pesos m�dios sob os tratamentos 1 e 2.
L <- rbind('Trat 1 - Trat 2' = c(0, 1, -1)); L
g2 <- glht(ajuste2, linfct = L); g2
summary(g2)
confint(g2)

### E se quisermos as estimativas dos pesos m�dios para cada grupo?
### Para o grupo controle a estimativa � simplesmente o intercepto do 
### modelo ajustado (beta0), e o resultado est� no summary. Para o tratamento
### 1 devemos estimar beta0 + beta1 e para o tratamento 2 beta0 + beta2:

L <- rbind('Trat 1' = c(1, 1, 0),
           'Trat 2' = c(1, 0, 1)); L
g2 <- glht(ajuste2, linfct = L); g2
### estimativas pontuais para cada grupo.

summary(g2)
confint(g2)
### Intervalos de confian�a.

########################################################################
### Agora vamos ajustar o modelo sem o intercepto.

contrasts(PlantGrowth$group)
### Padr�o do R, contrastes de tratamentos, primeiro n�vel como refer�ncia.

### Remo��o do intercepto:

ajuste4 <- lm(weight ~ group - 1, data = PlantGrowth)
model.matrix(ajuste4)
summary(ajuste4)
confint(ajuste4)

### Cada estimativa agora corresponde � m�dia sob uma das condi��es experimentais.


########################################################################
### Regress�o com covari�veis quantitativas e qualitativas.
### Exemplo 1 - Dados de rea��o enzim�tica com c�lulas tratadas ou n�o
### com Puromycin. Nesse caso a vari�vel qualitativa tem dois n�veis
### (tratado ou n�o tratado).

require(ggplot2)

help("Puromycin")

ggplot(aes(x = conc, y = rate, color = state), data = Puromycin) + geom_point() +
    theme_bw(base_size = 18)

ggplot(aes(x = conc, y = rate), data = Puromycin) + geom_point() + 
    facet_grid(~state) + theme_bw(base_size = 18) +
    xlab('Concentra��o de substrato (ppm)') + 
    ylab('Taxa de rea��o (contagem/min^2)')

### A rela��o entre a concentra��o de substrato e a taxa de rea��o �
### claramente n�o linear. Vamos prosseguir a an�lise considerando transforma��o
### logar�tmica para a concentra��o.

### Vamos ajustar quatro modelos, em ordem crescente de complexidade.

### Ajuste 1 - Modelo nulo, sem efeito de concentra��o.
ajuste1 <- lm(rate ~ 1, data = Puromycin)

### Ajuste2 - Modelo com efeito de concentra��o, sem efeito de tratamento.
ajuste2 <- lm(rate ~ I(log2(conc)), data = Puromycin)

### Ajuste3 - Modelo com efeitos aditivos de concentra��o e tratamento
### (sem intera��o)
ajuste3 <- lm(rate ~ I(log2(conc)) + state, data = Puromycin)

### Ajuste4 - Modelo com efeito multiplicativo de concentra��o e tratamento
### (com intera��o)
ajuste4 <- lm(rate ~ I(log2(conc)) * state, data = Puromycin)

anova(ajuste1, ajuste2, ajuste3, ajuste4)

### De acordo com os resultados:

### A adi��o da (log)concentra��o ao modelo nulo tem efeito significativo
### (F=518.87, p < 0.001);

### A adi��o do tratamento ao modelo ajustado pela (log) concentra��o
### tem efeito significativo (F=43.258, p < 0.001);

### A adi��o da intera��o entre tratamento e (log) concentra��o ao modelo
### aditivo tem efeito significativo (F=11.892, p = 0.0026).

### Dessa forma, o modelo selecionado para descrever a rela��o entre a
### concentra��o de substrato e taxa de rea��o requer tanto interceptos
### quanto slopes distintos para cada tratamento.

summary(ajuste4)

### Gr�ficos de res�duos
par(mfrow = c(2,2))
plot(ajuste4)
### Acrescente seus coment�rios, produza resultados complementares.

### Vamos plotar os ajustes. Para isso, primeiro vamos criar uma base para
### predi��o.
new_conc <- seq(min(Puromycin$conc), max(Puromycin$conc), length.out = 100)
trat <- c('treated', 'untreated')
new_data <- expand.grid(new_conc, trat)
names(new_data) <- c('conc', 'state')

### Agora o gr�fico.
ggplot(aes(x = conc, y = rate), data = Puromycin) + geom_point() + 
    facet_grid(~state) + theme_bw(base_size = 18) +
    xlab('Concentra��o de substrato (ppm)') + 
    ylab('Taxa de rea��o (contagem/min^2)') +
    geom_line(data = new_data, 
          aes(x = conc, y=predict(ajuste4, newdata=new_data)), 
          color='blue')

########################################################################
### Regress�o com covari�veis quantitativas e qualitativas.
### Exemplo 2 - Dados de logevidade de moscas machos submetidas a diferentes
### condi��es experimentais. Consulte a documenta��o.

help("fruitfly")

ggplot(aes(x = thorax, y = longevity, color = activity), data = fruitfly) + geom_point() +
    theme_bw(base_size = 18)

ggplot(aes(x = thorax, y = longevity), data = fruitfly) + geom_point() + 
    facet_grid(~activity) + theme_bw(base_size = 18) +
    xlab('Tamanho do torax') + 
    ylab('Tempo de vida (em dias)')

### Vamos ajustar quatro modelos, em ordem crescente de complexidade.

### Ajuste 1 - Modelo nulo, sem efeito de concentra��o.
ajuste1 <- lm(longevity ~ 1, data = fruitfly)

### Ajuste2 - Modelo com efeito de concentra��o, sem efeito de tratamento.
ajuste2 <- lm(longevity ~ thorax, data = fruitfly)

### Ajuste3 - Modelo com efeitos aditivos de concentra��o e tratamento
### (sem intera��o)
ajuste3 <- lm(longevity ~ thorax + activity, data = fruitfly)

### Ajuste4 - Modelo com efeito multiplicativo de concentra��o e tratamento
### (com intera��o)
ajuste4 <- lm(longevity ~ thorax * activity, data = fruitfly)

anova(ajuste1, ajuste2, ajuste3, ajuste4)

### De acordo com os resultados:

### A adi��o do tamanho do torax ao modelo nulo tem efeito significativo
### (F=130.733, p < 0.001);

### A adi��o do tratamento ao modelo ajustado pelo tamanho do torax
### tem efeito significativo (F=20.988, p < 0.001);

### A adi��o da intera��o entre tamanho do torax e tratamento ao modelo
### aditivo n�o tem efeito significativo (F=0.053, p = 0.9947).

### Dessa forma, o modelo selecionado para descrever a rela��o entre a
### concentra��o de substrato e taxa de rea��o requer um intercepto para
### cada tratamento, mas um �nico slope.

summary(ajuste3)

### Gr�ficos de res�duos
par(mfrow = c(2,2))
plot(ajuste3)
### Os res�duos visivelmente n�o tem vari�ncia constante. Fica como tarefa
### encontrar uma transforma��o que contorne isso. Como o objetivo aqui �
### simplesmente obter as restas ajustadas (sem procupa��o quanto aos erros
### ou outras infer�ncias, prosseguimos com o modelo original.)

### Vamos plotar os ajustes. Para isso, primeiro vamos criar uma base para
### predi��o.
new_thorax <- seq(min(fruitfly$thorax), max(fruitfly$thorax), length.out = 100)
trat <- c('isolated', 'one', 'low', 'many', 'high')
new_data <- expand.grid(new_thorax, trat)
names(new_data) <- c('thorax', 'activity')

### Agora o gr�fico.
ggplot(aes(x = thorax, y = longevity), data = fruitfly) + geom_point() + 
    facet_grid(~activity) + theme_bw(base_size = 18) +
    xlab('Concentra��o de substrato (ppm)') + 
    ylab('Taxa de rea��o (contagem/min^2)') +
    geom_smooth(method='lm')

fruitfly$activity2 <- fruitfly$activity 
levels(fruitfly$activity2) <- c('iso or preg', 'iso or preg', 'virg1', 'iso or preg', 'virg2')
ajuste3_alt <- lm(longevity ~ thorax + activity2, data = fruitfly)
summary(ajuste3_alt)
anova(ajuste3_alt, ajuste3)

### Vamos testar agora se uma �nica reta de regress�o � suficiente para 
### explicar a rela��o entre longevidade e tamanho do torax para controle
### e para os dois grupos expostos a f�meas gr�vidas.

fruitfly$activity2 <- fruitfly$activity 
levels(fruitfly$activity2) <- c('iso or preg', 'iso or preg', 'low', 'iso or preg', 'high')
### O fator activity2 tem tr�s n�veis: 'iso or preg' para animais do grupo 
### controle e submetidos a f�meas gr�vidas; 'low' e 'high' para os outros
### dois grupos.

ajuste3_alt <- lm(longevity ~ thorax + activity2, data = fruitfly)
### Modelo de regress�o ajustado para o tratamento com tr�s n�veis.

### Vamos comparar os ajustes dos modelos considerando o tratamento com cinco
### e com tr�s n�veis. 
summary(ajuste3_alt)
anova(ajuste3_alt, ajuste3)
### Observe que o acr�scimo na soma de quadrados resultante da redu��o de
### cinco para tr�s n�veis do tratamento � n�o significativa (p=0.3848).
### Assim, podemos considerar o modelo aditivo com uma �nica reta de 
### regress�o para os grupos 'isolated', 'one' e 'many'.

### Vamos plotar os resultados do ajuste, como feito anteriormente.
new_thorax <- seq(min(fruitfly$thorax), max(fruitfly$thorax), length.out = 100)
trat <- c('virg1', 'iso or preg', 'virg2')
new_data <- expand.grid(new_thorax, trat)
names(new_data) <- c('thorax', 'activity2')

ggplot(aes(x = thorax, y = longevity), data = fruitfly) + geom_point() + 
    facet_grid(~activity2) + theme_bw(base_size = 18) +
    xlab('Concentra��o de substrato (ppm)') + 
    ylab('Taxa de rea��o (contagem/min^2)') +
    geom_smooth(method='lm')