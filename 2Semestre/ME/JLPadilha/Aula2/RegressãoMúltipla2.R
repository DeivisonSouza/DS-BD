### Exemplo - Regress�o linear m�ltipla aplicada � modelagem do pre�o de venda de
### im�veis. As vari�veis s�o as seguintes:

### area: �rea do im�vel (m^2);
### idade: Idade do im�vel (anos);
### distancia: Dist�ncia do im�vel ao marco central do munic�pio (em km);
### ncomodos: N�mero de c�modos;
### pcomerc: N�mero de pontos comerciais num raio de um quil�metro;
### valor: Pre�o de venda do im�vel (vari�vel resposta, em milhares de d�lares).

dados <- read.csv2('https://docs.ufpr.br/~taconeli/CE07118/Imoveis18.csv') ### Importando os dados.
# dados <- read.csv2('Imoveis18.csv')
head(dados,10)

summary(dados) 

require(car)
require(corrplot)
require(effects)
require(ggplot2)
par(cex=1.2)
scatterplotMatrix(dados,smooth=F) 
### Matriz de gr�ficos de dispers�o. Os resultados fornecem um primeiro indicativo 
### de correla��o positiva entre o pre�o de venda e a �rea e n�mero de c�modos do
### im�vel. Tamb�m h� alguma evid�ncia de correla��o negativa em rela��o � idade
### do im�vel.

x11()
corrplot.mixed(cor(dados), upper = "ellipse")

### Vamos ajustar o modelo de regress�o linear m�ltipla:
ajuste1 <- lm(valor ~ area + idade + distancia + ncomodos + pcomerc, data = dados)

### O mesmo modelo pode ser ajustado, de forma mais breve, atrav�s do seguinte 
### comando:
ajuste1 <- lm(valor ~ ., data = dados)
model.matrix(ajuste1)[1:10,]
### Dez primeiras linhas da matriz do modelo.

### Vamos extrair os principais resultados da an�lise.

print(ajuste1) 
### Estimativas de m�nimos quadrados dos par�metros de regress�o.

summary(ajuste1)
### Os resultados do ajuste indicam que o valor de venda est� associado � �rea,
### n�mero de c�modos e idade do im�vel.

### Estima-se, em m�dia, um aumento de 1496 d�lares para cada m^2 a mais de �rea;
### uma redu��o de 713 d�lares a cada ano a mais de idade e redu��o de 917 d�lares
### a cada c�modo a mais. Em cada uma dessas afirmativas estamos considerando fixos
### os valores das demais vari�veis explicativas.

### Vamos ajustar modelos de regress�o linear simples para cada vari�vel
### explicativa e comparar os resultados.

ajuste_area <- lm(valor ~ area, data = dados)
summary(ajuste_area)

ajuste_idade <- lm(valor ~ idade, data = dados)
summary(ajuste_idade)

ajuste_distancia <- lm(valor ~ distancia, data = dados)
summary(ajuste_distancia)

ajuste_ncomodos <- lm(valor ~ ncomodos, data = dados)
summary(ajuste_ncomodos)

ajuste_pcomerc <- lm(valor ~ pcomerc, data = dados)
summary(ajuste_pcomerc)

### Observe o efeito do n�mero de c�modos no valor de venda do im�vel no 
### modelo de regress�o linear m�ltipla e no modelo de regress�o linear
### simples. O que ocorre? Voc� tem uma justificativa? 

fitted(ajuste1) ### Valores ajustados pelo modelo de regress�o linear m�ltipla.
residuals(ajuste1) ### Res�duos.

summary(ajuste1)$sigma^2 ### Estimativa de sigma^2.
summary(ajuste1)$r.squared ### Coeficiente de determina��o.
### Aproximadamente 60% da varia��o dos dados � explicada pelo modelo.

summary(ajuste1)$adj.r.squared
### Coeficiente de determina��o ajustado.

plot(fitted(ajuste1), dados$valor, pch = 20,xlab = 'Vendas ajustadas', 
     ylab = 'Vendas observadas', las = 1) 
### Gr�fico de valores observados vs valores ajustados.

confint(ajuste1) 
### Intervalos de confian�a (95%) para os par�metros do modelo.

### Agora vamos fazer algumas predi��es.

dnovos <- data.frame(area = c(80, 100, 150), idade = c(5, 10, 10), 
                     distancia = c(15, 12, 10), ncomodos = c(5, 6, 8),
                     pcomerc = c(10, 10, 10)) 
dnovos

predict(ajuste1, newdata = dnovos) 
### Estimativas pontuais para os pre�os de venda para os novos dados. 

predict(ajuste1, interval = 'confidence', newdata = dnovos)
### Intervalos de confian�a (95%) para os pre�os m�dios de venda de im�veis com 
### os tr�s "perfis".

predict(ajuste1, interval = 'prediction', newdata = dnovos) 
### Intervalos de predi��o (95%) para o valor de venda de tr�s novos im�veis com
### as caracter�sticas especificadas.

p1 <- predict(ajuste1, interval = 'confidence', newdata = dados)
### Intervalos de confian�a para os pre�os m�dios de venda de im�veis com os perfis
### dos im�veis da base.
cbind(dados[1:20,1:5], p1[1:20,])

p2 <- predict(ajuste1, interval = 'prediction', newdata = dados)
### Intervalos de predi��o para os pre�os de venda de novos im�veis com os perfis
### dos im�veis da base.
cbind(dados[1:20,1:5], p2[1:20,])

### Agora, alguns testes de hip�teses. Come�amos pelo quadro da anova.
anova(ajuste1)

### O que podemos concluir:

### Efeito significativo da inclus�o de area (conforme a redu��o na soma de 
### quadrados de res�duos) ao modelo nulo (p < 0.0001);

### Efeito significativo da inclus�o de idade (conforme a redu��o na soma de 
### quadrados de res�duos) ao modelo ajustado pelo efeito de area (p = 0.0055);

### Efeito n�o significativo da inclus�o de distancia (conforme a redu��o na soma de 
### quadrados de res�duos) ao modelo ajustado pelos efeitos de area e idade
### (p = 0.6358);

### Efeito significativo da inclus�o de ncomodos (conforme a redu��o na soma de 
### quadrados de res�duos) ao modelo ajustado pelos efeitos de area, idade e distancia
### (p = 0.0444);

### Efeito n�o significativo da inclus�o de pcomerc (conforme a redu��o na soma de 
### quadrados de res�duos) ao modelo ajustado pelos efeitos de area, idade, distancia
### e ncomodos (p = 0.5856).

### E se mudarmos a ordem de declara��o das vari�veis?

ajuste1_alt <- lm(valor ~ pcomerc + ncomodos + area + idade + distancia , data = dados)
anova(ajuste1_alt)

### Os resultados s�o id�nticos? Justifique.


### Usando a fun��o Anova do pacote car, temos uma sequ�ncia de testes diferentes:

Anova(ajuste1)

### O que podemos concluir (ao n�vel de 5% de signific�ncia):

### Efeito significativo da inclus�o de area (conforme a redu��o na soma de 
### quadrados de res�duos) ao modelo ajustado por todas as demais vari�veis (p < 0.0001);

### Efeito significativo da inclus�o de idade (conforme a redu��o na soma de 
### quadrados de res�duos) ao modelo ajustado por todas as demais vari�veis (p = 0.0042);

### Efeito n�o significativo da inclus�o de distancia (conforme a redu��o na soma de 
### quadrados de res�duos) ao modelo ajustado por todas as demais vari�veis (p = 0.64817);

### Efeito significativo da inclus�o de ncomodos (conforme a redu��o na soma de 
### quadrados de res�duos) ao modelo ajustado por todas as demais vari�veis (p = 0.0439);

### Efeito n�o significativo da inclus�o de pcomerc (conforme a redu��o na soma de 
### quadrados de res�duos) ao modelo ajustado por todas as demais vari�veis (p = 0.5856).

### E se mudarmos a ordem de declara��o das vari�veis?

ajuste1_alt <- lm(valor ~ pcomerc + ncomodos + area + idade + distancia , data = dados)
Anova(ajuste1_alt)

### Os resultados s�o id�nticos? Justifique.

### Observe que, no segundo caso os testes F s�o equivalentes ao teste t apresentados 
### no summary. Isso n�o � coincid�ncia, e sempre ocorrer� quando houver apenas 
### um par�metro do modelo sob teste.

### Vamos testar o efeito parcial da idade do im�vel. Esse teste j� est� apresentado
### na sa�da da fun��o Anova, e � equivalente ao teste apresentado no summary.
### Mas usando essa forma de proceder o teste teremos condi��es de realizar testes
### envolvendo, conjuntamente, dois ou mais par�metros.

ajuste2 <- lm(valor ~. -idade, data = dados) 
### Modelo de regress�o linear m�ltipla sem considerar a covari�vel idade.

anova(ajuste2, ajuste1)
### O acr�scimo na soma de quadrados (ASQ) de res�duos, resultante da elimina��o do 
### termo idade do modelo, � igual a 4392. A estat�stica do teste � dada por
### F = (ASQ/df)/QMRes = 4392/531 = 8.26. Sob a hip�tese nula, a estat�stica F
### tem distribui��o F-Snedecor com par�metros 1 (associado ao n�mero de par�metros)
### fixados pela hip�tese nula) e 494 (associado � soma de quadrados de res�duos).
### Vamos ver quais seriam as conclus�es aos n�veis de 5 e 1% de signific�ncia:

qf(0.95, df1 = 1, df2 = 494)
### Como F = 8.26 > 3.86, podemos rejeitar a hip�tese nula e comprovar o efeito
### da idade do im�vel ao n�vel de signific�ncia de 5%;

qf(0.99, df1 = 1, df2 = 494)
### Como F = 8.26 > 6.68, podemos rejeitar a hip�tese nula e comprovar o efeito
### da idade do im�vel ao n�vel de signific�ncia de 5%;

pf(8.26, df1 = 1, df2 = 494, lower.tail = FALSE)
### Valor p do teste (ligeiramente diferente por erros de arredondamento).

### Agora vamos testar a hip�tese nula H_0: beta_distancia = beta_pcomerc = 0.
ajuste3 <- lm(valor ~. -distancia - pcomerc, data = dados)
anova(ajuste3, ajuste1)
summary(ajuste3)

### N�o h� evid�ncia significativa contr�ria � hip�tese nula (p = 0.7558). Assim,
### a partir deste ponto vamos considerar o modelo apenas com efeito de area, idade
### e ncomodos.



### Explorando os efeitos das vari�veis.
plot(allEffects(ajuste3))
### Gr�ficos de efeitos para cada vari�vel. No eixo vertical temos a resposta esperada
### (pre�o m�dio de venda). Em cada gr�fico as demais vari�veis s�o fixadas em suas
### m�dias. Al�m da estimativa pontual (representada pela reta s�lida), tamb�m s�o
### apresentadas as bandas de confian�a (95%).



### Vamos ajustar um novo modelo, agora considerando as vari�veis explicativas
### centradas, isso �, subtraindo de cada vari�vel a respectiva m�dia na
### amostra.

dados$areac <- dados$area - mean(dados$area)
dados$ncomodosc <- dados$ncomodos - mean(dados$ncomodos)
dados$idadec <- dados$idade - mean(dados$idade)

ajuste3c <- lm(valor ~ areac + ncomodosc + idadec, data = dados)
summary(ajuste3c)
summary(ajuste3)

### Observe que centrar os dados n�o tem qualquer efeito sobre as estimativas,
### erros padr�es e signific�ncias dos betas. Isso se repetiria para qualquer
### outra constante que fosse somada ou adicionada aos valores de cada vari�vel.
### Adicionalmente:

### 1- O intercepto agora tem uma interpreta��o v�lida. Podemos interpret�-lo
### como a estimativa do pre�o m�dio de venda para im�veis com caracter�sticas
### "m�dias", ou seja, com valores para as covari�veis definidas pelas m�dias
### amostrais. Como o valor ajustado de y avaliado nas m�dias de x1, x2, ...,
### xp � a m�dia amostral de y, ent�o a estimativa do intercepto ser�
### simplesmente ybarra.

### 2- O intercepto, agora, � ortogonal aos demais par�metros do modelo.
### Para verificar isso, nest aplica��o, basta extrair as matrizes de covari�ncias
### dos estimadores produzidas pelos dois modelos.

vcov(ajuste3)
vcov(ajuste3c)

### Agora vamos ajustar um novo modelo em que, al�m de centradas, as vari�veis
### explicativas est�o escalonadas. Ou seja, para uma vari�vel x qualquer,
### consideramos x' = (x - media(x))/desv.padr�o(x).

dados$areas <- (dados$area - mean(dados$area))/sd(dados$area)
dados$ncomodoss <- (dados$ncomodos - mean(dados$ncomodos))/sd(dados$ncomodos)
dados$idades <- (dados$idade - mean(dados$idade))/sd(dados$idade)

ajuste3s <- lm(valor ~ areas + idades + ncomodoss, data = dados)
summary(ajuste3s)
summary(ajuste3c)
summary(ajuste3)

### Nota: a fun��o scale transforma as vari�veis de maneira mais simples.

### Observe que agora, embora as signific�ncias das vari�veis sejam as
### mesmas, as estimativas e os erros padr�es mudaram, devido � mudan�a
### de escala. Neste modelo as interpreta��es dos par�metros n�o s�o
### mais em rela��o ao "acr�scimo de uma unidade em x", mas sim quanto ao
### "acr�scimo de um desvio padr�o de x em x".

### Como resultado de eliminar o efeito de escala, os valores das estimativas
### (suas grandezas) s�o diretamente compar�veis. Assim, fica evidente o
### maior efeito da �rea do im�vel no pre�o de venda. Nos modelos anteriores
### os valores das estimativas dependiam da escala (e da unidade de medida)
### das respectivas vari�veis. Aqui isso j� n�o acontece.

### A interpreta��o do intercepto � a mesma do ajuste anterior.

estim <- data.frame(coef(ajuste3s), confint(ajuste3s))[-1,]
### Data frame com as estimativas pontuais e intervalos de confian�a 95%
### para os betas (exceto o intercepto).

names(estim) <- c('Estimativa', 'LI', 'LS')

p <- ggplot(aes(y = Estimativa, ymin = LI, ymax = LS, x = rownames(estim)),
            data = estim) + geom_pointrange()

p + coord_flip() + xlab("Vari�vel") + geom_hline(yintercept = 0)
