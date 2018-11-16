### Exemplo - Regressão linear múltipla aplicada à modelagem do preço de venda de
### imóveis. As variáveis são as seguintes:

### area: Área do imóvel (m^2);
### idade: Idade do imóvel (anos);
### distancia: Distância do imóvel ao marco central do município (em km);
### ncomodos: Número de cômodos;
### pcomerc: Número de pontos comerciais num raio de um quilômetro;
### valor: Preço de venda do imóvel (variável resposta, em milhares de dólares).

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
### Matriz de gráficos de dispersão. Os resultados fornecem um primeiro indicativo 
### de correlação positiva entre o preço de venda e a área e número de cômodos do
### imóvel. Também há alguma evidência de correlação negativa em relação à idade
### do imóvel.

x11()
corrplot.mixed(cor(dados), upper = "ellipse")

### Vamos ajustar o modelo de regressão linear múltipla:
ajuste1 <- lm(valor ~ area + idade + distancia + ncomodos + pcomerc, data = dados)

### O mesmo modelo pode ser ajustado, de forma mais breve, através do seguinte 
### comando:
ajuste1 <- lm(valor ~ ., data = dados)
model.matrix(ajuste1)[1:10,]
### Dez primeiras linhas da matriz do modelo.

### Vamos extrair os principais resultados da análise.

print(ajuste1) 
### Estimativas de mínimos quadrados dos parâmetros de regressão.

summary(ajuste1)
### Os resultados do ajuste indicam que o valor de venda está associado à área,
### número de cômodos e idade do imóvel.

### Estima-se, em média, um aumento de 1496 dólares para cada m^2 a mais de área;
### uma redução de 713 dólares a cada ano a mais de idade e redução de 917 dólares
### a cada cômodo a mais. Em cada uma dessas afirmativas estamos considerando fixos
### os valores das demais variáveis explicativas.

### Vamos ajustar modelos de regressão linear simples para cada variável
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

### Observe o efeito do número de cômodos no valor de venda do imóvel no 
### modelo de regressão linear múltipla e no modelo de regressão linear
### simples. O que ocorre? Você tem uma justificativa? 

fitted(ajuste1) ### Valores ajustados pelo modelo de regressão linear múltipla.
residuals(ajuste1) ### Resíduos.

summary(ajuste1)$sigma^2 ### Estimativa de sigma^2.
summary(ajuste1)$r.squared ### Coeficiente de determinação.
### Aproximadamente 60% da variação dos dados é explicada pelo modelo.

summary(ajuste1)$adj.r.squared
### Coeficiente de determinação ajustado.

plot(fitted(ajuste1), dados$valor, pch = 20,xlab = 'Vendas ajustadas', 
     ylab = 'Vendas observadas', las = 1) 
### Gráfico de valores observados vs valores ajustados.

confint(ajuste1) 
### Intervalos de confiança (95%) para os parâmetros do modelo.

### Agora vamos fazer algumas predições.

dnovos <- data.frame(area = c(80, 100, 150), idade = c(5, 10, 10), 
                     distancia = c(15, 12, 10), ncomodos = c(5, 6, 8),
                     pcomerc = c(10, 10, 10)) 
dnovos

predict(ajuste1, newdata = dnovos) 
### Estimativas pontuais para os preços de venda para os novos dados. 

predict(ajuste1, interval = 'confidence', newdata = dnovos)
### Intervalos de confiança (95%) para os preços médios de venda de imóveis com 
### os três "perfis".

predict(ajuste1, interval = 'prediction', newdata = dnovos) 
### Intervalos de predição (95%) para o valor de venda de três novos imóveis com
### as características especificadas.

p1 <- predict(ajuste1, interval = 'confidence', newdata = dados)
### Intervalos de confiança para os preços médios de venda de imóveis com os perfis
### dos imóveis da base.
cbind(dados[1:20,1:5], p1[1:20,])

p2 <- predict(ajuste1, interval = 'prediction', newdata = dados)
### Intervalos de predição para os preços de venda de novos imóveis com os perfis
### dos imóveis da base.
cbind(dados[1:20,1:5], p2[1:20,])

### Agora, alguns testes de hipóteses. Começamos pelo quadro da anova.
anova(ajuste1)

### O que podemos concluir:

### Efeito significativo da inclusão de area (conforme a redução na soma de 
### quadrados de resíduos) ao modelo nulo (p < 0.0001);

### Efeito significativo da inclusão de idade (conforme a redução na soma de 
### quadrados de resíduos) ao modelo ajustado pelo efeito de area (p = 0.0055);

### Efeito não significativo da inclusão de distancia (conforme a redução na soma de 
### quadrados de resíduos) ao modelo ajustado pelos efeitos de area e idade
### (p = 0.6358);

### Efeito significativo da inclusão de ncomodos (conforme a redução na soma de 
### quadrados de resíduos) ao modelo ajustado pelos efeitos de area, idade e distancia
### (p = 0.0444);

### Efeito não significativo da inclusão de pcomerc (conforme a redução na soma de 
### quadrados de resíduos) ao modelo ajustado pelos efeitos de area, idade, distancia
### e ncomodos (p = 0.5856).

### E se mudarmos a ordem de declaração das variáveis?

ajuste1_alt <- lm(valor ~ pcomerc + ncomodos + area + idade + distancia , data = dados)
anova(ajuste1_alt)

### Os resultados são idênticos? Justifique.


### Usando a função Anova do pacote car, temos uma sequência de testes diferentes:

Anova(ajuste1)

### O que podemos concluir (ao nível de 5% de significância):

### Efeito significativo da inclusão de area (conforme a redução na soma de 
### quadrados de resíduos) ao modelo ajustado por todas as demais variáveis (p < 0.0001);

### Efeito significativo da inclusão de idade (conforme a redução na soma de 
### quadrados de resíduos) ao modelo ajustado por todas as demais variáveis (p = 0.0042);

### Efeito não significativo da inclusão de distancia (conforme a redução na soma de 
### quadrados de resíduos) ao modelo ajustado por todas as demais variáveis (p = 0.64817);

### Efeito significativo da inclusão de ncomodos (conforme a redução na soma de 
### quadrados de resíduos) ao modelo ajustado por todas as demais variáveis (p = 0.0439);

### Efeito não significativo da inclusão de pcomerc (conforme a redução na soma de 
### quadrados de resíduos) ao modelo ajustado por todas as demais variáveis (p = 0.5856).

### E se mudarmos a ordem de declaração das variáveis?

ajuste1_alt <- lm(valor ~ pcomerc + ncomodos + area + idade + distancia , data = dados)
Anova(ajuste1_alt)

### Os resultados são idênticos? Justifique.

### Observe que, no segundo caso os testes F são equivalentes ao teste t apresentados 
### no summary. Isso não é coincidência, e sempre ocorrerá quando houver apenas 
### um parâmetro do modelo sob teste.

### Vamos testar o efeito parcial da idade do imóvel. Esse teste já está apresentado
### na saída da função Anova, e é equivalente ao teste apresentado no summary.
### Mas usando essa forma de proceder o teste teremos condições de realizar testes
### envolvendo, conjuntamente, dois ou mais parâmetros.

ajuste2 <- lm(valor ~. -idade, data = dados) 
### Modelo de regressão linear múltipla sem considerar a covariável idade.

anova(ajuste2, ajuste1)
### O acréscimo na soma de quadrados (ASQ) de resíduos, resultante da eliminação do 
### termo idade do modelo, é igual a 4392. A estatística do teste é dada por
### F = (ASQ/df)/QMRes = 4392/531 = 8.26. Sob a hipótese nula, a estatística F
### tem distribuição F-Snedecor com parâmetros 1 (associado ao número de parâmetros)
### fixados pela hipótese nula) e 494 (associado à soma de quadrados de resíduos).
### Vamos ver quais seriam as conclusões aos níveis de 5 e 1% de significância:

qf(0.95, df1 = 1, df2 = 494)
### Como F = 8.26 > 3.86, podemos rejeitar a hipótese nula e comprovar o efeito
### da idade do imóvel ao nível de significância de 5%;

qf(0.99, df1 = 1, df2 = 494)
### Como F = 8.26 > 6.68, podemos rejeitar a hipótese nula e comprovar o efeito
### da idade do imóvel ao nível de significância de 5%;

pf(8.26, df1 = 1, df2 = 494, lower.tail = FALSE)
### Valor p do teste (ligeiramente diferente por erros de arredondamento).

### Agora vamos testar a hipótese nula H_0: beta_distancia = beta_pcomerc = 0.
ajuste3 <- lm(valor ~. -distancia - pcomerc, data = dados)
anova(ajuste3, ajuste1)
summary(ajuste3)

### Não há evidência significativa contrária à hipótese nula (p = 0.7558). Assim,
### a partir deste ponto vamos considerar o modelo apenas com efeito de area, idade
### e ncomodos.



### Explorando os efeitos das variáveis.
plot(allEffects(ajuste3))
### Gráficos de efeitos para cada variável. No eixo vertical temos a resposta esperada
### (preço médio de venda). Em cada gráfico as demais variáveis são fixadas em suas
### médias. Além da estimativa pontual (representada pela reta sólida), também são
### apresentadas as bandas de confiança (95%).



### Vamos ajustar um novo modelo, agora considerando as variáveis explicativas
### centradas, isso é, subtraindo de cada variável a respectiva média na
### amostra.

dados$areac <- dados$area - mean(dados$area)
dados$ncomodosc <- dados$ncomodos - mean(dados$ncomodos)
dados$idadec <- dados$idade - mean(dados$idade)

ajuste3c <- lm(valor ~ areac + ncomodosc + idadec, data = dados)
summary(ajuste3c)
summary(ajuste3)

### Observe que centrar os dados não tem qualquer efeito sobre as estimativas,
### erros padrões e significâncias dos betas. Isso se repetiria para qualquer
### outra constante que fosse somada ou adicionada aos valores de cada variável.
### Adicionalmente:

### 1- O intercepto agora tem uma interpretação válida. Podemos interpretá-lo
### como a estimativa do preço médio de venda para imóveis com características
### "médias", ou seja, com valores para as covariáveis definidas pelas médias
### amostrais. Como o valor ajustado de y avaliado nas médias de x1, x2, ...,
### xp é a média amostral de y, então a estimativa do intercepto será
### simplesmente ybarra.

### 2- O intercepto, agora, é ortogonal aos demais parâmetros do modelo.
### Para verificar isso, nest aplicação, basta extrair as matrizes de covariâncias
### dos estimadores produzidas pelos dois modelos.

vcov(ajuste3)
vcov(ajuste3c)

### Agora vamos ajustar um novo modelo em que, além de centradas, as variáveis
### explicativas estão escalonadas. Ou seja, para uma variável x qualquer,
### consideramos x' = (x - media(x))/desv.padrão(x).

dados$areas <- (dados$area - mean(dados$area))/sd(dados$area)
dados$ncomodoss <- (dados$ncomodos - mean(dados$ncomodos))/sd(dados$ncomodos)
dados$idades <- (dados$idade - mean(dados$idade))/sd(dados$idade)

ajuste3s <- lm(valor ~ areas + idades + ncomodoss, data = dados)
summary(ajuste3s)
summary(ajuste3c)
summary(ajuste3)

### Nota: a função scale transforma as variáveis de maneira mais simples.

### Observe que agora, embora as significâncias das variáveis sejam as
### mesmas, as estimativas e os erros padrões mudaram, devido à mudança
### de escala. Neste modelo as interpretações dos parâmetros não são
### mais em relação ao "acréscimo de uma unidade em x", mas sim quanto ao
### "acréscimo de um desvio padrão de x em x".

### Como resultado de eliminar o efeito de escala, os valores das estimativas
### (suas grandezas) são diretamente comparáveis. Assim, fica evidente o
### maior efeito da área do imóvel no preço de venda. Nos modelos anteriores
### os valores das estimativas dependiam da escala (e da unidade de medida)
### das respectivas variáveis. Aqui isso já não acontece.

### A interpretação do intercepto é a mesma do ajuste anterior.

estim <- data.frame(coef(ajuste3s), confint(ajuste3s))[-1,]
### Data frame com as estimativas pontuais e intervalos de confiança 95%
### para os betas (exceto o intercepto).

names(estim) <- c('Estimativa', 'LI', 'LS')

p <- ggplot(aes(y = Estimativa, ymin = LI, ymax = LS, x = rownames(estim)),
            data = estim) + geom_pointrange()

p + coord_flip() + xlab("Variável") + geom_hline(yintercept = 0)
