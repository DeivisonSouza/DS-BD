#########################################################################
### Este primeiro exemplo da disciplina tem por objetivo motivar o uso 
### de gamlss e apresentar alguns recursos implementados no pacote gamlss.

require(gamlss) ### Carregando o pacote gamlss.
require(gamlss.demo)
options(device = 'x11')
data(rent) ### Dados sobre aluguéis de imóveis em Munique, 1980.
help(rent) ### Acessando a documentação da base de dados.

### Vamos considerar para a análise o valor do aluguel dos imóveis (R)
### além de quatro covariáveis: Fl, A, H e loc. 

########################################################################
### Parte 1 - Análise exploratória.

### Inicialmente, vamos construir alguns gráficos para explorar as relações 
### entre as variáveis.

x11()
par(mfrow = c(2,2), mar = c(4,4,2,2), las = 1)
plot(R ~ Fl, data = rent, col = 'blue', cex = 0.8)
plot(R ~ A, data = rent, col = 'blue', cex = 0.8)
plot(R ~ H, data = rent, col = 'lightblue', cex = 0.8)
plot(R ~ loc, data = rent, col = 'lightblue', cex = 0.8)

### Alguns comentários:
# O valor do aluguel parece aumentar conforme a área do imóvel. Além disso,
# observa-se também que a variação dos preços de aluguel aumenta conforme
# a área.

# Com relação ao ano de construção, não parece haver uma alteração, em termos
# de valor mediano, até o ano de 1960. A partir de então nota-se um ligeiro
# aumento.

# Imóveis com aquecimento central (H=0) apresentam maior mediana para os
# preços de aluguel em relação aos que não tem (H=1). A distribuição dos
# valores de aluguel, em ambos os grupos, apresenta assimetria (á direita).
# Além disso, a variação é maior no grupo com maior mediana.

# Finalmente, os preços de aluguel apresentam maior mediana para a região
# "acima da média", seguida pelas regiões "na média" e "abaixo da média".
# Nota-se, novamente, assimetria nas distribuições dos preços de aluguel
# em cada grupo e variância não constante.

########################################################################
########################################################################
########################################################################
### Parte 2 - Ajuste de um modelo de regressão linear.

### Todos os modelos serão ajustados usando a função gamlss da biblioteca
### gamlss. Antes de mais nada, vamos consultar sua documentação.

help(gamlss)

### Para ajustar o modelo de regressão linear, precisamos especificar a
### família normal (NO).

mod1 <- gamlss(R ~ Fl + A + H + loc, family = NO, data = rent)
coef(mod1)

### Se ajustássemos usando a função lm...
mod1_2 <- lm(R ~ Fl + A + H + loc, data = rent)
coef(mod1_2)
### os resultados seriam os mesmos.

### Vamos avaliar o resumo do ajuste e alguns gráficos para os resíduos.
### Uma observação importante: o pacote gamlss utiliza, como padrão, os
### resíduos quantílicos aleatorizados, que t~em distribuição normal 
### sob a suposição de que o modelo ajustado é correto.

summary(mod1)
### Segundo o modelo ajustado, as quatro variáveis apresentam efeito 
### significativo no preço de aluguel.

plot(mod1)

### Claramente o modelo não ajusta bem os dados. 

# No gráfico do canto superior esquerdo fica evidente que os resíduos não
# tem dispersão constante, de forma que a variância aumenta conforme a média.

# Podemos observar também, pelos quatro gráficos, que os resíduos têm 
# distribuição assimétrica.

# O gráfico quantil-quantil normal deixa nítido que os resíduos não apresentam
# distribuição normal.

fitted(mod1, what = 'sigma')[1]
### Estimativa do desvio padrão, assumido constante a todas as observações.


### Vamos plotar os resíduos versus a idade do imóvel, com o objetivo de 
### identificar alguma relação não explicada pelo modelo.

x11()
plot(rent$A, resid(mod1))
lines(lowess(resid(mod1) ~ rent$A), col = 'red', lwd = 2)

### Podemos notar que os resíduos aumentam (em média) a partir de 1960. 
### O efeito linear, considerado no ajuste do modelo, parece não ser suficiente
### para explicar a relação entre os preços de aluguel e o ano de construção
### do imóvel.

########################################################################
########################################################################
########################################################################
### Parte 3 - Ajuste de um modelo linear generalizado com resposta gamma.

### O objetivo aqui é acomodar a assimetria nos dados e variância não constante
### no ajuste do modelo.

mod2 <- gamlss(R ~ Fl + A + H + loc, family = GA, data = rent)
coef(mod2)

### Se ajustássemos usando a função glm (especificando função de ligação log)
mod2_2 <- glm(R ~ Fl + A + H + loc, family = Gamma(link = log), data = rent)
coef(mod2_2)
### os resultados seriam idênticos.

GAIC(mod1, mod2, k = 2)
### O modelo 2 (resposta gamma) proporciona melhor ajuste aos dados (menor AIC)
### que o modelo 1.

summary(mod2)
### Novamente, as quatro variáveis apresentam efeito significativo na resposta.

plot(mod2)
### Os resíduos para o modelo com resposta gamma apresentam comportamento 
### bem mais satisfatório do que para o modelo com resposta normal. Não se tem
### mais um padrão de variância não constante, e percebe-se boa aderência à
### distribuição normal. De qualquer forma, vamos avançar no ajuste e 
### avaliação de outros modelos.

fitted(mod2, what = 'sigma')[1]
### Estimativa do parâmetro de escala.

########################################################################
########################################################################
########################################################################
### Parte 4 - Ajuste de um modelo linear generalizado com resposta gamma e
### funções suaves para as variáveis numéricas (Fl e A).

mod3 <- gamlss(R ~ pb(Fl) + pb(A) + H + loc, family = GA, data = rent)
### A função pb() é a implementação do suavizador P-splines no gamlss.

GAIC(mod2, mod3, k = 2)
### O modelo com termos suaves produziu menor AIC do que o glm ajustado 
### anteriormente, produzindo, portanto, melhor ajuste.

summary(mod3)
### Os coeficientes para Fl e A referem-se apenas à parte linear dos efeitos
### dessas variáveis. Para testar suas significâncias, considerando também
### a parte não linear, podemos usar a função drop1().

drop1(mod3)
### A forma apropriada de avaliar o efeito das variáveis ao usar suavizadores
### não paramétricos é por meio de gráficos de efeitos.

term.plot(mod3, pages = 1, ask = FALSE)
### O eixo vertical representa a média na escala do preditor (log(mu)).
### Podemos observar relação praticamente linear com Fl, mas não linear
### conforme A (praticamente constante até 1960, crescente a partir de
### então).

### Vamos ajustar um modelo alternativo com suavização apenas para o ano
### de construção do imóvel.

mod3_alt <- gamlss(R ~ Fl + pb(A) + H + loc, family = GA, data = rent)
GAIC(mod2, mod3, mod3_alt, k = 2)

### O AIC ainda é menor para o modelo em que funções suavizadoras são
### incorporadas para ambas as variáveis, mas a diferença diminuiu
### consideravelmente, em relação ao modelo sem termos não paramétricos (mod2).
### As áreas sombreadas correspondem a intervalos de confiança 95%.

plot(mod3)
### Um gráfico útil para checar a dequação do ajuste é o worm plot. O worm
### plot é uma alternativa ao qqplot. O padrão, para o caso em que o modelo
### está bem ajustado, é os pontos dispersos em torno da linha horizontal,
### com 95% deles internos as linhas (bandas) tracejadas.


wp(mod3, ylim.all = 0.6)
### Podemos observar que os pontos não estão dispersos conforme o esperado,
### com acentuada curvatura na parte esquerda do gráfico.

########################################################################
########################################################################
########################################################################
### Parte 5 - Ajuste de um modelo linear generalizado com  funções suaves 
### para as variáveis numéricas (Fl e A) e inclusão de covariáveis para
### o parâmetro de dispersão.

### Vamos testar duas distribuições: gamma e normal inversa. Primeiro a 
### distribuição Gamma.
mod4 <- gamlss(R ~ pb(Fl) + pb(A) + H + loc,
               sigma.fo =~ pb(Fl) + pb(A) + H + loc, family = GA, data = rent)
coef(mod4)
term.plot(mod4, pages = 1, ask = FALSE, what = 'mu')
### Gráfico de efeitos para a média (parâmetro de locação). Percebe-se
### efeito aproximadamente linear, na escala do preditor, para Fl. Para H,
### verificamos efeito aproximadamente constante até 1960, com elevação
### partir de então.

term.plot(mod4, pages = 1, ask = FALSE, what = 'sigma')
### Gráfico de efeitos para o parâmetro de escala (dispersão). Observa-se
### efeito claramente não linear de Fl e A na dispersão. Além disso, maior
### efeito é verificado para casas com localização 1, seguida pelas localizações
### II e III.


### Agora a distribuição normal inversa.
mod5 <- gamlss(R ~ pb(Fl) + pb(A) + H + loc,
               sigma.fo =~ pb(Fl) + pb(A) + H + loc,
                family = IG, data = rent)
coef(mod5)

term.plot(mod5, pages = 1, ask = FALSE, what = 'mu')
### Gráfico de efeitos para a média (parâmetro de locação).

term.plot(mod5, pages = 1, ask = FALSE, what = 'sigma')
### Gráfico de efeitos para o parâmetro de escala (dispersão).

GAIC(mod3, mod4, mod5, k = 2)
### O modelo mod4 produziu melhor ajuste (menor valor de AIC). Mod 4 é o
### modelo com resposta gamma, função suave para Fl e A e modelagem do
### parâmetro de dispersão.

drop1(mod4, what = 'sigma')
### Todas as covariáveis, exceto H, apresentam efeito significativo no 
### parâmetro de dispersão. Vamos atualizar o modelo, excluindo o efeito 
### não significativo.

mod4_alt <- update(mod4, ~.-H, what = 'sigma')
term.plot(mod4_alt, pages = 1, ask = FALSE, what = 'sigma')

### Vamos usar o worm plot para diagnóstico do ajuste.
wp(mod4_alt, ylim.all = 0.6)
### O gráfico não reflete um ajuste satisfatório, com padrão na forma
### de U invertido.


########################################################################
########################################################################
########################################################################
### Parte 6 - Ajuste de um modelo generalizado aditivo para locação, escala
### e forma com resposta Box-Cox Cole and Green.

### A família gamlss compreende diversas distribuições, com até quatro 
### parâmetros, que permitem modelar dados com diferentes níveis de dispersão,
### assimetria e curtose. Nesta aplicação vamos considerar a distribuição
### Box-Cox Cole and Green (BCCGo).

help("dBCCGo")

### Vamos considerar a distribuição BCCGo com parâmetros mu = 1, sigma = 0.1, nu = 2.5.
dBCCGo(x = 1.5, mu = 1, sigma = 0.1, nu = 2.5) ### f(1.5)
pBCCGo(q = 1.5, mu = 1, sigma = 0.1, nu = 2.5) ### F(1.5) = P(X <= 1.5).
qBCCGo(p = 0.7, mu = 1, sigma = 0.1, nu = 2.5) ### q: F(q)  = P(X <= q) = 0.7.


### Vamos simular 100 valores aleatórios da distribuição Box-Cox Cole and Green,
### plotar o histograma dos valores simulados e sobrepor a curva da função
### densidade de probabilidade do modelo usado na simulação.

set.seed(87) ### Fixando a semente.
x <- rBCCGo(n = 100, mu = 1, sigma = 0.1, nu = 2.5) ### Gerando 100 valores aleatórios.
hist(x, probability = TRUE, ylim = c(0,4.5))
curve(dBCCGo(x, mu = 1, sigma = 0.1, nu = 2.5), from = 0.6, to = 1.3, 
      add = TRUE, col = 'red', lwd = 2)

gamlss.demo() ### Alguma demo.

### Voltermos à análise dos dados dos preços de aluguel de imóveis. Vamos
### ajustar dois modelos: o primeiro (mod6) não incluindo covariáveis na
### modelagem do prâmetro de forma (nu). No segundo (mod7) as mesmas covariáveis
### usadas para modelar mu são usadas também para nu.

mod6 <- gamlss(R ~ pb(Fl) + pb(A) + H + loc,
               sigma.fo =~ pb(Fl) + pb(A) + loc, family = BCCGo, data = rent)

mod7 <- gamlss(R ~ pb(Fl) + pb(A) + H + loc,
               sigma.fo =~ pb(Fl) + pb(A) + loc, 
               nu.fo =~ pb(Fl) + pb(A) + H + loc, family = BCCGo, data = rent)

### Vamos comparar os ajustes usando o AIC.
GAIC(mod4, mod6, mod7, k = 2)

### O modelo mod7 produziu menor valor de AIC, indicando, portanto, melhor 
### ajuste. Nesse caso, além de locação e escala, também a forma da distribuição 
### varia conforme os valores das covariáveis. 

### Vamos avaliar possível retirada dos termos no preditor do parâmetro de
### forma em mod7.

drop1(mod7, what = 'nu') ### Leva tempo...
### para o parâmetro de forma, apenas H resultou em efeito significativo.
### Vamos ajustar novo modelo, extraindo os demais efeitos do preditor.

mod7_alt <- update(mod7, ~H, what = 'nu')
GAIC(mod7, mod7_alt, k = 2)
### O modelo que contém apenas o efeito de H em nu produziu ajuste com menor
### AIC, e é preferível

### Vamos avaliar os gráficos de efeitos.
term.plot(mod7_alt, pages = 1, what = 'mu', ask = FALSE)
term.plot(mod7_alt, pages = 1, what = 'sigma', ask = FALSE)
term.plot(mod7_alt, pages = 1, what = 'nu', ask = FALSE)

### Vamos proceder com o diagnóstico do ajuste.
plot(mod7_alt)

wp(mod7_alt, ylim.all = 0.6, main = 'mod7_alt')
### O modelo, aparentemente, se ajusta bem aos dados.

########################################################################
### Neste ponto, vamos explorar o modelo ajustado, estimativas e predições.
### Vamos considerar o último modelo ajustado, mod7_alt.

### Vamos estimar os parâmetros para as distribuições dos preços de aluguel
### em dois cenários.

data_new <- data.frame(Fl = c(52, 82), A = c(1940, 1975), H = c('1', '0'), loc = c('1', '3'))
rownames(data_new) <- c('Imovel 1', 'Imovel 2')
### data_new armazena características de dois "perfis" distintos de imóveis. 

### Primeiramente, vamos estimar o preço médio de aluguel. 
pmu <- predict(mod7_alt, newdata = data_new, what = c('mu'))
pmu
### Estimativa na escala do preditor (log(mu)).

pmu <- predict(mod7_alt, newdata = data_new, what = c('mu'), type = 'response')
pmu
### Estimativa na escala do parâmetro (mu).

### Agora, de maneira semelhante, para os demais parâmetros do modelo.
### Para o parâmetro de escala (sigma):
psigma <- predict(mod7_alt, newdata = data_new, what = c('sigma'))
psigma
### Estimativa na escala do preditor (log(sigma)).

psigma <- predict(mod7_alt, newdata = data_new, what = c('sigma'), type = 'response')
psigma
### Estimativa na escala do parâmetro (sigma).

### Para o parâmetro de forma (nu):
pnu <- predict(mod7_alt, newdata = data_new, what = c('nu'))
pnu
### Como a função de ligação é do tipo identidade, a estimativa é a mesma
### nas escalas do preditor e do parâmetro.

### Vamos plotar as curvas das densidades ajustadas para os dois perfis
### de imóveis.
curve(dBCCGo(x, mu = pmu[1], sigma = psigma[1], nu = pnu[1]), 
      from = 0, to = 3500, xlab = 'Aluguel', ylab = 'Densidade', col = 'red', lwd = 2)
curve(dBCCGo(x, mu = pmu[2], sigma = psigma[2], nu = pnu[2]), 
      from = 0, to = 3500, col = 'blue', lwd = 2, add = TRUE)
legend(x = 'topright', legend = c('Imovel 1', 'Imovel 2'), col = c('red', 'blue'), lwd = 2)

### Probabilidade de um imóvel com valor de aluguel inferior a $500 para
### cada um dos dois perfis.
pBCCGo(500, mu = pmu[1], sigma = psigma[1], nu = pnu[1]) ### Perfil 1
pBCCGo(500, mu = pmu[2], sigma = psigma[2], nu = pnu[2]) ### Perfil 2

### Probabilidade de um imóvel com valor de aluguel superior a $1000 para
### cada um dos dois perfis.
pBCCGo(1000, mu = pmu[1], sigma = psigma[1], nu = pnu[1], lower.tail = FALSE) 
### Perfil 1
pBCCGo(1000, mu = pmu[2], sigma = psigma[2], nu = pnu[2], lower.tail = FALSE) 
### Perfil 2

### Estimativas dos preços medianos
qBCCGo(0.5, mu = pmu[1], sigma = psigma[1], nu = pnu[1]) 
### Perfil 1
qBCCGo(0.5, mu = pmu[2], sigma = psigma[2], nu = pnu[2]) 
### Perfil 2

### Estimativas dos quantis 75 e 90%
qBCCGo(c(0.75,0.90), mu = pmu[1], sigma = psigma[1], nu = pnu[1]) 
### Perfil 1
qBCCGo(c(0.75,0.90), mu = pmu[2], sigma = psigma[2], nu = pnu[2]) 
### Perfil 2

