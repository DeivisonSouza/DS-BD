#########################################################################
### Este primeiro exemplo da disciplina tem por objetivo motivar o uso 
### de gamlss e apresentar alguns recursos implementados no pacote gamlss.

require(gamlss) ### Carregando o pacote gamlss.
require(gamlss.demo)
options(device = 'x11')
data(rent) ### Dados sobre alugu�is de im�veis em Munique, 1980.
help(rent) ### Acessando a documenta��o da base de dados.

### Vamos considerar para a an�lise o valor do aluguel dos im�veis (R)
### al�m de quatro covari�veis: Fl, A, H e loc. 

########################################################################
### Parte 1 - An�lise explorat�ria.

### Inicialmente, vamos construir alguns gr�ficos para explorar as rela��es 
### entre as vari�veis.

x11()
par(mfrow = c(2,2), mar = c(4,4,2,2), las = 1)
plot(R ~ Fl, data = rent, col = 'blue', cex = 0.8)
plot(R ~ A, data = rent, col = 'blue', cex = 0.8)
plot(R ~ H, data = rent, col = 'lightblue', cex = 0.8)
plot(R ~ loc, data = rent, col = 'lightblue', cex = 0.8)

### Alguns coment�rios:
# O valor do aluguel parece aumentar conforme a �rea do im�vel. Al�m disso,
# observa-se tamb�m que a varia��o dos pre�os de aluguel aumenta conforme
# a �rea.

# Com rela��o ao ano de constru��o, n�o parece haver uma altera��o, em termos
# de valor mediano, at� o ano de 1960. A partir de ent�o nota-se um ligeiro
# aumento.

# Im�veis com aquecimento central (H=0) apresentam maior mediana para os
# pre�os de aluguel em rela��o aos que n�o tem (H=1). A distribui��o dos
# valores de aluguel, em ambos os grupos, apresenta assimetria (� direita).
# Al�m disso, a varia��o � maior no grupo com maior mediana.

# Finalmente, os pre�os de aluguel apresentam maior mediana para a regi�o
# "acima da m�dia", seguida pelas regi�es "na m�dia" e "abaixo da m�dia".
# Nota-se, novamente, assimetria nas distribui��es dos pre�os de aluguel
# em cada grupo e vari�ncia n�o constante.

########################################################################
########################################################################
########################################################################
### Parte 2 - Ajuste de um modelo de regress�o linear.

### Todos os modelos ser�o ajustados usando a fun��o gamlss da biblioteca
### gamlss. Antes de mais nada, vamos consultar sua documenta��o.

help(gamlss)

### Para ajustar o modelo de regress�o linear, precisamos especificar a
### fam�lia normal (NO).

mod1 <- gamlss(R ~ Fl + A + H + loc, family = NO, data = rent)
coef(mod1)

### Se ajust�ssemos usando a fun��o lm...
mod1_2 <- lm(R ~ Fl + A + H + loc, data = rent)
coef(mod1_2)
### os resultados seriam os mesmos.

### Vamos avaliar o resumo do ajuste e alguns gr�ficos para os res�duos.
### Uma observa��o importante: o pacote gamlss utiliza, como padr�o, os
### res�duos quant�licos aleatorizados, que t~em distribui��o normal 
### sob a suposi��o de que o modelo ajustado � correto.

summary(mod1)
### Segundo o modelo ajustado, as quatro vari�veis apresentam efeito 
### significativo no pre�o de aluguel.

plot(mod1)

### Claramente o modelo n�o ajusta bem os dados. 

# No gr�fico do canto superior esquerdo fica evidente que os res�duos n�o
# tem dispers�o constante, de forma que a vari�ncia aumenta conforme a m�dia.

# Podemos observar tamb�m, pelos quatro gr�ficos, que os res�duos t�m 
# distribui��o assim�trica.

# O gr�fico quantil-quantil normal deixa n�tido que os res�duos n�o apresentam
# distribui��o normal.

fitted(mod1, what = 'sigma')[1]
### Estimativa do desvio padr�o, assumido constante a todas as observa��es.


### Vamos plotar os res�duos versus a idade do im�vel, com o objetivo de 
### identificar alguma rela��o n�o explicada pelo modelo.

x11()
plot(rent$A, resid(mod1))
lines(lowess(resid(mod1) ~ rent$A), col = 'red', lwd = 2)

### Podemos notar que os res�duos aumentam (em m�dia) a partir de 1960. 
### O efeito linear, considerado no ajuste do modelo, parece n�o ser suficiente
### para explicar a rela��o entre os pre�os de aluguel e o ano de constru��o
### do im�vel.

########################################################################
########################################################################
########################################################################
### Parte 3 - Ajuste de um modelo linear generalizado com resposta gamma.

### O objetivo aqui � acomodar a assimetria nos dados e vari�ncia n�o constante
### no ajuste do modelo.

mod2 <- gamlss(R ~ Fl + A + H + loc, family = GA, data = rent)
coef(mod2)

### Se ajust�ssemos usando a fun��o glm (especificando fun��o de liga��o log)
mod2_2 <- glm(R ~ Fl + A + H + loc, family = Gamma(link = log), data = rent)
coef(mod2_2)
### os resultados seriam id�nticos.

GAIC(mod1, mod2, k = 2)
### O modelo 2 (resposta gamma) proporciona melhor ajuste aos dados (menor AIC)
### que o modelo 1.

summary(mod2)
### Novamente, as quatro vari�veis apresentam efeito significativo na resposta.

plot(mod2)
### Os res�duos para o modelo com resposta gamma apresentam comportamento 
### bem mais satisfat�rio do que para o modelo com resposta normal. N�o se tem
### mais um padr�o de vari�ncia n�o constante, e percebe-se boa ader�ncia �
### distribui��o normal. De qualquer forma, vamos avan�ar no ajuste e 
### avalia��o de outros modelos.

fitted(mod2, what = 'sigma')[1]
### Estimativa do par�metro de escala.

########################################################################
########################################################################
########################################################################
### Parte 4 - Ajuste de um modelo linear generalizado com resposta gamma e
### fun��es suaves para as vari�veis num�ricas (Fl e A).

mod3 <- gamlss(R ~ pb(Fl) + pb(A) + H + loc, family = GA, data = rent)
### A fun��o pb() � a implementa��o do suavizador P-splines no gamlss.

GAIC(mod2, mod3, k = 2)
### O modelo com termos suaves produziu menor AIC do que o glm ajustado 
### anteriormente, produzindo, portanto, melhor ajuste.

summary(mod3)
### Os coeficientes para Fl e A referem-se apenas � parte linear dos efeitos
### dessas vari�veis. Para testar suas signific�ncias, considerando tamb�m
### a parte n�o linear, podemos usar a fun��o drop1().

drop1(mod3)
### A forma apropriada de avaliar o efeito das vari�veis ao usar suavizadores
### n�o param�tricos � por meio de gr�ficos de efeitos.

term.plot(mod3, pages = 1, ask = FALSE)
### O eixo vertical representa a m�dia na escala do preditor (log(mu)).
### Podemos observar rela��o praticamente linear com Fl, mas n�o linear
### conforme A (praticamente constante at� 1960, crescente a partir de
### ent�o).

### Vamos ajustar um modelo alternativo com suaviza��o apenas para o ano
### de constru��o do im�vel.

mod3_alt <- gamlss(R ~ Fl + pb(A) + H + loc, family = GA, data = rent)
GAIC(mod2, mod3, mod3_alt, k = 2)

### O AIC ainda � menor para o modelo em que fun��es suavizadoras s�o
### incorporadas para ambas as vari�veis, mas a diferen�a diminuiu
### consideravelmente, em rela��o ao modelo sem termos n�o param�tricos (mod2).
### As �reas sombreadas correspondem a intervalos de confian�a 95%.

plot(mod3)
### Um gr�fico �til para checar a dequa��o do ajuste � o worm plot. O worm
### plot � uma alternativa ao qqplot. O padr�o, para o caso em que o modelo
### est� bem ajustado, � os pontos dispersos em torno da linha horizontal,
### com 95% deles internos as linhas (bandas) tracejadas.


wp(mod3, ylim.all = 0.6)
### Podemos observar que os pontos n�o est�o dispersos conforme o esperado,
### com acentuada curvatura na parte esquerda do gr�fico.

########################################################################
########################################################################
########################################################################
### Parte 5 - Ajuste de um modelo linear generalizado com  fun��es suaves 
### para as vari�veis num�ricas (Fl e A) e inclus�o de covari�veis para
### o par�metro de dispers�o.

### Vamos testar duas distribui��es: gamma e normal inversa. Primeiro a 
### distribui��o Gamma.
mod4 <- gamlss(R ~ pb(Fl) + pb(A) + H + loc,
               sigma.fo =~ pb(Fl) + pb(A) + H + loc, family = GA, data = rent)
coef(mod4)
term.plot(mod4, pages = 1, ask = FALSE, what = 'mu')
### Gr�fico de efeitos para a m�dia (par�metro de loca��o). Percebe-se
### efeito aproximadamente linear, na escala do preditor, para Fl. Para H,
### verificamos efeito aproximadamente constante at� 1960, com eleva��o
### partir de ent�o.

term.plot(mod4, pages = 1, ask = FALSE, what = 'sigma')
### Gr�fico de efeitos para o par�metro de escala (dispers�o). Observa-se
### efeito claramente n�o linear de Fl e A na dispers�o. Al�m disso, maior
### efeito � verificado para casas com localiza��o 1, seguida pelas localiza��es
### II e III.


### Agora a distribui��o normal inversa.
mod5 <- gamlss(R ~ pb(Fl) + pb(A) + H + loc,
               sigma.fo =~ pb(Fl) + pb(A) + H + loc,
                family = IG, data = rent)
coef(mod5)

term.plot(mod5, pages = 1, ask = FALSE, what = 'mu')
### Gr�fico de efeitos para a m�dia (par�metro de loca��o).

term.plot(mod5, pages = 1, ask = FALSE, what = 'sigma')
### Gr�fico de efeitos para o par�metro de escala (dispers�o).

GAIC(mod3, mod4, mod5, k = 2)
### O modelo mod4 produziu melhor ajuste (menor valor de AIC). Mod 4 � o
### modelo com resposta gamma, fun��o suave para Fl e A e modelagem do
### par�metro de dispers�o.

drop1(mod4, what = 'sigma')
### Todas as covari�veis, exceto H, apresentam efeito significativo no 
### par�metro de dispers�o. Vamos atualizar o modelo, excluindo o efeito 
### n�o significativo.

mod4_alt <- update(mod4, ~.-H, what = 'sigma')
term.plot(mod4_alt, pages = 1, ask = FALSE, what = 'sigma')

### Vamos usar o worm plot para diagn�stico do ajuste.
wp(mod4_alt, ylim.all = 0.6)
### O gr�fico n�o reflete um ajuste satisfat�rio, com padr�o na forma
### de U invertido.


########################################################################
########################################################################
########################################################################
### Parte 6 - Ajuste de um modelo generalizado aditivo para loca��o, escala
### e forma com resposta Box-Cox Cole and Green.

### A fam�lia gamlss compreende diversas distribui��es, com at� quatro 
### par�metros, que permitem modelar dados com diferentes n�veis de dispers�o,
### assimetria e curtose. Nesta aplica��o vamos considerar a distribui��o
### Box-Cox Cole and Green (BCCGo).

help("dBCCGo")

### Vamos considerar a distribui��o BCCGo com par�metros mu = 1, sigma = 0.1, nu = 2.5.
dBCCGo(x = 1.5, mu = 1, sigma = 0.1, nu = 2.5) ### f(1.5)
pBCCGo(q = 1.5, mu = 1, sigma = 0.1, nu = 2.5) ### F(1.5) = P(X <= 1.5).
qBCCGo(p = 0.7, mu = 1, sigma = 0.1, nu = 2.5) ### q: F(q)  = P(X <= q) = 0.7.


### Vamos simular 100 valores aleat�rios da distribui��o Box-Cox Cole and Green,
### plotar o histograma dos valores simulados e sobrepor a curva da fun��o
### densidade de probabilidade do modelo usado na simula��o.

set.seed(87) ### Fixando a semente.
x <- rBCCGo(n = 100, mu = 1, sigma = 0.1, nu = 2.5) ### Gerando 100 valores aleat�rios.
hist(x, probability = TRUE, ylim = c(0,4.5))
curve(dBCCGo(x, mu = 1, sigma = 0.1, nu = 2.5), from = 0.6, to = 1.3, 
      add = TRUE, col = 'red', lwd = 2)

gamlss.demo() ### Alguma demo.

### Voltermos � an�lise dos dados dos pre�os de aluguel de im�veis. Vamos
### ajustar dois modelos: o primeiro (mod6) n�o incluindo covari�veis na
### modelagem do pr�metro de forma (nu). No segundo (mod7) as mesmas covari�veis
### usadas para modelar mu s�o usadas tamb�m para nu.

mod6 <- gamlss(R ~ pb(Fl) + pb(A) + H + loc,
               sigma.fo =~ pb(Fl) + pb(A) + loc, family = BCCGo, data = rent)

mod7 <- gamlss(R ~ pb(Fl) + pb(A) + H + loc,
               sigma.fo =~ pb(Fl) + pb(A) + loc, 
               nu.fo =~ pb(Fl) + pb(A) + H + loc, family = BCCGo, data = rent)

### Vamos comparar os ajustes usando o AIC.
GAIC(mod4, mod6, mod7, k = 2)

### O modelo mod7 produziu menor valor de AIC, indicando, portanto, melhor 
### ajuste. Nesse caso, al�m de loca��o e escala, tamb�m a forma da distribui��o 
### varia conforme os valores das covari�veis. 

### Vamos avaliar poss�vel retirada dos termos no preditor do par�metro de
### forma em mod7.

drop1(mod7, what = 'nu') ### Leva tempo...
### para o par�metro de forma, apenas H resultou em efeito significativo.
### Vamos ajustar novo modelo, extraindo os demais efeitos do preditor.

mod7_alt <- update(mod7, ~H, what = 'nu')
GAIC(mod7, mod7_alt, k = 2)
### O modelo que cont�m apenas o efeito de H em nu produziu ajuste com menor
### AIC, e � prefer�vel

### Vamos avaliar os gr�ficos de efeitos.
term.plot(mod7_alt, pages = 1, what = 'mu', ask = FALSE)
term.plot(mod7_alt, pages = 1, what = 'sigma', ask = FALSE)
term.plot(mod7_alt, pages = 1, what = 'nu', ask = FALSE)

### Vamos proceder com o diagn�stico do ajuste.
plot(mod7_alt)

wp(mod7_alt, ylim.all = 0.6, main = 'mod7_alt')
### O modelo, aparentemente, se ajusta bem aos dados.

########################################################################
### Neste ponto, vamos explorar o modelo ajustado, estimativas e predi��es.
### Vamos considerar o �ltimo modelo ajustado, mod7_alt.

### Vamos estimar os par�metros para as distribui��es dos pre�os de aluguel
### em dois cen�rios.

data_new <- data.frame(Fl = c(52, 82), A = c(1940, 1975), H = c('1', '0'), loc = c('1', '3'))
rownames(data_new) <- c('Imovel 1', 'Imovel 2')
### data_new armazena caracter�sticas de dois "perfis" distintos de im�veis. 

### Primeiramente, vamos estimar o pre�o m�dio de aluguel. 
pmu <- predict(mod7_alt, newdata = data_new, what = c('mu'))
pmu
### Estimativa na escala do preditor (log(mu)).

pmu <- predict(mod7_alt, newdata = data_new, what = c('mu'), type = 'response')
pmu
### Estimativa na escala do par�metro (mu).

### Agora, de maneira semelhante, para os demais par�metros do modelo.
### Para o par�metro de escala (sigma):
psigma <- predict(mod7_alt, newdata = data_new, what = c('sigma'))
psigma
### Estimativa na escala do preditor (log(sigma)).

psigma <- predict(mod7_alt, newdata = data_new, what = c('sigma'), type = 'response')
psigma
### Estimativa na escala do par�metro (sigma).

### Para o par�metro de forma (nu):
pnu <- predict(mod7_alt, newdata = data_new, what = c('nu'))
pnu
### Como a fun��o de liga��o � do tipo identidade, a estimativa � a mesma
### nas escalas do preditor e do par�metro.

### Vamos plotar as curvas das densidades ajustadas para os dois perfis
### de im�veis.
curve(dBCCGo(x, mu = pmu[1], sigma = psigma[1], nu = pnu[1]), 
      from = 0, to = 3500, xlab = 'Aluguel', ylab = 'Densidade', col = 'red', lwd = 2)
curve(dBCCGo(x, mu = pmu[2], sigma = psigma[2], nu = pnu[2]), 
      from = 0, to = 3500, col = 'blue', lwd = 2, add = TRUE)
legend(x = 'topright', legend = c('Imovel 1', 'Imovel 2'), col = c('red', 'blue'), lwd = 2)

### Probabilidade de um im�vel com valor de aluguel inferior a $500 para
### cada um dos dois perfis.
pBCCGo(500, mu = pmu[1], sigma = psigma[1], nu = pnu[1]) ### Perfil 1
pBCCGo(500, mu = pmu[2], sigma = psigma[2], nu = pnu[2]) ### Perfil 2

### Probabilidade de um im�vel com valor de aluguel superior a $1000 para
### cada um dos dois perfis.
pBCCGo(1000, mu = pmu[1], sigma = psigma[1], nu = pnu[1], lower.tail = FALSE) 
### Perfil 1
pBCCGo(1000, mu = pmu[2], sigma = psigma[2], nu = pnu[2], lower.tail = FALSE) 
### Perfil 2

### Estimativas dos pre�os medianos
qBCCGo(0.5, mu = pmu[1], sigma = psigma[1], nu = pnu[1]) 
### Perfil 1
qBCCGo(0.5, mu = pmu[2], sigma = psigma[2], nu = pnu[2]) 
### Perfil 2

### Estimativas dos quantis 75 e 90%
qBCCGo(c(0.75,0.90), mu = pmu[1], sigma = psigma[1], nu = pnu[1]) 
### Perfil 1
qBCCGo(c(0.75,0.90), mu = pmu[2], sigma = psigma[2], nu = pnu[2]) 
### Perfil 2

