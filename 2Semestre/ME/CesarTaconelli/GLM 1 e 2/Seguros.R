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
### Regress?o para dados de contagens. Informa??es referentes a 500 portadores
### de seguro de autom?vel de uma particular seguradora. Foram filtrados
### apenas os segurados cujas ap?lices t?m mais de cinco anos. As vari?veis
### s?o as seguintes:

# idade: idade do segurado (em anos);

# sexo: Masc para masculino e Fem para feminino;

# usop: uso principal do ve?culo. Cidade para uso urbano e Estrada para 
# uso rodovi?rio;

# anosest: escolaridade do segurado, em anos de estudo;

# claims: n?mero de sinistros produzidos pelo segurado nos ?ltimos cinco
# anos.

### O objetivo aqui ? modelar a frequ?ncia de sinistros em fun??o das demais
### covari?veis.

########################################################################
### Importa??o dos dados e an?lise descritiva.

dados <- read.csv2('https://docs.ufpr.br/~taconeli/CE22517/sinistros.csv')[,-1]

head(dados, 10) ### Dez primeiras linhas da base.
summary(dados) ### Algumas descritivas dos dados.

### An?lise descritiva

x11(width = 12, height = 10)
par(las = 1, mar = c(5,4.5,2,2), cex = 1.4)
plot(table(dados$claims), col = 'blue', lwd = 5, xlab = 'N?mero de sinistros',
     ylab = 'Frequ?ncia', xaxt = 'n')
axis(1, 0:9) 
### Distribui??o de frequ?ncias para o n?mero de sinistros.

d1 <- with(dados, data.frame(table(sexo, factor(claims, levels = 0:9))))
names(d1) <- c("sexo","claims","Freq")
ggplot(data = d1, aes(x = claims, y = Freq)) + geom_bar(stat = "identity")+ 
    facet_wrap(~ sexo) + theme_bw(base_size = 18) + ylab("Frequ?ncia") + xlab('N?mero de sinistros')

d2 <- with(dados, data.frame(table(usop, factor(claims, levels = 0:9))))
names(d2) <- c("usop","claims","Freq")
ggplot(data = d2, aes(x = claims, y = Freq)) + geom_bar(stat = "identity")+ 
    facet_wrap(~ usop) + theme_bw(base_size = 18) + ylab("Frequ?ncia") + xlab('N?mero de sinistros')

ggplot(data = dados, aes(x = anosest, y = claims)) + geom_jitter() +
    theme_bw(base_size = 18) + ylab("N?mero de sinistros") + xlab('Anos de estudo') +
    geom_smooth(method = 'loess')

ggplot(data = dados, aes(x = idade, y = claims)) + geom_jitter() +
    theme_bw(base_size = 18) + ylab("N?mero de sinistros") + xlab('Idade') +
    geom_smooth(method = 'loess')

########################################################################
### Ajuste de um modelo linear com erros normais

### Vamos ajustar um modelo de regress?o linear, com erros normais, para o n?mero 
### de sinistros.
x11(width = 10, height = 10)
ajuste1 <- lm(claims ~ idade + sexo + usop + anosest, data=dados)
par(mfrow=c(2,2), cex = 1.4, las = 1)
plot(ajuste1)
### Os res?duos s?o claramente heteroced?sticos (vari?ncia aumenta conforme
### a m?dia) e tem distribui??o assim?trica. O modelo linear n?o se ajusta
### bem aos dados.

########################################################################
### Ajuste de um modelo log-linear com resposta poisson

ajuste2 <- glm(claims ~ . ,family = poisson(link = 'log'), data = dados)
coefficients(ajuste2) 
### Estimativas dos par?metros de regress?o.

### Vamos extrair mais alguns elementos produzidos pela fun??o glm:

head(model.matrix(ajuste2)) 
### Matriz do modelo.

fitted(ajuste2) 
### Valores ajustados pelo modelo, para os 500 indiv?duos da base, na escala 
### da resposta.

predict(ajuste2) 
### Valores ajustados pelo modelo, para os 500 indiv?duos da base, na escala 
### do preditor.
 
### Recomendo acessar a documenta??o da fun??o predict.glm para conferir 
### as op??es. Voltaremos a ela adiante.

ajuste2$iter 
### Foram necess?rias cinco itera??es at? a converg?ncia no processo de estima??o.

ajuste2$weights 
### Esses s?o os pesos calculados na ?ltima itera??o do algoritmo de estima??o.

summary(ajuste2) 
### Um resumo mais detalhado do modelo ajustado.

########################################################################
### Diagn?stico do ajuste.

x11(width = 10, height = 10)
par(mfrow=c(2,2), cex = 1.4, las = 1)
plot(ajuste2)

### Os res?duos utlizados ao plotar um objeto da classe glm s?o os res?duos
### componentes da deviance. Nem sempre esses res?duos t?m boa aproxima??o
### com a distribui??o normal se o modelo ajustado estiver correto. Isso
### ocorre, particularmente, para dados bin?rios ou de contagens. No entanto,
### ? poss?vel notar comportamento mais aceit?vel (vari?ncia mais homog?nea,
### melhor aproxima??o com a distribui??o Normal) em rela??o ao que foi 
### verificado para o modelo linear.

### para uma melhor aprecia??o do comportamento dos res?duos, vamos proceder
### com a an?lise dos res?duos quant?licos aleatorizados e a obten??o do
### gr?fico meio normal com envelopes simulados.

residuos <- qres.pois(ajuste2)
ajustados <- predict(ajuste2)
x11(width = 12, height = 10)
par(las = 1, mar = c(5,4.5,2,2), mfrow = c(1,2), cex = 1.4)
plot(residuos ~ ajustados, col = 'blue', xlab = 'Valores ajustados', ylab = 'Res?duos')
lines(lowess(residuos ~ ajustados), col = 'red', lwd = 2)
qqnorm(residuos, col = 'blue', main = '')
qqline(residuos, lty = 2)
### A curvatura da fun??o suave, presente no gr?fico ? esquerda, ? resultante
### da pequena quantidade de pontos nos extremos dos valores ajustados.
### De qualquer forma, fica evidente que os res?duos tem dispers?o aproxim.
### constante e boa aproxima??o com a distribui??o Normal.

x11()
par(las = 1, mar = c(5,4.5,2,2), cex = 1.4)
hnp(ajuste2)
### O gr?fico indica que o modelo se ajusta bem aos dados.

########################################################################
### Diagn?stico de outliers e pontos influentes.

### Vamos fazer uma an?lise de influ?ncia usando recursos do pacote car.
influenceIndexPlot(ajuste2, vars = c('Studentized','Cook','Hat'), id.n = 3)
### O gr?fico dos res?duos studentizados n?o indica observa??es com res?duos extremos.
### O gr?fico da dist?ncia de Cook tem dois pontos mais discrepantes: 15 e 385.
### O gr?fico dos valores de h aponta a observa??o 295 como poss?vel ponto de alavanca.

### Vamos avaliar os dados destacados na an?lise.
dados[15,]
dados[295,]
dados[385,]

### Vamos ver o summary dos dados e do ajuste para entender melhor o por 
### que desse dados serem destacados.
summary(dados)
summary(ajuste2)

# O indiv?duo 15 gerou nove sinistros, sendo o segurado com maior n?mero 
# de sinistros;

# O indiv?duo 385 tem 47 anos e usa o carro na cidade, mas registrou um 
# n?mero elevado de sinistros (cinco).

# O indiv?duo 295 ? at?pico, tem 16 anos de idade e 16 de estudo!!! Deve 
# ser erro de registro.

### Vamos tirar da base as observa??es 15, 295 e 385 e reajustar o modelo.

ajuste3 <- update(ajuste2, subset= -c(15,295,385))
compareCoefs(ajuste2, ajuste3) 
multiplot(ajuste2, ajuste3)
### Compara??o de estimativas e erros padr?es produzidos pelos dois modelos.
### Os resultados (e, consequentemente, as infer?ncias) n?o mudam de forma
### substancial ao remover as tr?s informa??es da base.

########################################################################
### Infer?ncia e predi??es.

summary(ajuste2)
### Os efeitos ajustados de sexo e anos de estudo n?o s?o significativos.
### Vamos ajustar um novo modelo sem essas duas covari?veis e testar a
### hip?tese de que eles s?o conjuntamente n?o significativos, via teste
### da raz?o de verossimilhan?as.

ajuste4 <- update(ajuste2, ~ idade + usop)
anova(ajuste4, ajuste2, test = 'Chisq')
### O resultado n?o significativo permite concluir que essas duas covari?veis
### n?o s?o importantes no modelo. Vamos considerar o ajuste4, daqui em diante.

### vamos investigar se o efeito de intera??o entre idade e uso principal
### ? significativo.
ajuste4_int <- update(ajuste4, ~.+ idade:usop)
anova(ajuste4, ajuste4_int, test = 'Chisq')

### O efeito de intera??o ? estatisticamente significativo. vamos ver o
### resumo do ajuste.
summary(ajuste4_int)
### O efeito de idade ? negativo na frequ?ncia de sinistros (segurados
### de maior idade produzem sinistro com menor frequ?ncia). No entanto,
### isso ? ainda mais acentuado para aqueles que fazem uso principal na
### estrada.

### Intervalos de confian?a para os par?metros de regress?o baseados nos
### perfis de verossimilhan?a.
confint(ajuste4_int)

### Exponenciando os limites, para uma escala em que s?o interpret?veis:
exp(confint(ajuste4_int))

### Podemos construir intervalos de confian?a do tipo Wald (baseados na
### distribui??o assint?tica normal dos EMVs) usando a fun??o confint.default.
confint.default(ajuste4_int)
exp(confint.default(ajuste4_int))

### Podemos pedir intervalos com outros n?veis de confian?a alternado o 
### argumento level.
confint(ajuste4_int, level = 0.99)
confint.default(ajuste4_int, level = 0.90)

### Agora, vamos estimar o n?mero m?dio de sinistros para alguns perfis
### de segurados. Primeiro, para segurados de 30 anos que fazem uso principal 
### na estrada.

predict(ajuste4_int, newdata = data.frame(idade = 30, usop = 'Estrada'))
### Por default, o R retorna a predi??o na escala do link. Para obter a
### estimativa da m?dia devemos aplicar a inversa da fun??o de liga??o:

exp(1.241035)

### ou pedir a predi??o direto na escala da m?dia (resposta):
predict(ajuste4_int, newdata = data.frame(idade=30, usop = 'Estrada'), type = 'response')

### Podemos realizar predi??es para toda uma base de novos indiv?duos. Uma
### pequena ilustra??o:

dpred <- data.frame(idade = c(30,30,60,60),
                    usop = c('Estrada','Cidade','Estrada','Cidade'))

mu_est <- predict(ajuste4_int, newdata = dpred, type = 'response')
dpred$mu <- mu_est
rownames(dpred) <- c('Perfil 1', 'Perfil 2', 'Perfil 3', 'Perfil 4')
dpred
### estimativas para o n?mero esperado de sinistros em cinco anos para os
### quatro perfis de segurados.

### Vamos ver a distribui??o de probabilidades estimada para cada perfil.
### Primeiro, vamos calcular as probabilidades ajustadas, usando as m?dias
### estimadas para cada perfil.

Perfil1 <- dpois(0:10, mu_est[1])
Perfil2 <- dpois(0:10, mu_est[2])
Perfil3 <- dpois(0:10, mu_est[3])
Perfil4 <- dpois(0:10, mu_est[4])

### Agora vamos plotar as probabilidades para avaliar as distribui??es.
x11(width = 12, height = 10)
par(mfrow = c(2,2), cex = 1.3, las = 1, mar = c(5,4,2,2), las = 1)
plot(0:10, Perfil1, type = 'h', lwd = 3, xlab = 'x', main = 'Perfil 1', 
     xaxt = 'n', ylim = c(0, 0.7))
axis(1, 0:10)
plot(0:10, Perfil2, type = 'h', lwd = 3, xlab = 'x', main = 'Perfil 2', 
     xaxt = 'n', ylim = c(0, 0.7))
axis(1, 0:10)
plot(0:10, Perfil3, type = 'h', lwd = 3, xlab = 'x', main = 'Perfil 3', 
     xaxt = 'n', ylim = c(0, 0.7))
axis(1, 0:10)
plot(0:10, Perfil4, type = 'h', lwd = 3, xlab = 'x', main = 'Perfil 4', 
     xaxt = 'n', ylim = c(0, 0.7))
axis(1, 0:10)

########################################################################
### Gr?ficos de efeitos
plot(allEffects(ajuste4_int)) ### Escala do link.
plot(allEffects(ajuste4_int), type = 'response') ### Escala da resposta.