########################################################################
########################################################################
########################################################################
### Exerc?cio - Dados s?cio econ?micos do censo do Paran? (2010).
### Vari?veis:
# Munic?pio: Nome do munic?pio;
# Urbaniza??o: Grau de urbaniza??o (%);
# Idosos: ?ndice de idosos (%);
# P60: Probabilidade de sobreviv?ncia at? 60 anos;
# Renda: Renda m?dia domiciliar;
# Analfabetismo: Taxa de analfabetismo.

library(car)
dados=read.csv2('https://docs.ufpr.br/~taconeli/CE07118/parana.csv') 
### Importa??o dos dados.
summary(dados) ### Resumo dos dados.
head(dados) ### Seis primeiras linhas.

### Vamos analisar a correla??o entre a probabilidade de sobreviv?ncia 
### aos 60 anos e a taxa de analfabetismo.

with(dados, plot(Analfabetismo, P60, pch = 20, xlab = 'Taxa de analfabetismo (%)',
                 ylab='Probabilidade de sobreviv?ncia - 60 anos', las = 1))
# Gr?fico de dispers?o.

with(dados, lines(lowess(Analfabetismo,P60), col = 'red', lwd = 2))
# Adicionando uma "linha de tend?ncia" (regress?o n?o param?trica).

r <- cor(dados$Analfabetismo, dados$P60) 
r
### Correla??o linear entre as duas vari?veis.

### Vamos testar a signific?ncia da correla??o linear. A hip?tese nula ?
### que a correla??o ? igual a zero.

### Estat?stica teste:
n <- nrow(dados)
t_calc <- r * sqrt((n-2)/(1-r^2))

### Vamos ver como ficaria a decis?o aos n?veis de 5% e 1% de signific?ncia.
### Lembre-se que estamos efetuando um teste bilateral (a hip?tese nula
### ? de diferen?a), ent?o os valores cr?ticos correspondentes delimitam
### alpha/2 de probabilidade para cada lado da distribui??o.

t_crit_5 <- qt(0.975, n-2); t_crit_5
t_crit_1 <- qt(0.995, n-2); t_crit_1

### Como |t_calc| = 10.58 > t_crit_1 = 2.58, rejeitamos a hip?tese nula.
### Ou seja, os dados apresentam correla??o (negativa) significativa.

### Agora usando a fun??o cor.test do R.

cor.test(dados$Analfabetismo, dados$P60) 
### Teste de hip?tese e intervalo de confian?a para a correla??o. Fa?a sua an?lise.

cor.test(dados$Analfabetismo,dados$P60,alternative='less') 
### Trocando a hip?tese alternativa.

### Agora vamos analisar as correla??es das cinco vari?veis da base (duas a duas).

### Matrizes de gr?ficos de dispers?o.
pairs(dados[,-1], cex.labels=1.4) 
scatterplotMatrix( ~ Urbaniza??o + Idosos + P60 + Renda + Analfabetismo,
                   data = dados, smooth = F, pch = 20, lwd = 2)

cor(dados[,-1]) 
### Matriz de correla??es para as cinco vari?veis.


########################################################################
########################################################################
########################################################################

### Exemplo - an?lise de correla??o usando simula??o.

### Como alternativa, vamos usar um teste de hip?tese baseado estritamente 
### em permuta??es. Para isso, vamos construir a distribui??o da correla??o
### amostral usando aleatoriza??o. nesta aplica??o vamos considerar duas
### outras vari?veis: taxa de idosos e urbaniza??o. O procedimento ? o seguinte:

dados_aleat <- data.frame(dados$Urbanização, dados$Idosos)
names(dados_aleat) <- c('Urbanização', 'Idosos')
r <- cor(dados_aleat[,1], dados_aleat[,2]); r
c_test <- cor.test(dados_aleat[,1], dados_aleat[,2]); c_test

### 1- Criar um novo conjunto de oberva?oes (x_i, y_i'), i = 1, 2, ..., 399,
### em que os valores x_i s?o fixados na sequ?ncia em que aparecem na base
### e os valores de y_i t?m sua ordem aleatorizada.

sorteio <- sample(1:399) ### ordem aleat?ria.
dados_aleat[,2] <- dados_aleat[sorteio,2] 
### Aleatorizando os valores da taxa de idosos.

### 2- Para o conjunto de pares obtidos, calcular a correlação amostral.
### Este é um valor de correlação simulado sob a hipótese nula, uma
### vez que a aleatorização elimina possível correlação presente nos
### dados.

r_aleat <- cor(dados_aleat[,1], dados_aleat[,2])
r_aleat

### 3- Repita os passos 1 e 2 um grande número de vezes. Armazene os valores
### simulados para a correlação amostral.

total <- 100000
r_aleat <- numeric()

for(i in 1:total){
    sorteio <- sample(1:399)
    dados_aleat[,2] <- dados_aleat[sorteio,2]
    r_aleat[i] <- cor(dados_aleat[,1], dados_aleat[,2])
}


### 4 - Calcule o p-valor produzido pela simulação que é a proporção de correlações
### simuladas que excedem (em valor absoluto) a correlação amostral original.

p_aleat <- sum(abs(r_aleat) > abs(r))/total; p_aleat

hist(r_aleat)
abline(v = r, col = 'red')

### Vamos comparar com o resultado do teste t.
c_test
### Embora os p-valores sejam numericamente diferente, são muito próximos, 
### conduzem a uma mesma conclusão.



