### Carregando os arquivos para plotar o mapa (recomendo que voc� fa�a mas,
### caso queira pular essa etapa, basta desconsiderar os c�digos referentes
### aos mapas).

require(rgdal)
require(spdep)
require(GISTools)
require(plotGoogleMaps)
require(cluster)
require(corrplot)
require(car)
require(psych)
require(NbClust)
require(ggplot2)
options(device = 'x11')

########################################################################
### M�todo k-means para o dataset iris.
### Vamos considerar duas vari�veis para essa aplica��o: o comprimento e a largura
### da p�tala.

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + 
  geom_point(cex = 2)

### Na an�lise de clusters, nao vamos considerar a esp�cie das flores, 
### configurando a aplica��o de um m�todo n�o supervisionado.
ggplot(iris, aes(Petal.Length, Petal.Width)) + geom_point(cex = 2)

### Aplica��o do algoritmo k-means.
kmeans1 <- kmeans(iris[,3:4], centers = 3, nstart = 20)

### Fixamos k=3 grupos e usamos n=20 configura��es iniciais para determina��o do melhor
### agrupamento.

kmclust <- as.factor(kmeans1$cluster)
kmclust 
### Aloca��o das flores aos tr�s clusters constitu�dos.

### Vamos plotar novamente as duas vari�veis, mas agora colorindo os pontos segundo
### os clusters formados.

ggplot(iris, aes(Petal.Length, Petal.Width, color = kmclust)) + 
  geom_point(cex = 2)

### A solu��o de k=3 clusters mostrou-se eficiente em identificar grupos, 
### que s�o condizentes com as tr�s esp�cies.

table(kmclust, iris$Species)

### Tabela com as frequ�ncias cruzadas de esp�cie e cluster. 

########################################################################
### Descri��o da base: Indicadores s�cio-econ�micos dos munic�pios do PR.

### Vari�veis:
### IDHM_Long: Componente do IDH referente � longevidade da popula��o;
### IDHM_Educ: Componente do IDH referente � educa��o;
### Renda_PC: Renda familiar per-capita;
### IPDM_EmpRenda: Indice Ipardes desenvolvimento Municipal: Emprego, renda,...
### IPDM_Saude: Indice Ipardes desenvolvimento Municipal: Sa�se.

### Nota: Como nem todas as vari�veis, originalmente, estavam numa mesma
### escala, procedeu-se a padroniza��o (subtraindo dos valores de cada
### vari�vel a respectiva m�dia e dividindo o resultado pelo resp. desvio
### padr�o.)

PR <- readOGR("C:/Users/Samsung/Dropbox/DSBD", "41MUE250GC_SIR", use_iconv = TRUE, encoding = "UTF-8")
head(PR@data)

### Importando os dados s�cio-econ�micos dos munic�pios.
setwd("C:/Users/Samsung/Dropbox/DSBD")
dados <- read.csv2('Dados_PRCluster.csv', dec = ',',  header = T)

### Atribuindo os nomes dos munic�pios como nomes das linhas; selecionando
### as vari�veis que efetivamente ser�o usadas na an�lise.
rownames(dados) <- dados[,1]
dados <- dados[,-1]
dados$Renda_PC <- (dados$Renda_PC - min(dados$Renda_PC))/(max(dados$Renda_PC) - min(dados$Renda_PC))

### Os comandos na sequ�ncia tem por objetivo sincronizar as ordens das
### linhas (munic�pios) nas duas bases.
maiusc <- toupper(rownames(dados))
ordem <- pmatch(PR$NM_MUNICIP, maiusc)
odados <- dados[ordem,]

### w1 guarda a posi��o de Curitiba nas bases. Como Curitiba tem resultados
### extremos, algumas visualiza��es (mapas) ser�o feitas sem Curitiba, para
### melhor avalia��o.
w1 <- which(rownames(odados) == 'Curitiba')
w1

### Fun��o para plotar os nomes dos munic�pios nos mapas.
sp.label <- function(x, label) {list("sp.text", coordinates(x), label, cex=0.5)}

########################################################################
### Vamos visualizar, espacialmente, a distribui��o de cada indicador.
x11(width = 20, height = 15)
par(mar = c(0,0,0,0))

### IDHM longevidade.
PR@data$IDHM_Long <- odados$IDHM_Long
spplot(PR, "IDHM_Long", col.regions = terrain.colors(20), 
       main = 'Distribui��o espacial - IDHM longevidade',
       sp.layout = sp.label(PR, rownames(odados)))


### IPDM - Emprego e renda.
# Com Curitiba
PR@data$IPDM_EmpRenda <- odados$IPDM_EmpRenda
spplot(PR, "IPDM_EmpRenda", col.regions = terrain.colors(20), 
       main = 'Distribui��o espacial - IDHM longevidade',
       sp.layout = sp.label(PR, rownames(odados)))

# Sem Curitiba
PR@data$IPDM_EmpRendaNA <- PR@data$IPDM_EmpRenda
PR@data$IPDM_EmpRendaNA[w1] <- NA
spplot(PR, "IPDM_EmpRendaNA", col.regions = terrain.colors(50), 
       main = 'Distribui��o espacial - IDHM longevidade',
       sp.layout = sp.label(PR, rownames(odados)))


### IPDM - Sa�de
PR@data$IPDM_Saude <- odados$IPDM_Saude
spplot(PR, "IPDM_Saude", col.regions = terrain.colors(20), 
       main = 'Distribui��o espacial - IDHM longevidade',
       sp.layout = sp.label(PR, rownames(odados)))


### IDHM - Educa��o
PR@data$IDHM_Educ <- odados$IDHM_Educ
spplot(PR, "IDHM_Educ", col.regions = terrain.colors(20), 
       main = 'Distribui��o espacial - IDHM longevidade',
       sp.layout = sp.label(PR, rownames(odados)))


### Renda per capita
PR@data$Renda_PC <- odados$Renda_PC
spplot(PR, "Renda_PC", col.regions = terrain.colors(20), 
       main = 'Distribui��o espacial - IDHM longevidade',
       sp.layout = sp.label(PR, rownames(odados)))

# Sem Curitiba
PR@data$Renda_PCNA <- PR@data$Renda_PC
PR@data$Renda_PCNA[w1] <- NA
spplot(PR, "Renda_PCNA", col.regions = terrain.colors(20), 
       main = 'Distribui��o espacial - IDHM longevidade',
       sp.layout = sp.label(PR, rownames(odados)))

### Mapa interativo.
tmp <- plotGoogleMaps(PR, 'teste.html', zcol=4)

########################################################################
### Agora, vamos analisar as correla��es.
corrplot.mixed(cor(dados), number.cex = 1.5, upper = 'ellipse')
scatterplotMatrix(dados)

########################################################################
### Matriz de dist�ncias. Como as vari�veis s�o todas cont�nuas, vamos
### usar dist�ncia euclidiana.

help(dist)
mat_dist <- dist(dados, method = 'euclidean')
as.matrix(mat_dist)[1:6,1:6]

########################################################################
### An�lise de clusters - m�todo k-means.

### Vamos testar alguns crit�rios para determina��o do n�mero de clusters.
### Vamos considerar solu��es com 2 a 12 clusters.

### Crit�rio 1 - Gr�fico das domas de quadrados intra clusters.
within <- numeric()
for(k in 1:12){
  within[k] <- kmeans(dados,k)$tot.withinss
}

plot(1:12, within, xlab = 'N�mero de clusters', 
     ylab = 'Soma de quadrados dentro dos clusters', pch = 20, cex = 1.2,
     type = 'b', xaxt = 'n')
axis(1, 1:12)
### ...

### Crit�rio 2 - Estat�stica GAP
gap <- clusGap(dados, FUN = kmeans, nstart = 20, K.max = 12, B = 60)
plot(gap, main = '', xaxt = 'n')
axis(1, 1:12)

### Crit�rio 3 - Silhueta m�dia.
sil <- numeric()
for(k in 2:12){
  km.res <- kmeans(dados, centers = k, nstart = 20)
  ss <- silhouette(km.res$cluster, dist(dados))
  sil[k-1] <- mean(ss[, 3])
}
plot(2:12, sil, xlab = 'N�mero de clusters', 
     ylab = 'Silhueta m�dia', pch = 20, cex = 1.2,
     type = 'b', xaxt = 'n')
axis(1, 2:12)

### Agora, usando no atacado os crit�rios implementados no pacote nbClust.
nb <- NbClust(data = dados, diss = NULL, distance = "euclidean",
              min.nc = 2, max.nc = 12, method = "kmeans", index = "all")
### A solu��o com 3 clusters � apontada por 10 crit�rios, recebendo a maior
### quantidade de votos. Seguimos com ela.

ksol <- kmeans(dados, centers = 3, nstart = 20)
ksol
kclu <- ksol$cluster; kclu 
### Aloca��o dos munic�pios aos clusters.
table(kclu)

### Rodamos para 20 sementes (20 configura��es de pontos de gravidade).
### E se rod�ssemos s� para uma, chegar�amos � mesma solu��o?
ksol$tot.withinss
set.seed(200)
ksol2 <- kmeans(dados, centers = 3, nstart = 1)
ksol2$tot.withinss
### Observe que a soma de quadrados � ligeiramente diferente (maior), indicando
### que n�o se trata da mesma solu��o.

### Vamos explorar a solu��o de tr�s clusters. Primeiro, espacialmente.
PR@data$grupos <- factor(kclu)[ordem]
spplot(PR, "grupos", col.regions = c('yellow', 'red', 'white'), 
       main = 'Distribui��o espacial - Clusters',
       sp.layout = sp.label(PR, rownames(odados)))

x11(width = 20, height = 15)
par(mfrow = c(2,3), mar = c(4,4,2,2), las = 1, cex = 1.2, pch = 20)
plot(PR@data$IDHM_Long ~ PR@data$grupos, col = c('yellow', 'red', 'white'), 
     xlab = 'Cluster', ylab = 'IDHM Longevidade')
plot(PR@data$IPDM_EmpRenda ~ PR@data$grupos, col = c('yellow', 'red', 'white'), 
     xlab = 'Cluster', ylab = 'IPDM Emprego e renda')
plot(PR@data$IPDM_Saude ~ PR@data$grupos, col = c('yellow', 'red', 'white'), 
     xlab = 'Cluster', ylab = 'IPDM Sa�de')
plot(PR@data$IDHM_Educ ~ PR@data$grupos, col = c('yellow', 'red', 'white'), 
     xlab = 'Cluster', ylab = 'IDHM Educa��o')
plot(PR@data$Renda_PC ~ PR@data$grupos, col = c('yellow', 'red', 'white'), 
     xlab = 'Cluster', ylab = 'Renda per capita')

########################################################################
########################################################################
########################################################################
### Nesta aplica��o, vamos usar an�lise de clusters para identificar grupos
### de universidades, do mundo todo, segundo as respectivas posi��es nos
### seguintes rankings:

### y1: Qualidade de ensino;
### y2: Empregabilidade dos alunos;
### y3: Qualidade da institui��o;
### y4: Publica��es;
### y5: Influ�ncia externa;
### y6: Cita��es;
### y7: Impacto geral;
### y8: Patentes.

data_uni <- read.csv("Univ_Cluster.csv")
head(data_uni, 10)
data_uni <- data_uni[,-1]
summary(data_uni)

data_cluster <- data_uni[,3:10]
### data_cluster cont�m apenas as vari�veis que ser�o usadas na an�lise de clusters.
corrplot.mixed(cor(data_cluster), lower.col = "black", number.cex = 1.5, 
               upper = 'ellipse')

mat_dist <- dist(data_cluster)
as.matrix(mat_dist)[1:6,1:6]
### Matriz de dist�ncias.

### Vamos avaliar os dendrogramas produzidos por diferentes algoritmos de
### agrupamentos hier�rquicos.
hc_single <- hclust(mat_dist, method = 'single')
hc_complete <- hclust(mat_dist, method = 'complete')
hc_average <- hclust(mat_dist, method = 'average')
hc_ward <- hclust(mat_dist, method = 'ward.D2')
hc_median <- hclust(mat_dist, method = 'median')
hc_centroid <- hclust(mat_dist, method = 'centroid')

par(mfrow = c(2,3), mar = c(4,4,2,2), cex = 1)
plot(hc_single, main = 'Single linkage', xlab = '', sub = '', labels = FALSE)
plot(hc_complete, main = 'Complete linkage', xlab = '', sub = '', labels = FALSE)
plot(hc_average, main = 'Average linkage', xlab = '', sub = '', labels = FALSE)
plot(hc_ward, main = 'Ward', xlab = '', sub = '', labels = FALSE)
plot(hc_median, main = 'Median', xlab = '', sub = '', labels = FALSE)
plot(hc_centroid, main = 'Centroid', xlab = '', sub = '', labels = FALSE)

### Vamos investir na solu��o produzida pelo m�todo do vizinho mais distante
### (complete linkage). Vamos investigar um n�mero "�timo" de clusters.

par(mar = c(4,4,2,2), cex = 1)
plot(hc_complete, main = 'Complete linkage', xlab = '', sub = '', labels = FALSE)

### A fun��o hclust n�o retorna as somas de quadrados. A fun��o abaixo
### ser� usada para extrair as somas de quadrados intra n�s.
wss <- function(x) (nrow(x)-1) * sum(apply(x, 2, var))

k_max <- 12 ### N�mero m�ximo de clusters a ser investigado.
wssC <- rep(0, k_max-1)

### Calculando a soma de quadrados intra n�s para solu��es de 2 a k_max grupos.
for(i in 2:k_max){
  clusters <- cutree(hc_complete, i)
  for(j in 1:i)
    wssC[i-1] <- wssC[i-1] + wss(data_cluster[which(clusters == j),])
}  

plot(2:k_max, wssC, xlab = 'N�mero de clusters', 
     ylab = 'Soma de quadrados dentro dos clusters', pch = 20, cex = 1.2,
     type = 'b', xaxt = 'n')
axis(1, 2:12)
### A solu��o com tr�s n�s parece ser a mais indicada. Vamos usar crit�rios 
### adicionais, implementados na fun��o nbClust.

nclust <- NbClust(data_cluster, diss=NULL, distance = "euclidean", min.nc=2, max.nc=12, 
                  method = "complete", index = "all") 
### A maior parte dos crit�rios aponta para a solu��o com k=3 clusters.
### Vamos investir nela.

rect.hclust(hc_complete, 3)
clust_final <- factor(cutree(hc_complete, 3))
clust_final
table(clust_final)

### Vamos explorar as caracter�sticas das faculdades nos tr�s clusters.

x11(width = 20, height = 15)
par(mfrow = c(2,4), mar = c(4,4,2,2), las = 1, cex = 1.2, pch = 20)
plot(data_cluster$y1 ~ clust_final, col = c('white', 'yellow', 'red'), 
     xlab = 'Cluster', ylab = 'Qualidade de ensino')
plot(data_cluster$y2 ~ clust_final, col = c('white', 'yellow', 'red'), 
     xlab = 'Cluster', ylab = 'Empregabilidade')
plot(data_cluster$y3 ~ clust_final, col = c('white', 'yellow', 'red'), 
     xlab = 'Cluster', ylab = 'Qualidade da institui��o')
plot(data_cluster$y4 ~ clust_final, col = c('white', 'yellow', 'red'), 
     xlab = 'Cluster', ylab = 'Publica��es')
plot(data_cluster$y5 ~ clust_final, col = c('white', 'yellow', 'red'), 
     xlab = 'Cluster', ylab = 'Influ�ncia externa')
plot(data_cluster$y6 ~ clust_final, col = c('white', 'yellow', 'red'), 
     xlab = 'Cluster', ylab = 'Cita��es')
plot(data_cluster$y7 ~ clust_final, col = c('white', 'yellow', 'red'), 
     xlab = 'Cluster', ylab = 'Impacto geral')
plot(data_cluster$y8 ~ clust_final, col = c('white', 'yellow', 'red'), 
     xlab = 'Cluster', ylab = 'Patentes')

table(data_uni$country, clust_final)
### Distribui��o das universidades por cluster e pa�s.

data_uni[data_uni$country=='Brazil',]
clust_final[data_uni$country=='Brazil']
### Universidades brasileiras.

silhueta <- silhouette(cutree(hc_complete, 3), dist(data_cluster))
plot(silhueta, col = c('green', 'yellow', 'red'))