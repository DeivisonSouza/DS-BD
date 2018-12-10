### Aplicação de análise de componentes principais.
require(psych)
require(asbio)
require(corrplot)
require(car)

data(wine)
help(wine)

### A base de dados wine contém características químicas e sensoriais de
### 4898 variedades de vinho.

help(places)
dados <- wine[,-1]

matcor <- cor(dados)
corrplot.mixed(matcor, upper = 'ellipse')
scatterplotMatrix(dados)

### Análise de componentes principais baseada na matriz de covariâncias
### (dados não escalonados)
pca <- princomp(dados, cor = FALSE, scores = TRUE)
summary(pca)
### O primeiro componente, como era de se esperar, é aquele que apresenta
### maior variância (DP=43.94), explicando 90.9% da variância original
### dos dados; o segundo componente, por sua vez, tem DP=12.97, e explica
### 8% da variância original dos dados. Desta forma, os dois primeiros
### componentes, conjuntamente, explicam 99% da variância original. A partir
### de então, os componentes restantes têm uma parcela de explicação cada
### vez menor.

pca$loadings
### O primeiro componete apresenta maior carga (0.964) para a variável X6
### (conteúdo total de dióxido sulfúrico), refletindo, basicamente, o valor
### desta variável. Já o segundo componente representa o conteúdo livre de
### dióxido sulfúrico.

pca$scores[1:10,]
### Scores dos componentes calculados para as dez primeiras linhas (vinhos)
### da base.

cor_scores <- cor(pca$scores)
corrplot.mixed(cor_scores, upper = 'ellipse')
### Como era de se esperar, as correlações são iguais a zero, os componentes
### são ortogonais.


### Análise de componentes principais baseada na matriz de covariâncias
### (dados escalonados)
pca <- princomp(dados, cor = TRUE, scores = TRUE)
summary(pca)
### O primeiro componente explica aproximadamente 29,3% da variação dos dados
### escalonados. Os dois primeiros explicam em torno de 43,6% e, se considerarmos os
### quatro primeiros componentes, temos aproximadamente 64% da variação explicada. 

print(loadings(pca), cutoff = 0.2)
### As conclusões a seguir baseiam-se nos sinais das variáveis com carga
### superior a 0.2 (em valor absoluto) para cada componente.

### O primeiro componente contrasta o açúcar residual, as quantidades de cloreto,
### dióxido sulfúrico livre e total e a densidade em relação ao teor alcoólico.

### O segundo componente contrasta as duas quantidades de dióxido sulfúrico,
### ph e sulfato em relação ao conteúdo de ácido cítrico e à acidez. 

### Assim por diante para os demais componentes.
plot(pca, type = 'l')
abline(h=1, col = 'red')
### As soluções de 4 a 6 componentes principais podem ser avaliadas.

biplot(pca)
biplot(pca, xlabs=rep("", nrow(dados)), choices = 1:2)
abline(v = 0, lty = 2)
abline(h = 0, lty = 2)
text(x = 15, y = 55, 'PCA2')
text(x = 48, y = -8, 'PCA1')

### Primeiro e segundo componentes.

biplot(pca, xlabs=rep("", nrow(dados)), choices = 3:4)
abline(v = 0, lty = 2)
abline(h = 0, lty = 2)
text(x = 12, y = 45, 'PCA4')
text(x = 40, y = -5, 'PCA3')
### Terceiro e quarto componentes.