#########################################################################
### Estimação de centis e curvas centílicas. Vamos usar a base de dados
### dbbmi, do pacote gamlss
require(gamlss) 
require(gamlss.demo)
require(gamlss.util)

options(device = 'x11')
data(dbbmi) 
help("dbbmi")
### Dados sobre índice de massa corporal (bmi) e idade (age) de uma população
### de 7294 alemães do sexo masculino, com idade inferior a 22 anos. O interesse
### é construir as curvas centílicas de bmi versus idade.

### Gráfico de dispersão para a base completa
plot(bmi ~ age, col = gray(0.5), cex = 1.2, data = dbbmi, xlab = 'Idade (anos)',
     ylab = 'Índice de massa corporal')

### Para esta aplicação, vamos considerar apenas uma amostra, selecionada de
### forma aleatória, de 1000 indivíduos da base original.

### Seleção dos 1000 indivíduos que vão compor a base para a análise.
set.seed(2803)

ind <- sample(1:7294, 1000, replace = FALSE)
base <- dbbmi[ind,]

### Gráfico de dispersão para a base selecionada
plot(bmi ~ age, col = gray(0.5), cex = 1.2, data = base, xlab = 'Idade (anos)',
     ylab = 'Índice de massa corporal')

########################################################################
### Ajuste do modelo para construção das curvas centílicas. Vamos usar a
### função lms do pacote gamlss. Antes de mais nada, vamos acessar a 
### documentação da função.

help(lms)
ajuste1 <- lms(bmi, age, data = base, trans.x = TRUE, k = 2)
### trans.x = TRUE indica que transformações (do tipo potência) para age
### serão consideradas. k = 2 é o parâmetro de penalização para seleção do
### modelo via GAIC.

ajuste1$family
### A distribuição que produziu melhor ajuste foi a Box-Cox Power Exponential.

demo.BCPE()
### Explorando a distribuição BCPE através de sua demo.

ajuste1$power
### A potência selecionada para transformação da variável explanatória
### (age) foi p=0.936.

dfs <- edfAll(ajuste1)
dfs
### Graus de liberdade usados para ajuste de cada um dos quatro parâmetros.

par(mfrow = c(2,2))
term.plot(ajuste1, what = 'mu')
term.plot(ajuste1, what = 'sigma')
term.plot(ajuste1, what = 'nu')
term.plot(ajuste1, what = 'tau')
### O parâmetro de curtose (tau) é aquele que apresenta relação mais linear
### com a idade, requerendo menor número de graus de liberdade para ajuste.

########################################################################
### Diagnóstico do ajuste. Vamos usar gráficos de resíduos e worm-plots.

plot(ajuste1)
wp(ajuste1, ylim.all = 0.5)

### Uma representação alternativa do worm plot divide os dados em faixas
### segundo os valores da variável explanatória. No caso, vamos dividir
### o intervalo de valores da variável age em nove intervalos:

wp(ajuste1, xvar = base$age, n.inter = 9)
### Para todos os intervalos os resíduos, em sua maioria, se encontram
### no interior das bandas de 95%, indicando bom ajuste.

########################################################################
### Plotagem das curvas centílicas.

### Podemos extrair os centis estimados usando a função centiles.
centiles(ajuste1, xvar = base$age, xlab = 'Age', ylab = 'IMC', lwd = 2,
         main = 'Curvas centílicas', col.centiles = c(1,2,3,4,5,4,3,2,1))

### Se desejado, podemos especificar um conjunto de centis de interesse.
centiles(ajuste1, xvar = base$age, xlab = 'Age', ylab = 'IMC', lwd = 2,
         cent = c(20,40,60,80), col.centiles = c(1,2,2,1), 
         main = 'Curvas centílicas')

### Visualização alternativa:
centiles.fan(ajuste1, xvar = base$age, xlab = 'Age', ylab = 'IMC',
             colors = 'topo', main = 'Curvas centílicas')

### Plotando a distribuição condicional da resposta para alguns valores
### específicos da variável age.
plotSimpleGamlss(bmi, age, ajuste1, data = base, x.val = c(1,5,10,15,20),
                 xlim = c(-3,23), val = 10)

########################################################################
### Predição.
### Vamos criar um vetor de novas observações para predição.

n_age <- c(1, 3, 10)

### Método 1: Predição de centis para os valores informados de x.

centiles.pred(ajuste1, xname = "age", xvalues = n_age)
### Centis estimados para as três idades declaradas em n_age. Podemos definir
### algum outro conjunto de centis para predição. Por exemplo:

centiles.pred(ajuste1, xname = "age", xvalues = n_age, cent = c(20,40,60,80))


### Método 2: Centis de y para dados x e centis para os escores z.
### Neste caso, os centis são criados a partir dos escores normais, e não
### nas porcentagens.

z <- c(-3, -2, -1, 0, 1, 2, 3)
### Escores z.

### Podemos calcular as porcentagens associadas a cada escore z usando a
### função distribuição acumulada da Normal:
round(100*pNO(z), 3)

centiles.pred(ajuste1, xname = "age", xvalues = n_age, type = "standard-centiles", dev = z)


### Método 3: Escores z para dados y e x.
### vamos considerar as seguintes idades e índices de massa corporal de 4 indivíduos:

n_age <- c(2, 5, 10, 15)
n_bmi <- c(20, 18, 25, 14)
cbind(n_age, n_bmi)

centiles.pred(ajuste1, xname = "age", xval = c(2, 5, 10, 15), yval = c(20, 18, 25, 14),
              type = "z-scores")
### Os indivíduos 1 e 3 têm índice de massa corporal muito elevado para a 
### idade (escore z acima de dois); o indivíduo 4, por sua vez, IMC muito
### baixo para a idade.