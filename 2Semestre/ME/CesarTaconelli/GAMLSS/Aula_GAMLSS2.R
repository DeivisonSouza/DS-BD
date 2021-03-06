#########################################################################
### Estima��o de centis e curvas cent�licas. Vamos usar a base de dados
### dbbmi, do pacote gamlss
require(gamlss) 
require(gamlss.demo)
require(gamlss.util)

options(device = 'x11')
data(dbbmi) 
help("dbbmi")
### Dados sobre �ndice de massa corporal (bmi) e idade (age) de uma popula��o
### de 7294 alem�es do sexo masculino, com idade inferior a 22 anos. O interesse
### � construir as curvas cent�licas de bmi versus idade.

### Gr�fico de dispers�o para a base completa
plot(bmi ~ age, col = gray(0.5), cex = 1.2, data = dbbmi, xlab = 'Idade (anos)',
     ylab = '�ndice de massa corporal')

### Para esta aplica��o, vamos considerar apenas uma amostra, selecionada de
### forma aleat�ria, de 1000 indiv�duos da base original.

### Sele��o dos 1000 indiv�duos que v�o compor a base para a an�lise.
set.seed(2803)

ind <- sample(1:7294, 1000, replace = FALSE)
base <- dbbmi[ind,]

### Gr�fico de dispers�o para a base selecionada
plot(bmi ~ age, col = gray(0.5), cex = 1.2, data = base, xlab = 'Idade (anos)',
     ylab = '�ndice de massa corporal')

########################################################################
### Ajuste do modelo para constru��o das curvas cent�licas. Vamos usar a
### fun��o lms do pacote gamlss. Antes de mais nada, vamos acessar a 
### documenta��o da fun��o.

help(lms)
ajuste1 <- lms(bmi, age, data = base, trans.x = TRUE, k = 2)
### trans.x = TRUE indica que transforma��es (do tipo pot�ncia) para age
### ser�o consideradas. k = 2 � o par�metro de penaliza��o para sele��o do
### modelo via GAIC.

ajuste1$family
### A distribui��o que produziu melhor ajuste foi a Box-Cox Power Exponential.

demo.BCPE()
### Explorando a distribui��o BCPE atrav�s de sua demo.

ajuste1$power
### A pot�ncia selecionada para transforma��o da vari�vel explanat�ria
### (age) foi p=0.936.

dfs <- edfAll(ajuste1)
dfs
### Graus de liberdade usados para ajuste de cada um dos quatro par�metros.

par(mfrow = c(2,2))
term.plot(ajuste1, what = 'mu')
term.plot(ajuste1, what = 'sigma')
term.plot(ajuste1, what = 'nu')
term.plot(ajuste1, what = 'tau')
### O par�metro de curtose (tau) � aquele que apresenta rela��o mais linear
### com a idade, requerendo menor n�mero de graus de liberdade para ajuste.

########################################################################
### Diagn�stico do ajuste. Vamos usar gr�ficos de res�duos e worm-plots.

plot(ajuste1)
wp(ajuste1, ylim.all = 0.5)

### Uma representa��o alternativa do worm plot divide os dados em faixas
### segundo os valores da vari�vel explanat�ria. No caso, vamos dividir
### o intervalo de valores da vari�vel age em nove intervalos:

wp(ajuste1, xvar = base$age, n.inter = 9)
### Para todos os intervalos os res�duos, em sua maioria, se encontram
### no interior das bandas de 95%, indicando bom ajuste.

########################################################################
### Plotagem das curvas cent�licas.

### Podemos extrair os centis estimados usando a fun��o centiles.
centiles(ajuste1, xvar = base$age, xlab = 'Age', ylab = 'IMC', lwd = 2,
         main = 'Curvas cent�licas', col.centiles = c(1,2,3,4,5,4,3,2,1))

### Se desejado, podemos especificar um conjunto de centis de interesse.
centiles(ajuste1, xvar = base$age, xlab = 'Age', ylab = 'IMC', lwd = 2,
         cent = c(20,40,60,80), col.centiles = c(1,2,2,1), 
         main = 'Curvas cent�licas')

### Visualiza��o alternativa:
centiles.fan(ajuste1, xvar = base$age, xlab = 'Age', ylab = 'IMC',
             colors = 'topo', main = 'Curvas cent�licas')

### Plotando a distribui��o condicional da resposta para alguns valores
### espec�ficos da vari�vel age.
plotSimpleGamlss(bmi, age, ajuste1, data = base, x.val = c(1,5,10,15,20),
                 xlim = c(-3,23), val = 10)

########################################################################
### Predi��o.
### Vamos criar um vetor de novas observa��es para predi��o.

n_age <- c(1, 3, 10)

### M�todo 1: Predi��o de centis para os valores informados de x.

centiles.pred(ajuste1, xname = "age", xvalues = n_age)
### Centis estimados para as tr�s idades declaradas em n_age. Podemos definir
### algum outro conjunto de centis para predi��o. Por exemplo:

centiles.pred(ajuste1, xname = "age", xvalues = n_age, cent = c(20,40,60,80))


### M�todo 2: Centis de y para dados x e centis para os escores z.
### Neste caso, os centis s�o criados a partir dos escores normais, e n�o
### nas porcentagens.

z <- c(-3, -2, -1, 0, 1, 2, 3)
### Escores z.

### Podemos calcular as porcentagens associadas a cada escore z usando a
### fun��o distribui��o acumulada da Normal:
round(100*pNO(z), 3)

centiles.pred(ajuste1, xname = "age", xvalues = n_age, type = "standard-centiles", dev = z)


### M�todo 3: Escores z para dados y e x.
### vamos considerar as seguintes idades e �ndices de massa corporal de 4 indiv�duos:

n_age <- c(2, 5, 10, 15)
n_bmi <- c(20, 18, 25, 14)
cbind(n_age, n_bmi)

centiles.pred(ajuste1, xname = "age", xval = c(2, 5, 10, 15), yval = c(20, 18, 25, 14),
              type = "z-scores")
### Os indiv�duos 1 e 3 t�m �ndice de massa corporal muito elevado para a 
### idade (escore z acima de dois); o indiv�duo 4, por sua vez, IMC muito
### baixo para a idade.