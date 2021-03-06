### Este exemplo tem como objetivo esclarecer o significado hist�rico do
### termo regress�o, baseado em dados de altura de pais e filhos coletados
### por Francis Galton, s�culo XIX.

require(HistData)
data("GaltonFamilies")
help("GaltonFamilies")
attach(GaltonFamilies)

x11()
library(car)
scatterplot(childHeight ~ midparentHeight | gender, 
            xlab = 'Altura m�dia dos pais', ylab = 'Altura do(a) filho(a)', smooth = FALSE)
### Gr�fico de dispers�o da altura dos filhos vs altura m�dia dos pais
### segundo o sexo do filho.

childHeight_2 <- ifelse(gender == 'female', 1.08 * GaltonFamilies$childHeight, 
                        GaltonFamilies$childHeight)
### A multiplica��o da altura das mulheres por 1.08 tem por objetivo 
### compensar a menor altura das mulheres (em geral).

scatterplot(childHeight_2 ~ midparentHeight | gender, 
            xlab = 'Altura m�dia dos pais',
            ylab = 'Altura do(a) filho(a)',
			smooth = FALSE)
### Gr�fico de dispers�o da altura dos filhos (corrigida) vs altura m�dia dos pais
### segundo o sexo.

scatterplot(childHeight_2 ~ midparentHeight, 
            ylab = 'Altura do(a) filho(a)',
			smooth = FALSE)
### Gr�fico de dispers�o da altura dos filhos (corrigida) vs altura m�dia dos pais
### (desconsiderando o sexo).


ajuste <- lm(childHeight_2 ~ midparentHeight)
### ajuste armazena o resultado da regress�o linear para o par de vari�veis,
### considerando a altura (corrigida) dos filhos como a vari�vel resposta.

coefficients(ajuste)
### Estimativas de m�nimos quadrados para os par�metros da reta de regress�o.

### Podemos expressar o modelo de regress�o envolvendo um par de vari�veis,
### de maneira alternativa, explicitando a correla��o linear entre as vari�veis.
### Nesse caso, as estimativas de m�nimos quadrados podem ser obtidas 
### usando as seguintes linhas de comando:

r <- cor(childHeight_2, midparentHeight)
sdx <- sd(midparentHeight)
sdy <- sd(childHeight_2)
mx <- mean(midparentHeight)
my <- mean(childHeight_2)

beta1 <- r * sdy/sdx
beta0 <- my - beta1 * mx

plot(childHeight_2 ~ midparentHeight, pch = 20, xlab = 'Altura m�dia dos pais', ylab = 'Altura do(a) filho(a)')
abline(a = beta0, b = beta1, lwd = 2, col = 'red')

### E se a correla��o fosse perfeita (igual a 1)?
r <- 1
beta1_r1 <- r * sdy/sdx
beta0_r1 <- my - beta1_r1 * mx
abline(a = beta0_r1, b = beta1_r1, lwd = 2, col = 'red', lty = 2)

### E se n�o houvesse correla��o alguma?

abline(h = mean(childHeight_2), col = 'blue', lwd = 2)
legend(x = 'topleft', legend = c('Ajuste', 'r=1', 'r=0'), lty = c(1,2,1), 
       col = c('red', 'red', 'blue'), lwd = 2)

### Pais com altura acima da m�dia tendem a ter filhos com altura acima da m�dia,
### mas n�o t�o altos quanto eles;
### Pais com altura abaixo da m�dia tendem a ter filhos com altura abaixo da m�dia,
### mas n�o t�o baixos quanto eles.
### Galton definiu esse fen�meno usando a express�o "regress�o � m�dia".