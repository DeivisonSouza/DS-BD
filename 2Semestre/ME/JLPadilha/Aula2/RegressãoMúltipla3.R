### Notas de alunos de uma faculdade em quatro exames do processo seletivo
### (V1, V2, V3 e V4) e a nota final no exame de conhecimentos gerais ao
### t�rmino do primeiro semestre (y).

require(multcomp)

notas <- read.csv2('https://docs.ufpr.br/~taconeli/CE07118/Notas.csv') ### Importando os dados
# notas <- read.csv2('Notas.csv')
pairs(notas, pch = 20, cex = 1.2)

ajuste <- lm(y ~ ., data = notas)
### Ajuste do modelo de regress�o linear m�ltipla.

summary(ajuste)

### Vamos testar a hip�tese H0: beta2 = beta4 E beta3 = 2*beta2.
### Podemos escrever as duas conjecturas que comp�em H0, de maneira equivalente,
### por beta2 - beta4 = 0 e -2*beta2 + beta3 = 0. As matrizes de constantes 
### correspondentes ficam dadas por:

L <- matrix(c(0,0,1,0,-1,0,0,-2,1,0), nrow = 2, byrow = TRUE)
c0 <- matrix(c(0,0), nrow = 2)

q <- 2 ### N�mero de restri��es definidas H0
X <- model.matrix(ajuste)
beta <- coef(ajuste)
sigma2 <- summary(ajuste)$sigma^2
n <- nrow(X)
p <- ncol(X)

### C�lculo da estat�stica F:
F_calc <- (t(L %*% beta - c0) %*% solve(L %*% solve(t(X) %*% X) %*% t(L)) %*% (L %*% beta - c0))/(q*sigma2)

### Valor da tabela F de refer�ncia, considerando 5% de signific�ncia:
qf(0.95, df1 = q, df2 = n-p)

### Logo, n�o se tem evid�ncias ao n�vel de 5% para rejeitar o par de contrastes 
### definidas em H0.

pf(F_calc, df1 = q, df2 = n-p, lower.tail = FALSE)
### Valor p do teste.

### Agora vamos obter as estimativas pontuais e IC's 95% para cada um dos contrastes.

### Para beta2 - beta4:
l <- matrix(L[1,], ncol = 1)
t(l) %*%beta ### Estimativa pontual.
c(t(l) %*%beta + qt(0.025, n-p) * sqrt(sigma2 * t(l) %*% solve(t(X) %*% X) %*% l),
  t(l) %*%beta - qt(0.025, n-p) * sqrt(sigma2 * t(l) %*% solve(t(X) %*% X) %*% l))

### Para beta3 - 2*beta2:
l <- matrix(L[2,], ncol = 1)
t(l) %*%beta ### Estimativa pontual.
c(t(l) %*%beta + qt(0.025, n-p) * sqrt(sigma2 * t(l) %*% solve(t(X) %*% X) %*% l),
  t(l) %*%beta - qt(0.025, n-p) * sqrt(sigma2 * t(l) %*% solve(t(X) %*% X) %*% l))


### Agora vamos explorar a fun��o glht do pacote multcomp para executar testes 
### de hip�teses e obter intervalos de confian�a para combina��es lineares dos
### par�metros.

help(glht)

L <- rbind('V2 - V4' = c(0, 0, 1, 0, -1),
           '-2*V2 + V3' = c(0, 0, -2, 1, 0))
### Especificando os contrastes.

g1 <- glht(ajuste, linfct = L)
g1
### Estimativas pontuais.

summary(g1, test = Ftest())
### Testando a hip�tese nula para o par de contrstes.

summary(g1, adjusted(type = "none"))
### Estimativas, erros padr�es e testes de hip�teses para cada um dos dois
### contrastes. 

summary(g1)
### Semelhante � sa�da anterior, mas neste caso os p-valores s�o corrigidos
### devido aos m�ltiplos testes (dois contrastes avaliados simultaneamente).

confint(g1, calpha = univariate_calpha())
### Intervalos de confian�a n�o corrigidos (cada um com 95% de confian�a)

confint(g1)
### Intervalos de confian�a corrigidos (95% de confian�a para o par de intervalos).



### Agora, vamos testar outras hip�teses lineares usando a fun��o glht.

### H0: beta1 = 0.
L <- rbind('V1' = c(0, 1, 0, 0, 0))
g2 <- glht(ajuste, linfct = L)
summary(g2)

### H0: beta2 = 0.
L <- rbind('V2' = c(0, 0, 1, 0, 0))
g3 <- glht(ajuste, linfct = L)
summary(g3)

### H0: beta1 = beta2 = beta3 = beta4 = 0.
L <- rbind('V1' = c(0, 1, 0, 0, 0),
           'V2' = c(0, 0, 1, 0, 0),
           'V3' = c(0, 0, 0, 1, 0),
           'V4' = c(0, 0, 0, 0, 1))
g4 <- glht(ajuste, linfct = L)
summary(g4, test = Ftest())
summary(g4, adjusted(type = "none"))


### H0: beta2 - (1/2) * (beta1 + beta4) = 0
### As notas nos exames V1 e V4 contribuem, em m�dia, igualmente ao escore em V2
L <- rbind('cte' = c(0, -0.5, 1, 0, -0.5))
g5 <- glht(ajuste, linfct = L)
summary(g5)

### Agora, vamos definir quatro contrastes distintos a serem testados:
### c1: beta2 = beta3 (beta2 - beta3 = 0);
### c2: beta3 = 2*beta1 (beta3 - 2*beta1 = 0);
### c3: beta4 = (1/3) * (beta1 + beta2 + beta3) (beta4 - (1/3) * (beta1 + beta2 + beta3) = 0);
### c4: beta1 = 0.

L <- rbind('c1' = c(0, 0, 1, -1, 0),
           'c2' = c(0, -2, 0, 1, 0),
           'c3' = c(0, -1/3, -1/3, -1/3, 1),
           'c4' = c(0, 1, 0, 0, 0))
g6 <- glht(ajuste, linfct = L)
summary(g6, adjusted(type = "none"))
### Resultados n�o ajustados.

summary(g6)
### P-valores ajustados pelos fato de avaliarmos conjuntamente quatro contrastes.


confint(g6, calpha = univariate_calpha()) ### IC's 95% (individualmente)
confint(g6) ### IC's 95% (no conjunto).

### Para testar a hip�tese H0: beta1 = beta2 = beta4:
L <- rbind('V1 - V2' = c(0, 1, -1, 0, 0),
           'V1 - V4' = c(0, 1, 0, 0, -1),
           'V2 - V4' = c(0, 0, 1, 0, -1))
g7 <- glht(ajuste, linfct = L)
summary(g7, test = Ftest())



