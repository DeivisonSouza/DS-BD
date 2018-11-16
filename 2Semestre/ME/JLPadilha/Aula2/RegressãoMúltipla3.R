### Notas de alunos de uma faculdade em quatro exames do processo seletivo
### (V1, V2, V3 e V4) e a nota final no exame de conhecimentos gerais ao
### término do primeiro semestre (y).

require(multcomp)

notas <- read.csv2('https://docs.ufpr.br/~taconeli/CE07118/Notas.csv') ### Importando os dados
# notas <- read.csv2('Notas.csv')
pairs(notas, pch = 20, cex = 1.2)

ajuste <- lm(y ~ ., data = notas)
### Ajuste do modelo de regressão linear múltipla.

summary(ajuste)

### Vamos testar a hipótese H0: beta2 = beta4 E beta3 = 2*beta2.
### Podemos escrever as duas conjecturas que compõem H0, de maneira equivalente,
### por beta2 - beta4 = 0 e -2*beta2 + beta3 = 0. As matrizes de constantes 
### correspondentes ficam dadas por:

L <- matrix(c(0,0,1,0,-1,0,0,-2,1,0), nrow = 2, byrow = TRUE)
c0 <- matrix(c(0,0), nrow = 2)

q <- 2 ### Número de restrições definidas H0
X <- model.matrix(ajuste)
beta <- coef(ajuste)
sigma2 <- summary(ajuste)$sigma^2
n <- nrow(X)
p <- ncol(X)

### Cálculo da estatística F:
F_calc <- (t(L %*% beta - c0) %*% solve(L %*% solve(t(X) %*% X) %*% t(L)) %*% (L %*% beta - c0))/(q*sigma2)

### Valor da tabela F de referência, considerando 5% de significância:
qf(0.95, df1 = q, df2 = n-p)

### Logo, não se tem evidências ao nível de 5% para rejeitar o par de contrastes 
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


### Agora vamos explorar a função glht do pacote multcomp para executar testes 
### de hipóteses e obter intervalos de confiança para combinações lineares dos
### parâmetros.

help(glht)

L <- rbind('V2 - V4' = c(0, 0, 1, 0, -1),
           '-2*V2 + V3' = c(0, 0, -2, 1, 0))
### Especificando os contrastes.

g1 <- glht(ajuste, linfct = L)
g1
### Estimativas pontuais.

summary(g1, test = Ftest())
### Testando a hipótese nula para o par de contrstes.

summary(g1, adjusted(type = "none"))
### Estimativas, erros padrões e testes de hipóteses para cada um dos dois
### contrastes. 

summary(g1)
### Semelhante à saída anterior, mas neste caso os p-valores são corrigidos
### devido aos múltiplos testes (dois contrastes avaliados simultaneamente).

confint(g1, calpha = univariate_calpha())
### Intervalos de confiança não corrigidos (cada um com 95% de confiança)

confint(g1)
### Intervalos de confiança corrigidos (95% de confiança para o par de intervalos).



### Agora, vamos testar outras hipóteses lineares usando a função glht.

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
### As notas nos exames V1 e V4 contribuem, em média, igualmente ao escore em V2
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
### Resultados não ajustados.

summary(g6)
### P-valores ajustados pelos fato de avaliarmos conjuntamente quatro contrastes.


confint(g6, calpha = univariate_calpha()) ### IC's 95% (individualmente)
confint(g6) ### IC's 95% (no conjunto).

### Para testar a hipótese H0: beta1 = beta2 = beta4:
L <- rbind('V1 - V2' = c(0, 1, -1, 0, 0),
           'V1 - V4' = c(0, 1, 0, 0, -1),
           'V2 - V4' = c(0, 0, 1, 0, -1))
g7 <- glht(ajuste, linfct = L)
summary(g7, test = Ftest())



