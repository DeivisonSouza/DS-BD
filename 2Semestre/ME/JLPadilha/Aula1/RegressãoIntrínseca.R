### Exemplo 1 - dados sobre velocidade de moinhos de vento (wind) e energia
### gerada (DC)

### Entrando com os dados
wind <- c(5.00, 6.00, 3.40, 2.70, 10.0, 9.70, 9.55, 3.05, 8.15, 6.20, 2.90, 6.35, 4.60,
          5.80, 7.40, 3.60, 7.85, 8.80, 7.00, 5.45, 9.10, 10.2, 4.10, 3.95, 2.45)

DC <- c(1.582, 1.822, 1.057, 0.500, 2.236, 2.386, 2.294, 0.558, 2.166, 1.866,
        0.653, 1.930, 1.562, 1.737, 2.088, 1.137, 2.179, 2.112, 1.800, 1.501,
        2.303, 2.310, 1.194, 1.144, 0.123)

x11()

plot(wind, DC, pch = 20, cex = 1.4, las = 1)

### Modelo 1 - regress�o linear simples.
ajuste1 <- lm(DC ~ wind)
abline(coefficients(ajuste1), col = 'red', lwd = 2)
summary(ajuste1)

### Modelo 2 - regress�o polinomial (quadr�tico)
ajuste2 <- lm(DC ~ wind + I(wind^2))
wind_grid <- data.frame(wind = seq(min(wind), max(wind), length.out = 100))
pred_m2 <- predict(ajuste2, newdata = wind_grid)
lines(wind_grid$wind, pred_m2, lwd = 2, col = 'green')
summary(ajuste2)

### Modelo 3 - regress�o intr�nseca: y = beta_0 + beta_1 (1/x) + epsilon.
ajuste3 <- lm(DC ~ I(1/wind))
pred_m3 <- predict(ajuste3, newdata = wind_grid)
lines(wind_grid$wind, pred_m3, lwd = 2, col = 'blue')
summary(ajuste3)

########################################################################
########################################################################
########################################################################
### Exemplo 2 - Dados de press�o do vapor para �gua sob diferentes temperaturas.

temp <- c(273,  283, 293, 303, 313,	323, 333, 343, 353, 363, 373)
pres <- c(4.6, 9.2,	17.5, 31.8,	55.3, 92.5,	149.4, 233.7, 355.1, 525.8,	760.0)

plot(temp, pres, pch=20, cex = 1.4, las = 1) 

### A rela��o entre temperatura e press�o � claramente n�o linear.
### Da literatura da Qu�mica sabe-se que ln(p) � proporcional a -1/t
### (equa��o de Clausius-Claperyon). 

### Assim, vamos ajustar o seguinte modelo: ln(y) = \beta_0 + \beta_1 (1/t) + epsilon.
### Na escala original ter�amos y = exp{\beta_0 + \beta_1 (1/t)} * epsilon',
### em que epsilon' = exp{epsilon}. Trata-se, portanto, de um modelo com 
### erros multiplicativos (e n�o aditivos).


log_p <- log(pres)
inv_t <- 1/temp

ajuste <- lm(log_p ~ inv_t)
summary(ajuste)
### Com base nas estimativas obtidas, a express�o do modelo ajustado fica
### dada por \hat{pres} = exp{20.61 - 5201 * (1/temp)}.

anova(ajuste)
confint(ajuste)
betas <- coefficients(ajuste)

p_pred <- function(temp, beta) exp(beta[1]+betas[2]/temp)
curve(p_pred(x, beta = betas), from = min(temp), to = max(temp), add = T, lwd = 2)


########################################################################
########################################################################
########################################################################
### Exerc�cio - Dados s�cio econ�micos do censo do Paran�.
### Vari�veis:
# Munic�pio;
# Urbaniza��o: Grau de urbaniza��o (%);
# Idosos: �ndice de idosos (%);
# P60: Probabilidade de sobreviv�ncia at� 60 anos;
# Renda: Renda m�dia domiciliar;
# Analfabetismo: Taxa de analfabetismo.

dados <- read.csv2('https://docs.ufpr.br/~taconeli/CE07118/parana.csv') ### Importa��o dos dados.
names(dados)
with(dados, plot(Analfabetismo, Renda, pch = 20)) 

### Proponha um modelo de regress�o que explique adequadamente a renda m�dia 
### domiciliar em fun��o da taxa de analfabetismo. Voc� pode come�ar
### por uma regress�o linear simples e, num segundo momento, tentar alguma regress�o
### intrinsicamente linear, transformando uma ou ambas as vari�veis.

