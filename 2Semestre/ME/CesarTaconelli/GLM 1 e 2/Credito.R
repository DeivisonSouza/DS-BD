########################################################################
### Regress?o para dados bin?rios - exemplo 2.

### Dados sobre concess?o de cr?dito. O objetivo ? modelar a vari?vel 
### resposta, que indica se o ind?viduo deixou de pagar o cr?dito (Yes - 1)
### ou se n?o deixou de pagar (No - 0) em fun??o de covari?veis. Para
### maiores detalhes, consultar a documenta??o.

library(ISLR)
data("Default")
help("Default")
summary(Default)

### Podemos observar que a frequ?ncia de devedores ? bastante inferior ? de
### n?o devedores. Adicionalmente, as duas vari?veis num?ricas apresentam
### valores bastante elevados, o que pode ser inconveniente para a an?lise.
### Vamos dividir ambas por 1000.

Default$balance <- Default$balance/1000
Default$income <- Default$income/1000

### Agora, uma an?lise explorat?ria dos dados.
tabela <- with(Default, table(default , student))
round(prop.table(tabela, 2), 3)
### A frequ?ncia de devedores ? maior entre os estudantes.

x11(height = 10, width = 10)
par(las = 1, mar = c(5,4.5,2,2))
plot(ifelse(Default$default == 'Yes',1 ,0) ~ balance, data = Default, pch = "|",
     col = 'blue', xlab = 'Balan?o', ylab = 'D?vida', cex = 1.5, cex.lab = 1.5,
     cex.axis = 1.5)
### Aparentemente, quanto maior o valor remanescente no cart?o de cr?dito, 
### maior o risco de n?o pagamento.

plot(ifelse(Default$default == 'Yes',1 ,0) ~ income, data = Default, pch = "|",
     col = 'blue', xlab = 'Renda', ylab = 'D?vida', cex = 1.5, cex.lab = 1.5,
     cex.axis = 1.5)
### Com rela??o ? renda, n?o se tem alguma evid?ncia de associa??o com a
### propens?o ao n?o pagamento.

### Na pr?xima etapa da an?lise vamos dividir a base de dados em duas novas
### bases: a primeira para o ajuste do modelo (com 70% das observa??es) e a 
### segunda para valida??o do modelo (com 30% das observa??es). Como a base
### ? bastante desequilibrada quanto ? frequ?ncia de pagadores e n?o pagadores,
### vamos garantir que os dois grupos estejam representados com as mesmas 
### porcentagens nas duas amostras.

i_def <- which(Default$default == 'Yes')
i_ndef <- which(Default$default == 'No')
### Os vetores i_def e i_ndef armazenam as posi??es na base (linhas) em que
### se tem n?o pagadores e pagadores, respectivamente.

ndef <- length(i_def); ndef
nndef <- length(i_ndef); nndef
### ndef e nndef correspondem ao total de n?o pagadores e pagadores na base.

ordem_def <- sample(1:ndef)
ordem_ndef <- sample(1:nndef)
### Sequ?ncias aleat?rias de n?meros que ser?o usadas para separar os dados
### nas amostras de ajuste e de valida??o.

ndef*0.7
nndef*0.7
### Quantidade de observa??es de devedores e n?o devedores que ser?o selecionadas
### para a base de ajuste.

baseaj <- Default[c(ordem_def[1:233],
                    ordem_ndef[1:6767]),]
### baseaj ? a base para ajuste.
summary(baseaj)

basepred <- Default[c(ordem_def[234:ndef],
                      ordem_ndef[6768:nndef]),]
### basepred ? a base para valida??o.
summary(basepred)

### Ajuste do modelo de regress?o log?stica
ajuste <- glm(default ~ ., data = baseaj, family = binomial(link = 'logit'))
summary(ajuste)
par(mfrow = c(2,2), cex = 1)
plot(ajuste)
### Os gr?ficos de res?duos t?m comportamento bastante at?pico, mas caracter?stico
### da an?lise de dados bin?rios, devido aos empates. Para um diagn?stico mais
### adequado, vamos usar os res?duos quant?licos aleatorizados, dispon?veis
### no pacote statmod, e os gr?ficos meio-normais com envelope simulado,
### dispon?veis no pacote hnp.

library(statmod)
library(hnp)

(residuos <- qres.binom(ajuste))
ajustados <- predict(ajuste)
x11(width = 12, height = 10)
par(las = 1, mar = c(5,4.5,2,2), mfrow = c(1,2), cex = 1.4)
plot(residuos ~ ajustados, col = 'blue', xlab = 'Valores ajustados', ylab = 'Res?duos')
lines(lowess(residuos ~ ajustados), col = 'red', lwd = 2)
qqnorm(residuos, col = 'blue', main = '')
qqline(residuos, lty = 2)

### Os res?duos apresentam dispers?o aleat?ria, vari?ncia constante e 
### distribui??o aproximadamente normal. O modelo parece estar bem ajustado.

x11(width = 10, height = 10)
par(las = 1, mar = c(5,4.5,2,2), cex = 1.4)
hnp(ajuste)
### O padr?o para um ajuste adequado ? os res?duos (pontos) dispostos no
### interior do envelope (linhas) simulado. Novamente temos um indicativo
### de que o modelo est? bem ajustado.

### Como o efeito de renda n?o se mostrou significativo, vamos remov?-lo
### do modelo.

ajuste2 <- update(ajuste, ~.-income)
summary(ajuste2)
anova(ajuste2, ajuste, test = 'Chisq')
### O resultado da aplica??o da fun??o anova, neste caso, ? o teste da raz?o
### de verossimilhan?a do efeito de renda.   

### Vamos testar agora poss?vel efeito de intera??o entre as duas covari?veis
### remanescentes no modelo.

ajuste3 <- update(ajuste2, ~. +student:balance)
summary(ajuste3)
### O efeito de intera??o n?o ? significativo. Vamos permanecer com o modelo
### aditivo.

### Adicionalmente, vamos investigar poss?vel efeito quadr?tico de balan?o
### (embora a an?lise de res?duos n?o tenha indicado algo do tipo).

ajuste4 <- update(ajuste2, ~. + I(balance^2))
summary(ajuste4)

### Como esperado, a inclus?o do termo quadr?tico n?o tem efeito significativo.

### Agora, vamos usar o modelo ajustado para fins de predi??o. Antes de 
### abordar a base de valida??o, vamos fazer predi??o para algus dados
### adicionais.

novos_dados <- data.frame(student = rep(c('Yes', 'No'), times = 3),
                          balance = c(0.5, 0.5, 1, 1, 2, 2))
### Base para predi??o.

predict(ajuste2, newdata = novos_dados)
### Predi??o na escala do preditor (link)

predict(ajuste2, newdata = novos_dados, type = 'response')
### Predi??o na escala da resposta (probabilidade, inversa do link)

### Agora, vamos fazer intervalos de confian?a (95%) para a probabilidade
(p_link <- predict(ajuste2, newdata = novos_dados, se.fit = TRUE))
### Predi??es na escala do link com os erros padr?es associados.

ic_link <- cbind(p_link$fit - 1.96 * p_link$se.fit, p_link$fit + 1.96 * p_link$se.fit)
ic_link
### Intervalos de confian?a na escala do link.

exp(ic_link)/(exp(ic_link) + 1)
### Intervalos de confian?a na escala da probabilidade de resposta.

### na sequ?ncia, vamos retomar a amostra de valida??o para avaliar a 
### capacidade preditiva do modelo.

predicoes <- predict(ajuste2, newdata = basepred, type = 'response')
### Extraindo as predi??es na escala da (probabilidade de) resposta

hist(predicoes)

### Vamos ver como ficaria o resultado da predi??o se adot?ssemos o ponto
### de corte p=0.5 para predi??o, isto ?, classificando como n?o pagadores
### os indiv?duos com probabilidade estimada superior a 0.5 e como pagadores
### aqueles com probabilidade inferior a 0.5.

tab_pred <- table(ifelse(predicoes < 0.5, 'Pred_No', 'Pred_Yes'), basepred$default)
tab_pred
prop.table(tab_pred, 2)

### A regra de classifica??o baseada no ponto de corte p = 0.5 tem elevada
### especificidade (0.996), mas sensibilidade muito baixa (0.308). Neste
### problema, em particular, sensibilidade (identificar n?o pagadores)
### deve ser mais importante que especificidade (identificar pagadores).
### Nesse sentido, precisamos explorar adequadamente a capacidade preditiva
### do modelo e buscar regras de classifica??o alternativas.

r1 <- roc(basepred$default, predicoes, plot=TRUE, ci=TRUE, ci.sp = TRUE)
r1

??roc

### A ?rea sob a curva ROC ? uma medida de qualidade preditiva do modelo.
### Valores pr?ximos de 1 indicam modelos com elevada capacidade preditiva,
### enquanto valores pr?ximos de 0.5 indicam modelos cujas predi??es s?o
### realizadas ao acaso.

r1$sensitivities
plot(r1, print.thres = c(0.001, 0.005, 0.01, 0.02, 0.03, 0.04, seq(0.05,0.95,0.05)),
     print.thres.pattern.cex = 0.8)
### Curva ROC. O valor que aparece fora dos par?ntesis ? o ponto de corte.
### No interior temos a sensibilidade e a especificidade correspondentes,
### respectivamente. Pontos de corte posicionados no canto superior esquerdo
### s?o aqueles que combinam maior sensibilidade e especificidade.

coords(r1, x = 0.01, ret = c("sensitivity", "specificity", "accuracy"))
### Sensibilidade, especificidade e acur?cia para o ponto de corte p = 0.01.

coords(r1, x = 0.05, ret = c("sensitivity", "specificity", "accuracy"))
### Sensibilidade, especificidade e acur?cia para o ponto de corte p = 0.05.

### Agora, vamos identificar a melhor regra de decis?o (ponto de corte)
### associada a diferentes custos de m?classifica??o. No argumento
### "best.weights=c(a, b)", em que a representa o custo de um falso negativo
### relativo a um falso positivo e "b" a preval?ncia (propor??o de sucessos)
### na popula??o.

### Vamos lembrar que, neste exemplo, falso negativo corresponde a classificar
### como pagador um n?o pagador. A preval?ncia de maus pagadores n?s vamos 
### fixar em 0.033, que ? a preval?ncia verificada na base.

coords(r1, x = "best", best.method = "youden", best.weights=c(1, 0.033))
### Custos iguais.

coords(r1, x = "best", best.method = "youden", best.weights=c(2, 0.033))
### O custo do falso negativo ? duas vezes o do falso positivo.

coords(r1, x = "best", best.method = "youden", best.weights=c(5, 0.033))
### O custo do falso negativo ? cinco vezes o do falso positivo.

coords(r1, x = "best", best.method = "youden", best.weights=c(20, 0.033))
### O custo do falso negativo ? vinte vezes o do falso positivo.