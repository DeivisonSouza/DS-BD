require(car)
# require(scatterplot3d)
# require(lsmeans)

### Exemplo - dados referentes a escolaridade, renda e prestígio de 45 profissões (EUA-1950).
help("Duncan") ### Consultando a documentação da base.
head(Duncan)

### O objetivo é propor um modelo de regressão para prestigio em função de escolaridade e renda.

summary(Duncan)

scatterplotMatrix( ~ prestige + income + education, data = Duncan, smooth = F, pch = 20, lwd = 2, cex = 1.5)
### Os gráficos indicam que escolaridade e renda estão (linearmente) 
### relacionados ao prestígio da profissão. Em ambos os casos a tendência
### é positiva (maior renda, maior prestigio; maior escolaridade, maior prestigio)

ajuste=lm(prestige ~ income + education, data = Duncan) 
### Ajuste do modelo de regressão linear múltipla para o prestigio da 
### profissão em função de escolaridade e renda.

print(ajuste) ### Estimativas de mínimos quadrados para os parâmetros do modelo.

summary(ajuste) 
### Repare que os coeficientes associados a renda e educação são positivos, 
### indicando que o prestigio aumenta quanto maiores os percentuais de 
### "alta renda" e "alta escolaridade" da profissão.

### Algumas interpretações:
### O intercepto não tem uma interpretação prática, pois percentuais iguais 
### a zero para escolaridade e renda não fazem parte do escopo do estudo.

### Estima-se, em média, um aumento de 0,545 no grau (percentual) de prestígio 
### da profissão para 1% a mais de profissionais com ensino médio, mantendo a renda fixa.

### Estima-se, em média, um aumento de 0,598 no grau (percentual) de prestígio 
### da profissão para 1% a mais de profissionais que recebem mais de $3500, mantendo a escolaridade fixa.

fitted(ajuste) ### Prestigio ajustado pelo modelo para as 45 profissões.
plot(Duncan$prestige, fitted(ajuste), xlab='Prestígio observado', ylab='Prestígio ajustado', pch = 20)
### Gráfico dos valores observados versus valores ajustados pelo modelo.

resid(ajuste) ### Resíduos produzidos pelo modelo ajustado.
plot(fitted(ajuste), resid(ajuste), xlab = 'Valores ajustados', ylab = 'Resíduos', pch = 20)
### Gráfico de resíduos vs valores ajustados.



### E se ajustássemos uma regressão linear simples para cada variável explicativa?

ajuste_srs1 <- lm(prestige ~ income, data = Duncan)
summary(ajuste_srs1)

ajuste_srs2 <- lm(prestige ~ education, data = Duncan)
summary(ajuste_srs2)

vcov(ajuste) ### Matriz de variâncias e covariâncias estimada para os estimadores dos betas.

confint(ajuste) ### Intervalos de confiança (95%) para os parâmetros do modelo.
predict(ajuste, newdata = data.frame(income = 50,education = 50), interval = 'confidence') 
### Intervalo de confiança (95%) para o prestigio esperado de profissões para as quais
### 50% dos profissionais tem ensino médio e 50% tem renda superior a $3500.


predict(ajuste,newdata=data.frame(income=50,education=50),interval='prediction') 
### Intervalo de predição (95%) para o grau de prestigio de uma "nova" profissão para a qual
### 50% dos profissionais tem ensino médio e 50% tem renda superior a $3500.


### Observem que, embora as conclusões sejam semelhantes, as estimativas
### são bem diferentes. No modelo de regressão linear múltipla estamos
### avaliando o efeito da educação no prestígio controlando o efeito
### de renda (o mesmo vale para o efeito de renda). Nos modelos de regressão
### linear simples estamos avaliando o efeito de cada variáveil explicativa
### não controlando o efeito da segunda variável. Como renda e escolaridade
### estão correlacionadas, o efeito das variáveis muda se ajustarmos ou não
### o efeito da outra variável.

### Um exemplo mais drástico. Vamos simular dados para duas variáveis explicativas
### correlacionadas e a resposta como função das duas variáveis.

set.seed(98)
x1 <- runif(100)
x2 <- x1 + rnorm(100, 0, 1.5) ### x2 = x1 + e2, e2 ~ Normal(0, 1.5^2)
erro <- rnorm(100, 0, 0.05) ### epsilon ~ Normal(0, 0.05^2) (erro)
y <- 0.1*x1 + 0.005*x2 + erro ### simulando valores para y.

ajuste_rlm <- lm(y ~ x1 + x2)
summary(ajuste_rlm)
### Observe que x2 não apresenta efeito significativo ao ajustarmos y também por 
### x1.

ajuste_rls <- lm(y ~ x2)
summary(ajuste_rls)
### Quando não ajustamos o efeito de x1, x2 passa a apresentar efeito significativo
### no modelo.

