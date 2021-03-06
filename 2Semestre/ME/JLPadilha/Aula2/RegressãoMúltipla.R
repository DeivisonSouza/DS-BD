require(car)
# require(scatterplot3d)
# require(lsmeans)

### Exemplo - dados referentes a escolaridade, renda e prest�gio de 45 profiss�es (EUA-1950).
help("Duncan") ### Consultando a documenta��o da base.
head(Duncan)

### O objetivo � propor um modelo de regress�o para prestigio em fun��o de escolaridade e renda.

summary(Duncan)

scatterplotMatrix( ~ prestige + income + education, data = Duncan, smooth = F, pch = 20, lwd = 2, cex = 1.5)
### Os gr�ficos indicam que escolaridade e renda est�o (linearmente) 
### relacionados ao prest�gio da profiss�o. Em ambos os casos a tend�ncia
### � positiva (maior renda, maior prestigio; maior escolaridade, maior prestigio)

ajuste=lm(prestige ~ income + education, data = Duncan) 
### Ajuste do modelo de regress�o linear m�ltipla para o prestigio da 
### profiss�o em fun��o de escolaridade e renda.

print(ajuste) ### Estimativas de m�nimos quadrados para os par�metros do modelo.

summary(ajuste) 
### Repare que os coeficientes associados a renda e educa��o s�o positivos, 
### indicando que o prestigio aumenta quanto maiores os percentuais de 
### "alta renda" e "alta escolaridade" da profiss�o.

### Algumas interpreta��es:
### O intercepto n�o tem uma interpreta��o pr�tica, pois percentuais iguais 
### a zero para escolaridade e renda n�o fazem parte do escopo do estudo.

### Estima-se, em m�dia, um aumento de 0,545 no grau (percentual) de prest�gio 
### da profiss�o para 1% a mais de profissionais com ensino m�dio, mantendo a renda fixa.

### Estima-se, em m�dia, um aumento de 0,598 no grau (percentual) de prest�gio 
### da profiss�o para 1% a mais de profissionais que recebem mais de $3500, mantendo a escolaridade fixa.

fitted(ajuste) ### Prestigio ajustado pelo modelo para as 45 profiss�es.
plot(Duncan$prestige, fitted(ajuste), xlab='Prest�gio observado', ylab='Prest�gio ajustado', pch = 20)
### Gr�fico dos valores observados versus valores ajustados pelo modelo.

resid(ajuste) ### Res�duos produzidos pelo modelo ajustado.
plot(fitted(ajuste), resid(ajuste), xlab = 'Valores ajustados', ylab = 'Res�duos', pch = 20)
### Gr�fico de res�duos vs valores ajustados.



### E se ajust�ssemos uma regress�o linear simples para cada vari�vel explicativa?

ajuste_srs1 <- lm(prestige ~ income, data = Duncan)
summary(ajuste_srs1)

ajuste_srs2 <- lm(prestige ~ education, data = Duncan)
summary(ajuste_srs2)

vcov(ajuste) ### Matriz de vari�ncias e covari�ncias estimada para os estimadores dos betas.

confint(ajuste) ### Intervalos de confian�a (95%) para os par�metros do modelo.
predict(ajuste, newdata = data.frame(income = 50,education = 50), interval = 'confidence') 
### Intervalo de confian�a (95%) para o prestigio esperado de profiss�es para as quais
### 50% dos profissionais tem ensino m�dio e 50% tem renda superior a $3500.


predict(ajuste,newdata=data.frame(income=50,education=50),interval='prediction') 
### Intervalo de predi��o (95%) para o grau de prestigio de uma "nova" profiss�o para a qual
### 50% dos profissionais tem ensino m�dio e 50% tem renda superior a $3500.


### Observem que, embora as conclus�es sejam semelhantes, as estimativas
### s�o bem diferentes. No modelo de regress�o linear m�ltipla estamos
### avaliando o efeito da educa��o no prest�gio controlando o efeito
### de renda (o mesmo vale para o efeito de renda). Nos modelos de regress�o
### linear simples estamos avaliando o efeito de cada vari�veil explicativa
### n�o controlando o efeito da segunda vari�vel. Como renda e escolaridade
### est�o correlacionadas, o efeito das vari�veis muda se ajustarmos ou n�o
### o efeito da outra vari�vel.

### Um exemplo mais dr�stico. Vamos simular dados para duas vari�veis explicativas
### correlacionadas e a resposta como fun��o das duas vari�veis.

set.seed(98)
x1 <- runif(100)
x2 <- x1 + rnorm(100, 0, 1.5) ### x2 = x1 + e2, e2 ~ Normal(0, 1.5^2)
erro <- rnorm(100, 0, 0.05) ### epsilon ~ Normal(0, 0.05^2) (erro)
y <- 0.1*x1 + 0.005*x2 + erro ### simulando valores para y.

ajuste_rlm <- lm(y ~ x1 + x2)
summary(ajuste_rlm)
### Observe que x2 n�o apresenta efeito significativo ao ajustarmos y tamb�m por 
### x1.

ajuste_rls <- lm(y ~ x2)
summary(ajuste_rls)
### Quando n�o ajustamos o efeito de x1, x2 passa a apresentar efeito significativo
### no modelo.

