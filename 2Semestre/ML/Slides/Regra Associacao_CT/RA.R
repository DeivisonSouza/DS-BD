require(arules)
require(MASS)
require(arulesViz)

### Para iniciar, vamos simular alguns dados e explorar a visualiza��o.
### Inicialmente, vamos simular 100 transa��es para um conjunto 100 itens. 
### Nesta primeira simula��o, as compras ser�o simuladas de maneira completamente
### aleat�ria, de forma que n�o se tenha associa��es. 

set.seed(2018)
a1 <- matrix(sample(c(0,1), prob = c(0.75,0.25), 10000, replace = TRUE), nrow = 100)
colnames(a1) <- paste('Item', 1:100)
rownames(a1) <- paste('Trans.', 1:100)
a1[1:10,] ### Imprimeindo as dez primeiras transa��es.

### Agora, vamos converter a matriz a1 para um objeto da classe "transactions",
### para an�lise no pacote arules.

a2 <- as(a1,  "transactions")
a2

summary(a2)
### Neste resumo, temos os itens mais frequentes e as respectivas frequ�ncias;
### a distribui��o dos n�meros de itens comprados nas 100 transa��es.
### Assim, o item 20 foi adquirido com maior frequ�ncia (em 39 transa��es),
### seguido pelo item 27 (em 33 transa��es) e assim por diante. Al�m disso,
### em duas transa��es tivemos 17 itens comprados; quatro transa��es apresentaram
### 18 utens e uma transa��o registrou o maior n�mero de itens (36).

image(a2) ### Representa��o gr�fica.

### Agora, uma simula��o com estrutura n�o aleat�ria (exist�ncia de associa��es).
### Vamos simular dados de uma distribui��o normal multivariada e depois 
### dicotomizar. As correla��es na normal multivariada v�o induzir as 
### associa��es entre os itens.

sigma <- matrix(0, nrow = 100, ncol = 100)
### Matriz de covari�ncias. Vamos substituir alguns dos zeros para introduzir
### correla��o para os itens.

sigma[1:25,1:25] <- 0.6 ### Covari�ncia entre os itens 1 a 25.
sigma[40:60,40:60] <- 0.7 #### Covari�ncia entre os itens 40 e 60.
sigma[70:90,70:90] <- 0.8 ### Covari�ncia entre os itens 70 e 90.

diag(sigma) <- 1 ### Vari�ncia igual a 1 para todos.

set.seed(2019)    
x <- mvrnorm(100, mu = sample(seq(-0.2,1,length.out = 100)), Sigma = sigma)
### A matriz x armazena os dados simulados.

x <- ifelse(x < 1.2, 0, 1)
### Dicotomiza��o.

x2 <- as(x,  "transactions")
image(x2)
### Convertendo e visualizando a base.

data("Groceries")
help("Groceries")

Groceries
Groceries@data ### Visualiza��o dos dados.
Groceries@itemInfo ### Descri��o dos itens e classifica��o (em dois n�veis)

### Vamos explorar algumas visualiza��es das cestas de compras. A t�tulo 
### de ilustra��o, vamos pegar as dez primeiras.

### A primeira transa��o (cesta de compras) � composta por {citrus fruit,
### semi-finished bread,margarine,ready soups}; a segunda, por
### {tropical fruit,yogurt,coffee}, e assim por diante.

as(Groceries[1:10], "list") ### Lista com as cestas de compras.
### Na forma de uma lista.

### Agora, vamos explorar as frequ�ncias e associa��es de compras.

image(sample(Groceries, 200))
### Visualiza��o de 200 transa��es.

itemFrequency(Groceries)
### Frequ�ncia (relativa) com que os itens apareceram nas cestas de compras.

sort(itemFrequency(Groceries), decreasing = FALSE)
### Ordenando do item com maior frequ�ncia para o item com menor frequ�ncia.

sort(itemFrequency(Groceries, type = 'absolute'), decreasing = FALSE)
### Frequ�ncias absolutas.

itemFrequencyPlot(Groceries, support = 0.05, horiz = TRUE, col = 'lightblue')
### Gr�fico com os itens que apareceram em pelo menos 5% das listas de compras.

itemFrequencyPlot(Groceries, topN = 10, horiz = TRUE, col = 'lightblue')
### Gr�fico com os dez itens mais frequentes nas cestas de compras.

itemFrequencyPlot(Groceries, topN = 30, horiz = TRUE, col = 'lightblue')
### Gr�fico com os 30 itens mais frequentes nas cestas de compras.

### Vamos construir a tabela cruzada com as frequ�ncias de compras para
### cada par de itens.
tabela <- crossTable(Groceries)
tabela[1:6,1:6] ### Pequeno extrato da tabela.

tabela['ham', 'ham'] ### Frequ�ncia de compras para presunto.
supp_ham <- tabela['ham', 'ham']/9835 
supp_ham
### Suporte (ham)

supp_eggs <- tabela['domestic eggs', 'domestic eggs']/9835 
supp_eggs
### Suporte (domestic eggs)

supp_ham_eggs <- tabela['ham', 'domestic eggs']/9835
supp_ham_eggs
### Suporte (domestic eggs U ham) 

supp_ham_eggs/supp_ham
### Confian�a(ham, domestic eggs)

supp_ham_eggs/supp_eggs
### Confian�a(domestic eggs, ham)

supp_ham_eggs/(supp_ham*supp_eggs)
### Lift(ham,eggs)

crossTable(Groceries, measure='lift',sort=T)[1:8,1:8]

### Vamos come�ar com a minera��o de itens mais frequentes.

### Conjuntos com no m�nimo dois itens.
itemsets <- apriori(Groceries,
                    parameter = list(support=.001,
                                     minlen=2,
                                     target='frequent' 
                    ))
summary(itemsets)
inspect(sort(itemsets, by='support', decreasing = T)[1:5])


### Conjuntos com no m�nimo tr�s itens.
itemsets <- apriori(Groceries,
                    parameter = list(support=.001,
                                     minlen=3,
                                     target='frequent' 
                    ))
inspect(sort(itemsets, by='support', decreasing = T)[1:5])

### Vamos ver qual o conjunto com cinco itens mais frequente.
itemsets <- apriori(Groceries,
                    parameter = list(support=.001,
                                     minlen=5,
                                     maxlen=5,
                                     target='frequent' 
                    ))
inspect(sort(itemsets, by='support', decreasing = T)[1])


### Agora, vamos fazer a minera��o de regras.

rules <- apriori(Groceries,
                 parameter = list(support=.02,
                                  confidence=.2,
                                  minlen=2,
                                  target='rules' 
                 ))
inspect(head(rules, n = 5, by = "support"))
inspect(head(rules, n = 5, by = "confidence"))
inspect(head(rules, n = 5, by = "lift"))

### Aplicando alguns filtros

inspect(subset(rules, subset = rhs %ain% 'yogurt' & confidence > .2))
### Regras com iogurte (� direita) e confian�a m�nima 0.8.

inspect(subset(rules, subset = lhs %ain% 'yogurt' & confidence > .2))
### Regras com iogurte (� esquerda) e confian�a m�nima 0.8.

inspect(subset(rules, subset = rhs %ain% 'yogurt' & support > .0015))
### Regras com iogurte (� direita) e suporte m�nimo 0.0015.

inspect(subset(rules, subset= lhs %ain% c("whole milk","yogurt") & confidence >.95))
### Regras com iogurte e leite (� esquerda) e confian�a m�nima de 0.95.

inspect(subset(rules, subset= items %ain% c("whole milk","yogurt") & confidence >.15))
### Regras com iogurte e leite (em qualquer posi��o) e confian�a m�nima de 0.95.

### Alguns gr�ficos.
plot(rules)
plot(rules, measure=c("support","lift"), shading="confidence")
plot(rules, method="grouped")
plot(rules, method="grouped", control=list(k=10))