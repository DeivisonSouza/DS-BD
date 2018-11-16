#Verossimilhança
#1) Considere as observações independentes de uma distribuição N(mu, var=5²):
#
#Escrever um código que estime (mu) numericamente para dados:
#
#a) 20, 28, 39
#b) <22, [25-32], >38
#
#a) 5,3,0,1, 6, 2, 3, 4,5,3
#b) 5, <3, 0,1,>4,[1-3], 3,4,>3,[1-6]

L1 <- function(theta) dnorm(20,m=theta, sd=25)

L2 <- function(theta) dnorm(28,m=theta, sd=25)

L3 <- function(theta) dnorm(39,m=theta, sd=25)

L4 <- function(theta) dnorm(c(20,28,39), m=theta, sd=25)







optim(par = 20,
      fn = L4,
#      y = Canal.1$fcum.insc,
#      x1 = Canal.1$dias)