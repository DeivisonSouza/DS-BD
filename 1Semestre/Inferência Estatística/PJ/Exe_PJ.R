## Obter a funÃ§Ã£o de verosimilhanÃ§a e a estimativa do parÃ¢metro em cada caso:

## 1. Seja $Y \sim {\rm N(\mu ; \sigma^2 = 5^2)$ e os dados:
## a) 20, 28, 39
## b) <22,  [25-32] , >38
## c) >22,  [25-32] , <38

#-------------------------------------------------------------------------------------------------------------------------------------
# A funçao de verossimilhanca expressa as informacoes que existem nos dados. O máximo da função é o melhor valor (que tem mais haver
# com os dados..). Suponha que temos dados de uma distribuição normal (theta, variância). A verossimilhança é o produtorio da densidade
#de todos os pontos. (isso é calculado pela dnorm). No entanto, é mais vantajoso calcular a log-verossimilhanca, 
#pois trabalha com somatório´....
#-------------------------------------------------------------------------------------------------------------------------------------

# pnorm = calcula a probabilidade numa região (para tráz)...
# dnorm = 

# Qual a contribuição de cada dado para a verossimilhança? A soma da contribuição de cada dado é o valor máximo da verossimilhança...

## a) 20, 28, 39
ya <- c(20, 28, 39)                               # Dado um conjunto de dados, com distribuicão normal e variância conhecida...

mean(ya)                                          # média do conjunto de dados...         

# função para calcular a log-verossimilhança para os valores 20, 28, 39 (para calcular log-verossimilhança usa-se TRUE). 
# A função recebe um parâmetro "par" que constitui o theta (média). Então desejo saber o valor de theta que maximiza a contribuição
# dos dados. A funcão calcula a contribuicao média na escala log...

lla <- function(par){
  l1 <- dnorm(20, mean=par, sd=5, log=TRUE)               # cada linha calcula o log das contribuiçoes...
  l2 <- dnorm(28, mean=par, sd=5, log=TRUE)                
  l3 <- dnorm(39, mean=par, sd=5, log=TRUE)
  return(l1+l2+l3)                                        # retorna a soma das contribuições...
}

# calcula os valores da log-verossimilhaça...
lla(20)
lla(25)             # 25 tem contribuicao maior do que 20 (pois possui maior valor)
lla(29)             # 29 possui maior contribuicao do que 25, 20...             
lla(10:45)          # verificar no intervalo de 10 a 45 que 29 tem maior contribuição...    

# Aqui, usa-se a função curve para obter a curva de log-verossimilhança no intervalo pre-especificado (10 a 45).
# Neste caso, x é uma simulacão para o intervalo de verossimilhança (10 a 45). Y é o valor da log-verossimilhança...

curve(lla, from=27, to=31)                # curva de verossimilhança.
abline(v=29, lty=2, col="green")
abline(h=lla(29), lty=2, col="red")

# Duas funçoes genéricas: que fazem a mesma coisa. Mas, pode receber um vetor de dados...
lla <- function(par, y){
  sapply(par, function(x) sum(dnorm(y, mean=x, sd=5, log=TRUE)))
}

llav <- Vectorize(function(par, y){
  sum(dnorm(y, mean=par, sd=5, log=TRUE))
}, "par")

sum(dnorm(ya, mean=20, sd=5, log=T))
lla(20, y=ya)
lla(20:28, y=ya)
llav(20, y=ya)
llav(20:28, y=ya)


curve(lla(x, y=ya), from=27, to=31)
mean(ya)

# a função optimize busca por default o minimo da função. Para encontrar o máximo deve-se usar: maximum=TRUE.
(est.a <- optimize(lla, interval=c(10,50), y=ya, maximum=TRUE))

# A função optim também miniminiza a função. Para maximixar deve-se usar: list(fnscale=-1).
optim(25, lla, y=ya, control=list(fnscale=-1))
curve(lla(x, y=ya[1]), from=0, to=55)
curve(lla(x, y=ya[2]), from=0, to=55, add=TRUE)
curve(lla(x, y=ya[3]), from=0, to=55, add=TRUE)

### VEROSSIMILHANÇA PARA DADOS INTERVALARES...
# Para dados intervalares, qual seria o valor da média? No caso da verossimilhança é o valor que maximiza a verossimilhança.

## b) <22,  [25-32] , >38
llb <- Vectorize(function(par){
  l1 <- pnorm(22, mean=par, sd=5, log=TRUE)
  l2 <- log(diff(pnorm(c(25,32), mean=par, sd=5)))
  l3 <- pnorm(38, mean=par, sd=5, log=TRUE, lower=FALSE)
  return(l1+l2+l3)
}, "par")

llb(20)
curve(llb, from=15, to=45, col=2)

(est.b <- optimize(llb, interval=c(10,50), maximum=TRUE))


curve(llb, from=15, to=45, col=2)
curve(lla(x, y=ya), from=15, to=45, add=TRUE)

# Função deviance...Torna as curvas comparáveis...
dev.f <- function(par, fun, max.fun, ...){
  -2*(fun(par, ...) - max.fun) 
}

dev.f(25, lla, max.fun=est.a[[2]], y=ya)
dev.f(25, llb, max.fun=est.b[[2]])

#observar as aberturas das curvas. Quanto maior abertura maior a incerteza. Portanto, a incerteza p/ dados intervalares é maior...
curve(dev.f(x, fun=lla, max.fun=est.a[[2]], y=ya), from=15, to=45)
abline(v=est.a[[1]], lty=2)
curve(dev.f(x, fun=llb, max.fun=est.b[[2]]), from=15, to=45, add=TRUE, col=2)
abline(v=est.b[[1]], lty=2, col=2)


## c) >22,  [25-32] , <38

llc <- Vectorize(function(par){
  l1 <- pnorm(22, mean=par, sd=5, lower=F, log=TRUE)
  l2 <- log(diff(pnorm(c(25,32), mean=par, sd=5)))
  l3 <- pnorm(38, mean=par, sd=5, log=TRUE)
  return(l1+l2+l3)
}, "par")
llc(20)
llc(25)
curve(llc, from=15, to=45, col=4)

(est.c <- optimize(llc, interval=c(10,50), maximum=TRUE))
abline(v=est.c[[1]], lty=2, col=4)

curve(llc, from=15, to=45, col=4, ylim=c(-15, 0))
curve(llb, from=15, to=45, col=2, add=TRUE)
curve(lla(x, y=ya), from=15, to=45, add=TRUE)


# Agora desviance para tornar as curvar comparáveis...(a amostra mais imprecisa é a terceira...)
curve(dev.f(x, fun=lla, max.fun=est.a[[2]], y=ya), from=15, to=45)
abline(v=est.a[[1]], lty=2)
curve(dev.f(x, fun=llb, max.fun=est.b[[2]]), from=15, to=45, add=TRUE, col=2)
abline(v=est.b[[1]], lty=2, col=2)
curve(dev.f(x, fun=llc, max.fun=est.c[[2]]), from=15, to=45, add=TRUE, col=4)
abline(v=est.c[[1]], lty=2, col=4)

##
##
##
#FunÇão que permite a ter dados pontuais e intervalares...

llint <- Vectorize(function(par, y.p, y.i){
  ## y.p Ã© um vetor de dados pontuais
  ## i.i Ã© uma matriz (n x 2) de dados intervalares
  if(missing(y.p)) ll.p <- 0
  else ll.p <-  sum(dnorm(y.p, mean=par, sd=5, log=TRUE))
  if(missing(y.i)) ll.i <- 0
  else ll.i <- sum(apply(y.i, 1, function(x) log(diff(pnorm(x, mean=par, sd=5)))))
  return(ll.i+ll.p)
}, "par")


yb  <- cbind(c(-Inf, 25, 38), c(22, 32, Inf)) 
yc  <- cbind(c(22, 25, -Inf), c(Inf, 32, 38)) 

lla(25, y=ya)
llint(25, y.p=ya)
llb(25)
llint(25, y.i=yb)
llc(25)
llint(25, y.i=yc)

curve(lla(x, y=ya), from=15, to=45)
curve(llint(x, y.p=ya), from=15, to=45, add=TRUE, col=3, lty=2)

curve(llb, from=15, to=45)
curve(llint(x, y.i=yb), from=15, to=45, col=3, add=TRUE, lty=2)

curve(llc, from=15, to=45, col=4)
curve(llint(x, y.i=yc), from=15, to=45, col=3, add=TRUE, lty=2)


optimize(lla, interval=c(10,50), y=ya, maximum=TRUE)
optimize(llint, interval=c(10,50), y.p=ya, maximum=TRUE)
optimize(llb, interval=c(10,50), maximum=TRUE)
optimize(llint, interval=c(10,50), y.i=yb, maximum=TRUE)
optimize(llc, interval=c(10,50), maximum=TRUE)
optimize(llint, interval=c(10,50), y.i=yc, maximum=TRUE)

curve(dev.f(x, fun=llint, max.fun=est.a[[2]], y.p=ya), from=15, to=45)
abline(v=est.a[[1]], lty=2)
curve(dev.f(x, fun=llint, max.fun=est.b[[2]], y.i=yb), from=15, to=45, add=TRUE, col=2)
abline(v=est.b[[1]], lty=2, col=2)
curve(dev.f(x, fun=llint, max.fun=est.c[[2]], y.i=yc), from=15, to=45, add=TRUE, col=4)
abline(v=est.c[[1]], lty=2, col=4)

ya
yb
yb
llint(25, y.p=ya[1])
llint(25, y.p=ya[2])
llint(25, y.p=ya[3])
llint(25, y.i=yb[1,, drop=FALSE])
llint(25, y.i=yb[2,, drop=FALSE])
llint(25, y.i=yb[3,, drop=FALSE])
llint(25, y.i=yc[1,, drop=FALSE])
llint(25, y.i=yc[2,, drop=FALSE])
llint(25, y.i=yc[3,, drop=FALSE])

#-----------------------------------------------------------------------------------------------------
# Contribuição de cada dado...
par(mfrow=c(1,3), mar=c(3,3,0,0))
## a) 20, 28, 39
curve(llint(x, y.p=ya[1]), from=0, to=55)
curve(llint(x, y.p=ya[2]), from=0, to=55, add=TRUE)
curve(llint(x, y.p=ya[3]), from=0, to=55, add=TRUE)

## b) <22,  [25-32] , >38
curve(llint(x, y.i=yb[1,, drop=FALSE]), from=0, to=55, col=2)
curve(llint(x, y.i=yb[2,, drop=FALSE]), from=0, to=55, add=TRUE, col=2)
curve(llint(x, y.i=yb[3,, drop=FALSE]), from=0, to=55, add=TRUE, col=2)

## c) >22,  [25-32] , <38
curve(llint(x, y.i=yc[1,, drop=FALSE]), from=0, to=55, col=4)
curve(llint(x, y.i=yc[2,, drop=FALSE]), from=0, to=55, add=TRUE, col=4)
curve(llint(x, y.i=yc[3,, drop=FALSE]), from=0, to=55, add=TRUE, col=4)
#-----------------------------------------------------------------------------------------------------

par(mfrow=c(1,1))
## a) 20, 28, 39
curve(llint(x, y.p=ya[1]), from=0, to=55, ylim=c(-25, 0))
curve(llint(x, y.p=ya[2]), from=0, to=55, add=TRUE)
curve(llint(x, y.p=ya[3]), from=0, to=55, add=TRUE)
## b) <22,  [25-32] , >38
curve(llint(x, y.i=yb[1,, drop=FALSE]), from=0, to=55, col=2, add=TRUE)
curve(llint(x, y.i=yb[2,, drop=FALSE]), from=0, to=55, add=TRUE, col=2)
curve(llint(x, y.i=yb[3,, drop=FALSE]), from=0, to=55, add=TRUE, col=2)
## c) >22,  [25-32] , <38
curve(llint(x, y.i=yc[1,, drop=FALSE]), from=0, to=55, col=4, add=TRUE)
curve(llint(x, y.i=yc[2,, drop=FALSE]), from=0, to=55, add=TRUE, col=4)
curve(llint(x, y.i=yc[3,, drop=FALSE]), from=0, to=55, add=TRUE, col=4)

## fazer grÃ¡ficos das deviances!!!

##
## Ã‰ possÃ?vel juntar todos os dados (de a) b) e c)!
##
curve(llint(x, y.p=ya, y.i=rbind(yb, yc)), from=15, to=45)
(est.all <- optimize(llint, interval=c(0, 50), y.p=ya, y.i=rbind(yb, yc), maximum=TRUE))
abline(v=est.all[[1]], lty=2)

curve(dev.f(x, fun=llint, max.fun=est.all[[2]], y.p=ya, y.i=rbind(yb, yc)),
      from=15, to=45)
abline(v=est.all[[1]], lty=2)
