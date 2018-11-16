library(read.dbc)
dim(rd <- read.dbc("~/dados/datasus/rd/PR/RDPR1807.dbc"))
if (FALSE)
    names(rd)

idade.b <- c(0:5, 7, 10, 12, 5*(3:19), max(rd$IDADE)+1e-6)
idade.g <- cut(rd$IDADE, idade.b, right=FALSE)
idade.m <- tapply(rd$IDADE, idade.g, mean)
p.idade.g <- tapply(rd$MORTE, idade.g, mean)

plot(idade.m, p.idade.g, axes=FALSE, xlab='Idade', ylab='Prop. de óbito')
box(); axis(1); axis(4)
abline(h=0:6/20, lty=2, col=gray(.5))

library(splines)

b <- bs(rd$IDADE, knots=5*(0:20), degree=3)
str(b)

idade <- 0:100
b0 <- bs(idade, knots=5*(0:20), degree=3)
plot(idade, b0[,1], type='n', ylab='')
for (j in 1:ncol(b0))
    lines(idade, b0[,j], col=j)

dim(b)
dados <- data.frame(y = rd$MORTE, b)
model.b <- glm(y ~ ., binomial, dados)
coef(model.b)

summary(model.b)

colnames(b0) <- paste0('X', 1:ncol(b0))
pred <- predict(model.b, newdata=data.frame(b0), type='response', se.fit=TRUE)

idade.b <- c(0:5, 7, 10, 12, 5*(3:19), max(rd$IDADE)+1e-6)
idade.g <- cut(rd$IDADE, idade.b, right=FALSE)
idade.m <- tapply(rd$IDADE, idade.g, mean)
p.idade.g <- tapply(rd$MORTE, idade.g, mean)

plot(idade.m, p.idade.g, axes=FALSE, xlab='Idade', ylab='Prop. de óbito')
box(); axis(1); axis(4)
abline(h=0:6/20, lty=2, col=gray(.5))

lines(idade, pred$fit -2 * pred$se.fit)
lines(idade, pred$fit +2 * pred$se.fit)

sum(rd$IDADE>90)
sum(rd$IDADE>95)


#### PARTE 2

cid1 <- substr(rd$DIAG_PRINC, 1, 1)
p.cid1 <- tapply(rd$MORTE, cid1, mean)
plot(p.cid1, axes=FALSE, ylab='Proporção de óbito', pch=19)
box(); axis(4); axis(1, 1:length(p.cid1), names(p.cid1))

cid1.idade <- tapply(
  rd$MORTE, list(cid=cid1, idade=idade.g), mean, na.rm=TRUE)
library(fields)
image.plot(1:nrow(cid1.idade), idade.m, cid1.idade,
           axes=FALSE, xlab='', ylab='')
axis(1, 1:nrow(cid1.idade), names(p.cid1))
axis(2, idade.m, names(p.idade.g), las=1); box()

data(ozone, package='faraway')
dim(ozone)
library(mgcv)
amint <- gam(O3 ~ te(temp, ibh), data=ozone)

plot(amint, select=1)
vis.gam(amint,theta=-45,color="gray")

