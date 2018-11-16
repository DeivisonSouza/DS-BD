
m <- lm(Fertility~., data=swiss)
s <- step(m, dir='both')
summary(s)

m0 <- lm(Fertility~1, data=swiss)
summary(m0)

s2 <- step(m0, ~ Agriculture + Examination + Education + Catholic + Infant.Mortality, dir='both')

m1 <- update(m0, .~.+Education)

s3 <- step(m1, ~Agriculture + Education + Examination + Catholic + Infant.Mortality, dir='forward')

