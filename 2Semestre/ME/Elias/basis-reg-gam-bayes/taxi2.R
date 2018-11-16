
library(data.table)

system.time(taxi <- fread('~/dados/diversos/taxiOneWeek.csv'))
dim(taxi)

head(taxi)

hist(taxi$pickup_latitude)
table(cut(taxi$pickup_latitude, c(-Inf, 35:43, Inf)))

taxi <- taxi[which(findInterval(taxi$pickup_latitude, c(40,41))==1), ]
dim(taxi)

hist(taxi$pickup_longitude)
table(cut(taxi$pickup_longitude, c(-Inf, -100, -80, -70, Inf)))
table(cut(taxi$pickup_longitude, c(-Inf, -75, -73, Inf)))

taxi <- taxi[which(findInterval(taxi$pickup_longitude, c(-75, -73))==1), ]

library(mgcv)

names(taxi)
dim(taxi)

id.s <- sample(1:nrow(taxi), 1e4)

m0 <- gam(fare_amount ~ s(pickup_longitude) + s(pickup_latitude), data=taxi[id.s,])

par(mfrow=c(2,1), mar=c(3,3,1,1), mgp=c(2,1,0))
plot(m0, select=1)
plot(m0, select=2)

m <- gam(fare_amount ~ s(pickup_longitude, pickup_latitude), data=taxi[id.s,])
anova(m0, m, test='F')

plot(m, select=1)
vis.gam(m,theta=-45,color="gray")

