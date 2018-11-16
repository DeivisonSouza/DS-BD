
library(data.table)

system.time(taxi <- fread('~/dados/diversos/taxiOneWeek.csv'))
dim(taxi)

head(taxi)

table(data.frame(flag=taxi$store_and_fwd_flag, code=taxi$rate_code, vendor=taxi$vendor_id)) 
data.frame(t.vsr <- table(vsr <- paste0(taxi$vendor_id, '_', taxi$store_and_fwd_flag, taxi$rate_code)))

cor(taxi$total_amount, taxi$fare_amount)
smoothScatter(taxi$trip_distance, taxi$total_amount) 
smoothScatter(taxi$trip_time_in_secs, taxi$total_amount) 

hist(taxi$pickup_latitude)
table(cut(taxi$pickup_latitude, c(-Inf, 35:43, Inf)))

taxi <- taxi[which(findInterval(taxi$pickup_latitude, c(40,41))==1), ]
dim(taxi)

hist(taxi$pickup_longitude)
table(cut(taxi$pickup_longitude, c(-Inf, -100, -80, -70, Inf)))
table(cut(taxi$pickup_longitude, c(-Inf, -75, -73, Inf)))

taxi <- taxi[which(findInterval(taxi$pickup_longitude, c(-75, -73))==1), ]

smoothScatter(taxi$pickup_latitude, log(taxi$fare_amount, 10))

smoothScatter(taxi$trip_time_in_secs, log(taxi$fare_amount, 10))
smoothScatter(taxi$trip_distance, log(taxi$fare_amount, 10))

table(cut(taxi$trip_distance, c(0:20, Inf), right=FALSE))
d <- cut(taxi$trip_distance, c(0:10, 12, 15, 20, 30, Inf), right=FALSE)

xd <- model.matrix(~d-1)
dim(xd)
print(object.size(xd), unit='Mb')

library(Matrix)
X <- Matrix(xd)
print(object.size(X), unit='Mb')


library(glmnet)
system.time(result <- glmnet(X, taxi$fare_amount))
plot(result, xvar='lambda', label=TRUE)

object.size(d)
object.size(xd)/object.size(d)

Matrix(d) -> xd2
