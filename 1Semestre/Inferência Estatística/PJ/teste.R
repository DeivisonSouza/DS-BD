
L1 <- function(theta) dnorm(2.45, m=theta, sd=1)
L2 <- function(theta)
  pnorm(4,mean=theta,sd=1)-pnorm(0.9,mean=theta,sd=1)
L3 <- function(theta)
  5*pnorm(3.5,m=theta,s=1)^4 * dnorm(3.5,m=theta,s=1)


