library(Tmisc)
data(quartet)
str(quartet)
#
library(plyr)
ddply(quartet, ~ set, summarize, Media = mean(x), DP = sd(x), rxy=cor(x,y))
#
library(ggplot2)
ggplot(quartet, aes(x, y)) + geom_point(size=2, col=2) + geom_smooth(method = lm, se = FALSE) + 
    facet_wrap(~set) + theme_bw()
#
mod1=lm(y~x,data=subset(quartet,set=="I"))
mod2=lm(y~x,data=subset(quartet,set=="II"))
mod3=lm(y~x,data=subset(quartet,set=="III"))
mod4=lm(y~x,data=subset(quartet,set=="IV"))
#
round(summary(mod1)$coef,3)
round(summary(mod2)$coef,3)
round(summary(mod3)$coef,3)
round(summary(mod4)$coef,3)
#
round(c(summary(mod1)$r.squared,summary(mod2)$r.squared,summary(mod3)$r.squared,summary(mod4)$r.squared),3)
