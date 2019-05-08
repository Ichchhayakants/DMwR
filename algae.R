
setwd("C:/Users/Ichchhayakant/Desktop/R/DMwR")
library(DMwR)
head(algae)
 algae<- read.table('Analysis.txt',
                    header = F,dec = '.',
                    col.names = c('season','size','speed','mxPH','mno2','cl',
                                  'NO3','NH4','oPO4','PO4','chla','a1','a2',
                                  'a3','a4','a5','a6','a7'),
                    na.strings = c('XXXXXXX'))
 
str(algae)
class(algae) 
hist(algae$mxPH, prob=T, col = "blue")


library(car)
par(mfrow=c(1,2))
hist(algae$mxPH, prob=T, xlab ='',
     main = 'Histogram of max pH value', ylim = 0:1)
lines(density(algae$mxPH, na.rm = T))
rug(jitter(algae$mxPH))
qq.plot(algae$mxPH,main='Normal QQ plot of max ph')
par(mfrow=c(1,1))

