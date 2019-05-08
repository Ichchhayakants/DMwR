library(xts)
library(zoo)
x1 <- xts(rnorm(100), seq(as.POSIXct("2000-01-01"),len=100, by="day"))
x1[1:5]

x2 <- xts(rnorm(100), seq(as.POSIXct("2000-01-01 13:00"),len=100, by="min"))
x2[1:5]

x3 <- xts(rnorm(3), as.Date(c("2005-01-01","2005-01-10","2005-01-12")))
x3

x1[as.POSIXct("2000-01-04")]
x1["2000-01-05"]

x1["20000105"]

x1["2000-04"]

x1["2000-03-27/"]

x1["2000-02-26/2000-03-03"]

x1["/2000-01-03"]

mts.vals <- matrix(round(rnorm(25),2),5,5)
colnames(mts.vals)<- paste('ts',1:5,sep = '')
mts<- xts(mts.vals, as.POSIXct(c('2003-01-01', '2003-01-04',
                                 '2003-01-05', '2003-01-06',
                                 '2003-02-16')))
mts
mts["2003-01", c("ts2","ts5")]
index(mts)
coredata(mts)
