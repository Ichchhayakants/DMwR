setwd("C:/Users/Ichchhayakant/Desktop/R/DMwR")
library(lattice)
library(grid)
library(DMwR)
library(zoo)
library(xts)
library(tseries)
library(TTR)
library(quantmod)
data("GSPC")
#data<-read.csv(file.choose(), header = T)
#head(data)
#GSPC<- as.xts(read.zoo("sp500.csv", header=T))
#gspc <- as.xts(get.hist.quote("^GSPC", start = "1970-01-02", quote=c("Open","High","Low","Close",
#                                                                     "Volume","AdjClose")))
#getSymbols("^GSPC")
#setSymbolLookup(IBM=list(name='IBM', src='yahoo'),USDEUR=list(name='USD/EUR',src='oanda'))
#getSymbols(c('IBM','USDEUR'))

head(GSPC)
str(GSPC)
class(GSPC)

## sums up the large absolyte returns(returns abobve the target variation margin(fixed at 25%) )
T.ind <- function(quotes, tgt.margin=0.025, n.days=10)
{ 
  v<- apply(HLC(quotes),1,mean)       ## HLC() obtains High,Low , Close from the quotes object 
  r<- matrix(NA, ncol = n.days, nrow = NROW(quotes))
  for(x in 1:n.days) r[,x]<- Next(Delt(v, k=x), x)    ## next allows to shift the values of a time series in time 
  ## delt is used to calculate percentage or log returns of a series of prices

  x <- apply(r, 1, function(x) sum(x[x> tgt.margin| x< -tgt.margin]))
  if(is.xts(quotes))
    xts(x,time(quotes))
  else x
}

## Candlestick graphs 
candleChart(last(GSPC, "3 months"),  theme = "white", TA= NULL)
avgPrice <- function(p) apply(HLC(p),1, mean)
addAvgPrice <- newTA(FUN = avgPrice, col=1,legend="AvgPrice")
addT.ind <- newTA(FUN = T.ind, col="red", legend="tgtRet")
addAvgPrice(on=1)  ## plot average price 1= same screen 
addT.ind()  


myATR <- function(x) ATR(HLC(x))[,"atr"]
mySMI <- function(x) SMI(HLC(x))[,"SMI"]
myADX <- function(x) ADX(HLC(x))[,"ADX"]
myAroon <- function(x) aroon(x[,c("High","Low")])$oscillator
myBB <- function(x) BBands(HLC(x))[,"pctB"]
myChaikinVol <- function(x) Delt(chaikinVolatility(x[,c("High","Low")]))[,1]
myCLV <- function(x) EMA(CLV(HLC(x)))[,1]
myEMV <- function(x) EMV(x[,c("High","Low")], x[,"Volume"])[,2]
myMACD <- function(x) MACD(Cl(x))[,2]
myMFI <- function(x) MFI(x[, c("High","Low","Close")], x[,"Volume"])
mySAR <- function(x) SAR(x[,c("High", "Close")])[,1]
myVolat <- function(x) volatility(OHLC(x), calc = "garman")[,1]
data(GSPC)
class(GSPC)


library(randomForest)
data.model <- specifyModel(T.ind(GSPC)~ Delt(Cl(GSPC),k=1:10)+
                           myATR(GSPC)+mySMI(GSPC)+ myADX(GSPC)+
                           myAroon(GSPC)+myBB(GSPC)+ myChaikinVol(GSPC)+
                           myCLV(GSPC)+CMO(Cl(GSPC))+EMA(Delt(Cl(GSPC)))+
                           myEMV(GSPC)+ myVolat(GSPC)+ myMACD(GSPC)+myMFI(GSPC)+
                           RSI(Cl(GSPC))+ mySAR(GSPC)+ runMean(Cl(GSPC))+ runSD(Cl(GSPC)))

set.seed(1234)                            
rf <- buildModel(data.model, method = 'randomForest', 
                 training.per = c(start(GSPC), index(GSPC["1999-12-31"])),
                 ntree=50, importance=T)
varImpPlot(rf@fitted.model, type = 1) ##type 1= mean decrease in accuracy
## type 2= men decrease in node impurity
imp <- importance(rf@fitted.model,type = 1)
rownames(imp)[which(imp>10)]  ## 10 is the threshold


data.model<- specifyModel(T.ind(GSPC)~ Delt(Cl(GSPC), k=1)+
                            myATR(GSPC)+myADX(GSPC)+myEMV(GSPC)+
                            myVolat(GSPC)+myMACD(GSPC)+mySAR(GSPC)+
                            runMean(Cl(GSPC)))


Tdata.train <- as.data.frame(modelData(data.model,data.window=c('1970-01-02','1999-12-31')))
Tdata.eval <- na.omit(as.data.frame(modelData(data.model, data.window = c('2000-01-01','2015-09-15'))))
Tform <- as.formula('T.ind.GSPC~.')

set.seed(1234)
library(nnet)
norm.data <- scale(Tdata.train)
nn <- nnet(Tform, norm.data[1:1000,], size=10, decay =0.01, maxit = 1000, linout = T, trace =F)
## decay controls the weight updating rate of the backpropagation algorithm
## trace =F is used to avoid some of the output of the function regarding the optimization process
norm.preds<- predict(nn, norm.data[1001:2000,]) 
preds <- unscale(norm.preds,norm.data)

## treading signals
sigs.nn <- trading.signals(preds, 0.1,-0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000,"T.ind.GSPC"], 0.1,-0.1)
sigs.PR(sigs.nn,true.sigs)


## classification using nnet
set.seed(1234)
library(nnet)
signals<- trading.signals(Tdata.train[,"T.ind.GSPC"], 0.1,-0.1)
norm.data <- data.frame(signals=signals, scale(Tdata.train[,-1]))
nn <- nnet(signals~., norm.data[1:1000,], size=10, decay=0.01, maxit=1000, trace=F)
preds <- predict(nn, norm.data[1001:2000,],type = "class")  ## class obtain a single class lebel for each test case
sigs.PR(preds, norm.data[1001:2000,1])


### Support Vecrtor Machine4

library(e1071)
sv <- svm(Tform,Tdata.train[1:1000,], gamma = 0.001, cost=100) ## cost = cost of violations of the margin
s.preds <- predict(sv,Tdata.train[1001:2000,])
sigs.svm <- trading.signals(s.preds, 0.1,-0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.GSPC"], 0.1, -0.1)
sigs.PR(sigs.svm, true.sigs)


## Classification Task using Kernlab 
library(kernlab)
data <- cbind(signals= signals, Tdata.train[,-1])
ksv <- ksvm(signals ~ . ,data[1:1000,], C=10)

## using automatic sigma estimation(sigest) for RBF or Laplace kernel 
ks.preds <- predict(ksv, data[1001:2000,])
sigs.PR(ks.preds, data[1001:2000,1])




## Multivariate Adaptive Regression (MARS)
library(earth)
e <- earth(Tform, Tdata.train[1:1000,])
e.preds <- predict(e, Tdata.train[1001:2000,])
sigs.e <- trading.signals(e.preds,0.1,-0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.GSPC"], 0.1, -0.1)
sigs.PR(sigs.e, true.sigs)


policy.1 <- function(signals, market, opened.pos, money, bet=0.2, hold.time=10,
                     exp.prof=0.025, max.loss=0.05)
{
  d<- NROW(market) ## id of today  
  orders <- NULL
  nos <- NROW(opened.pos)
  ## nothing to do!
  if(!nos && signals[d]== 'h') return(orders)
  
  ## First lets check if we can open a new position
  # I) long positions
  if(signals[d]== 'b' && !nos){
    quant <- round(bet*money/market[d,'Close'],0)
    if(quant >0)
      orders <- rbind(orders, 
                      data.frame(order=c(1,-1,-1), order.type=c(1,2,3),
                                         val= c(quant, 
                                                market[d,'Close']*(1+exp.prof),
                                                market[d,'Close']*(1- max.loss)
                                                ),
                                         action=c('open','close','close'),
                                         posID=c(NA,NA,NA)
                                         )
                      )
  # II short position  
  } else if(signals[d]=='s' && !nos){
    # this is the nr of stocks we already need to buy
    #because of currently opened short positions 
    need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                              "N.stocks"])*market[d,'Close']
    quant<- round(bet*(money-need2buy)/market[d,'Close'],0)
    if(quant>0)
      orders <- rbind(orders,
                      data.frame(order=c(-1,1,1),order.type=c(1,2,3),
                                 val=c(quant,
                                       market[d,'Close']*(1-exp.prof),
                                       market[d,'Close']*(1+max.loss)
                                       ),
                                 action=c('open','close','close'),
                                 posID=c(NA,NA,NA)
                                 )
                      )
  }
  
  ## now lets check if we need to close positions beacuse their time is over
  
  if(nos)
    for(i in 1:nos) {
      if((d - opened.pos[i,'odate']) >= hold.time)
        orders <- rbind(orders,
                        data.frame(order=-opened.pos[i,'pos.type'],
                                   order.type=1,
                                   val=NA,
                                   action='close',
                                   posID=rownames(opened.pos)[i]
                                   )
                        ) 
    }
  orders
}


#### policy 2 
policy.2 <- function(signals, market, opened.position, money,
                     bet=0.2, exp.prof=0.025, max.loss=0.05)
{
  d <- NROW(market) # this is the ID of today
  orders <- NULLn0s <- NROW(opened.pos)
  # nothing to do!
  if(!nos && signals[d]=='h') 
    return(orders)
  ## First lets check if we can open new positions 
  # i) long positions
  if(signals[d]=='b') {
    quant <- round(bet*money/market[d,'Close'],0)
    if(quant > 0)
      orders <- rbind(orders,
                      data.frame(order=c(1,-1,-1), order.type=c(1,2,3),
                                 val=c(quant,
                                       market[d,'Close']*(1+exp.prof),
                                       market[d,'Close']*(1-max.loss)
                                       ),
                                 action=c('open','close','close'),
                                 posID= c(NA,NA,NA)
                                 )
                      )
    
  ## ii) short positions 
  } else if(signals[d]=='s') {
      ## this is the money already comitted to buy stocks because of currently 
      # opened short positions
    need2buy <- sum(opened.pos[opened.ops[,'pos.type']==-1,
                               "N.stocks"])*market[d,'Close']
    quant <- round(bet*(money-need2buy)/market[d,'Close'],0)
    if(quant > 0)
      orders<- rbind(orders,
                     data.frame(order=c(-1,1,1),order.type=c(1,2,3),
                                val= c(quant,
                                       market[d,'Close']*(1-exp.prof),
                                       market[d,'Close']*(1+max.loss)
                                       ),
                                action = c('open','close','close'),
                                posID = c(NA,NA,NA)
                                )
                     )
  }
  orders
}


## Train and test position
start <- 1
len.tr <- 1000
len.ts <- 500
tr <- start :(start+len.tr-1)
ts <- (start+len.tr) : (start+len.tr+len.ts-1)
## getting the quotes for the testing period
data(GSPC)
date <- rownames(Tdata.train[start+len.tr,])
market <- GSPC[paste(date,'/',sep ='')][1:len.ts]
## learning the model and obtaining its signal predictions
library(e1071)
s <- svm(Tform, Tdata.train[tr,], cost=10, gamma=0.01)
p <- predict(s,Tdata.train[ts,])
sig <- trading.signals(p,0.1,-0.1)
# now using the simulated trader
t1 <- trading.simulator(market,sig,
                        'policy.1',
                        list(exp.prof=0.05, bet=0.2, hold.time=30))
