#QUandl Data import module for test one.adf . Switch statements will be added to make this module 
#usable for other tests.
library(Quandl,tseries)
ticker <- readline(prompt="Insert stock ticker: ")
input = paste(c("WIKI/",ticker),collapse='')
data <-Quandl(input)
test <- cbind.data.frame(data$Date,data$`Adj. Close`)
colnames(test) <- c('V1','V2')