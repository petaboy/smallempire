#Import function imports datasets as a list for each ticker in c(...). If Nasdaq index is to be used,
#import datasets as a list for each index in c(...).
import <- function(ticker,index) {
  library(Quandl,tseries)
  return = list()
  if(missing(index)) {
    for (i in 1:length(ticker)) {
      input <- paste(c("WIKI/",ticker[i]),collapse='')
      data <- Quandl(input)
      data <- cbind.data.frame('V1' = data$Date,  'V2' = data$`Adj. Close`)
      colnames(data) <- c('date',paste(ticker[i]))
      return[[i]] <-data
    }
    } 
  else {
    for (i in 1:length(ticker)) {
      input <- paste(c("WIKI/",ticker[i]),collapse='')
      data <- Quandl(input)
      data <- cbind.data.frame('V1' = data$Date,  'V2' = data$`Adj. Close`)
      colnames(data) <- c('date',paste(ticker[i]))
      return[[i]] <-data
    }
    for (i in 1:length(index)) {
      input <- paste(c("NASDAQOMX/",index[i]),collapse='')
      data <- Quandl(input)
      data <- cbind.data.frame('V1' = data[,1],  'V2' = data[,2])
      colnames(data) <- c('date',paste(index[i]))
      return[[i+length(ticker)]] <-data
  }
  

  }
  return = return
}

percent_return <- function (data) {

  frame <- Reduce(function(x, y) merge(x, y, by="date", all=TRUE), data)
  frame <- na.omit(frame)
  transform <- sweep(as.matrix(frame[, -1]), MARGIN = 2, as.matrix(frame[1,-1]), FUN = "/")
  return <-cbind(frame[1],data.frame(transform))
}


#Daily_percent accepts the list of datasets outputted from the import function

daily_percent <- function (data) {
  frame <- Reduce(function(x, y) merge(x, y, by="date", all=TRUE), data)
  frame <- na.omit(frame)
  transform <- apply(as.matrix(frame[,-1]), MARGIN = 2, diff)
  return <- cbind('date' = frame[-1,1],data.frame(transform))

}

#Accepts daily percentage change dataframe from the daily_percent function
#http://gekkoquant.com/2012/12/17/statistical-arbitrage-testing-for-cointegration-augmented-dicky-fuller/
cointegration <- function(data) {
  library(tseries)
  
  transform1 <-daily_percent(data)
  regression_1 <- combn(names(transform1[-1]), 2, function(x){lm(transform1[, x])}, simplify = FALSE)
  vars1 <- combn(names(transform1[-1]), 2) 
  result_one <- lapply(regression_1,summary)
  names(result_one) <- vars1[1 , ]
  
  transform2 <-percent_return(data)
  regression_2 <- combn(names(transform2[-1]), 2, function(x){lm(transform2[, x])}, simplify = FALSE)
  vars2 <- combn(names(transform2[-1]), 2)
  list_of_residuals <- lapply(regression_2,residuals)
  residuals_dataframe <- as.data.frame(list_of_residuals)
  result_two <- apply(residuals_dataframe,2,adf.test)
  names(result_two) <- vars2[1 , ]
  
  result <- list(result_one,result_two)

  return(result)
}


