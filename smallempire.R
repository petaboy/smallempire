#Import function imports datasets as a list for each ticker in c(...). If Nasdaq index is to be used,
#import datasets as a list for each index in c(...). Indices are available to view at 
#https://www.quandl.com/data/NASDAQOMX-NASDAQ-OMX-Global-Index-Data
import <- function(ticker,index=NA, eps=FALSE,short=FALSE) {
  library(Quandl,tseries)
  Quandl.api_key("oRAxkus8z_DNZzedLmky")
  
  return = list()
  for (i in 1:length(ticker)) {
    input <- paste(c("WIKI/",ticker[i]),collapse='')
    data <- Quandl(input)
    data <- cbind.data.frame('V1' = data$Date,  'V2' = data$`Adj. Close`)
    colnames(data) <- c('date',paste(ticker[i]))
    return[[i]] <-data
  }
  if(missing(index)==FALSE) {
    for (i in 1:length(index)) {
      input <- paste(c("NASDAQOMX/",index[i]),collapse='')
      data <- Quandl(input)
      data <- cbind.data.frame('V1' = data[,1],  'V2' = data[,2])
      colnames(data) <- c('date',paste(index[i]))
      return[[i+length(ticker)]] <-data
    }
  }
  if(eps == TRUE) {
    try (for (i in 1:length(ticker)) {
      input <- paste(c("SF0/",ticker[i],"_NETINC_MRY"),collapse='')
      data <- Quandl(input)
      data <- cbind.data.frame('V1' = data[,1],  'V2' = data[,2])
      colnames(data) <- c('date',paste(ticker[i],"Net Income"))
      return[[i+length(ticker)]] <-data
    }
    )
  }
  if(short == TRUE) {
    try(
      for (i in 1:length(ticker)) {
        input <- paste(c("FINRA/FNYX_",ticker[i]),collapse='')
        data <- Quandl(input)
        data <- cbind.data.frame('V1' = data[,1],  'V2' = data[,2]/data[,4])
        colnames(data) <- c('date',paste(ticker[i],"Short Interest"))
        if (eps == TRUE) {
          return[[i+2*length(ticker)]] <- data
        }
        else {
          return[[i+length(ticker)]] <- data
        }
      }
      
    )
    
  }
  return = return
  }

percent_return <- function (data,skip=NA,start=NA,end=NA) {
  if (is.na(skip) == FALSE){
    input = data[-skip]
  }
  else{
    input = data
  }
  if(is.na(start) == FALSE & is.na(end) == FALSE ){
    start = start
    end = end
    a <- lapply(input,function(x){
      b <- cbind('date'= x[start:end,1],data.frame(sweep(as.matrix(x[start:end, -1]), MARGIN = 2, as.matrix(abs(x[start,-1])), FUN = "/")))
      colnames(b) <- colnames(x)
      return(b)
    })
  }
  else {
    a <- lapply(input,function(x){
      b <- cbind('date'= x[1],data.frame(sweep(as.matrix(x[, -1]), MARGIN = 2, as.matrix(abs(x[length(x[,1]),-1])), FUN = "/")))
      colnames(b) <- colnames(x)
      return(b)
  })
  }

  
  frame <- Reduce(function(x, y) merge(x, y, by="date", all=TRUE), a)
  return(frame)
}


#Daily_percent accepts the list of datasets outputted from the import function

daily_percent <- function (data,skip=NA) {
  if (is.na(skip) == FALSE){
    input = data[-skip]
  }
  else{
    input = data
  }
  
  a <- lapply(input, function(x){
    b <-cbind('date'=x[-1,1],data.frame(100*diff(as.matrix(x[,-1]))/as.matrix(x[-1,-1])))
    colnames(b) <-colnames(x)
    return(b)
  })
  
  if (is.na(skip) == FALSE){
    a <- c(a,data[skip])
  }
  
  frame <- Reduce(function(x, y) merge(x, y, by="date", all=TRUE), a)
  return(frame)

}

#Accepts daily percentage change dataframe from the daily_percent function
#http://gekkoquant.com/2012/12/17/statistical-arbitrage-testing-for-cointegration-augmented-dicky-fuller/
cointegration <- function(data) {
  library(tseries)
  
  transform1 <-daily_percent(data)
  transform1 <- na.omit(transform1)
  regression_1 <- combn(names(transform1[-1]), 2, function(x){lm(transform1[, x])}, simplify = FALSE)
  vars1 <- combn(names(transform1[-1]), 2) 
  result_one <- lapply(regression_1,summary)
  names(result_one) <- vars1[1 , ]
  
  transform2 <-percent_return(data)
  transform2 <-na.omit(transform2)
  regression_2 <- combn(names(transform2[-1]), 2, function(x){lm(transform2[, x])}, simplify = FALSE)
  vars2 <- combn(names(transform2[-1]), 2)
  list_of_residuals <- lapply(regression_2,residuals)
  residuals_dataframe <- as.data.frame(list_of_residuals)
  result_two <- apply(residuals_dataframe,2,adf.test)
  names(result_two) <- vars2[1 , ]
  
  result <- list(result_one,result_two)
  names(result) <- c("beta","adf")

  return(result)
}

trend_plot <- function(data,skip=NA){
  library(reshape2)
  library(ggplot2)
  library(plotly)
  
  transform <- percent_return(data,skip)
  
  meltframe <- melt(transform,id = "date")
  ggplotly(ggplot(data=meltframe, aes(x=date, y=value, group=variable, color = variable)) + geom_line(aes(group=variable)) + geom_point()
  )
  

}
red_flags <- function(data){
  library(tseries)
  library(ggplot2)
  library(plotly)
  

  transform1 <- daily_percent(data)
  transform2 <- percent_return(data,skip=c(2,3))
  bounds<- maxdrawdown(transform2[,2])
  focus<- transform2[bounds$to:bounds$from,]

  } 

