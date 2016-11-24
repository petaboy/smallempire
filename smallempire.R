#Import function imports datasets as a list for each ticker in c(...). If Nasdaq index is to be used,
#import datasets as a list for each index in c(...). Indices are available to view at 
#https://www.quandl.com/data/NASDAQOMX-NASDAQ-OMX-Global-Index-Data
import <- function(ticker,type) {
  library(Quandl,tseries)
  Quandl.api_key("enter_API_key_here")
  
  return = list()
  switch(type,
         "stock" = {
           for (i in 1:length(ticker)) {
           input <- paste(c("WIKI/",ticker[i]),collapse='')
           data <- Quandl(input)
           data <- cbind.data.frame('V1' = data$Date,  'V2' = data$`Adj. Close`)
           colnames(data) <- c('date',paste(ticker[i]))
           return[[i]] <-data
           }
           return(return)
           },
         "index" = {
           for (i in 1:length(ticker)) {
           input <- paste(c("NASDAQOMX/",ticker[i]),collapse='')
           data <- Quandl(input)
           data <- cbind.data.frame('V1' = data[,1],  'V2' = data[,2])
           colnames(data) <- c('date',paste(ticker[i]))
           return[[i]] <-data  
           }
           return(return)
           },
          
         "eps" = {
           for (i in 1:length(ticker)) {
           input <- paste(c("SF0/",ticker[i],"_NETINC_MRY"),collapse='')
           data <- Quandl(input)
           data <- cbind.data.frame('V1' = data[,1],  'V2' = data[,2])
           colnames(data) <- c('date',paste(ticker[i],"Net Income"))
           return[[i]] <-data
            }
           return(return) 
           },
         "short" = {
           for (i in 1:length(ticker)) {
           input <- paste(c("FINRA/FNYX_",ticker[i]),collapse='')
           data <- Quandl(input)
           data <- cbind.data.frame('V1' = data[,1],  'V2' = data[,2]/data[,4])
           colnames(data) <- c('date',paste(ticker[i],"Short Interest"))
           return[[i]] <- data
           }
           return(return)
           }
         )
}


percent_return <- function (data) {
  a <- lapply(data,function(x){
    b <- cbind('date'= x[1],data.frame(sweep(as.matrix(x[, -1]), MARGIN = 2, as.matrix(abs(x[1,-1])), FUN = "/")))
    colnames(b) <- colnames(x)
    return(b)
        })
  frame <- Reduce(function(x, y) merge(x, y, by="date", all=TRUE), a)
  return(frame)
}

daily_percent <- function (data) {
  a <- lapply(data, function(x){
    b <-cbind('date'=x[-1,1],data.frame(100*diff(as.matrix(x[,-1]))/as.matrix(x[-1,-1])))
    colnames(b) <-colnames(x)
    return(b)
  })
  frame <- Reduce(function(x, y) merge(x, y, by="date", all=TRUE), a)
  return(frame)
}

#Accepts daily percentage change dataframe from the daily_percent function
#http://gekkoquant.com/2012/12/17/statistical-arbitrage-testing-for-cointegration-augmented-dicky-fuller/
cointegration <- function(data,index=NA) {
  library(tseries)
  
  if (is.na(index) == FALSE){
    data <- c(data,index)
  }
  transform1 <-daily_percent(data)
  transform1 <- na.omit(transform1)
  regression_1 <- combn(names(transform1[-1]), 2, function(x){lm(transform1[, x])}, simplify = FALSE)
  vars1 <- combn(names(transform1[-1]), 2) 
  result_one <- lapply(regression_1,function(x)
    { a <- summary(x)
    return(list("coefficients" = a$coefficients,"r-squared" = a$r.squared))
  })
  names(result_one) <- vars1[1 , ]
  
  transform2 <-percent_return(data)
  transform2 <-na.omit(transform2)
  regression_2 <- combn(names(transform2[-1]), 2, function(x){lm(transform2[, x])}, simplify = FALSE)
  vars2 <- combn(names(transform2[-1]), 2)
  list_of_residuals <- lapply(regression_2,residuals)
  residuals_dataframe <- as.data.frame(list_of_residuals)
  result_two <- apply(residuals_dataframe,2,function(x){adf.test(x,k=0,alternative="stationary")})
  names(result_two) <- vars2[1 , ]
  
  result <- list(result_one,result_two)
  names(result) <- c("beta","adf")

  return(result)
}

trend_plot <- function(data,eps=NA,short=NA,start=NA,end=NA){
  library(scales)
  library(reshape2)
  library(ggplot2)
  library(grid)
  library(gridExtra)
  
  if (is.na(eps)==FALSE){data <- c(data,eps)}
  transform <- percent_return(data)
  
  meltframe <- melt(transform,id = "date")
  shortframe <- melt(short, id = "date")
  if (is.na(start) == TRUE) {
    start = min(meltframe$date)
  }
  if (is.na(end) == TRUE) {
    end = max(meltframe$date)
  }
  plot1 <- ggplot(data=meltframe, aes(x=date, y=value, group=variable, color = variable)) + geom_line(aes(group=variable)) + geom_point() + scale_x_date(labels = date_format("%y/%m"),limits = c(as.Date(start),as.Date(end))) + scale_y_continuous(labels=percent) +ylab("Percent Increase - Adjusted to most recent value")
  plot2 <- ggplot(data=shortframe, aes(x=date, y=value, group=variable, color = variable)) + geom_line(aes(group=variable)) + scale_x_date(labels = date_format("%y/%m"),limits = c(as.Date(start),as.Date(end))) + scale_y_continuous(labels=percent) + ylab("FINRA TRF Short Interest")
  if (is.na(short) == TRUE) {
    grid.arrange(plot1)
  }
  else {
    grid.arrange(plot1, plot2, ncol = 1, heights = c(2, 1))  
  }
  
}
red_flags <- function(data){
  library(tseries)
  library(ggplot2)
  library(plotly)
  

  transform2 <- percent_return(data)
  bounds<- list()
  focus <- list()
  for (i in 2:length(transform2)) {
    bounds[[i-1]] <- maxdrawdown(na.omit(transform2[,i]))
    focus[[i-1]] <- transform2[bounds[[i-1]]$from:bounds[[i-1]]$to,c(1,i)]
    colnames(focus[[i-1]])<- colnames(transform2[c(1,i)])
  }
  frame <- Reduce(function(x, y) merge(x, y, by="date", all=TRUE), focus)
  meltframe <- melt(frame,id="date")
  plot <- ggplot(data=meltframe, aes(x=date, y=value, group=variable, color = variable)) + geom_line(aes(group=variable)) + geom_point() + ylab('Percent Increase: Adjusted to most recent value')
  return(plot)
} 

var <-function(data) {
  library(tseries)
  library(ggplot2)
  library(reshape2)
  
  transform <- daily_percent(data)
  meltframe <- melt(transform, id="date")
  plot1 <- ggplot(meltframe, aes(x=value)) + geom_density(aes(fill=factor(variable))) + xlab("Daily percent changes in price") + scale_x_continuous(labels=percent)
  
  dens <- lapply(na.omit(transform[,-1]), density)
  df <- lapply(dens,function(frame) {data.frame(x=frame$x, y=frame$y)})
  probs <- c(0.01, 0.05, 0.95, 0.99)
  quantiles <- lapply(transform[,-1],function(frame){quantile(frame, prob=probs,na.rm=TRUE)})
  for (i in 1:length(df)) {
    df[[i]]$quant <- factor(findInterval(df[[i]]$x,quantiles[[i]]),labels = c('0%-1%','1%-5%','5%-95%','95%-99%','99%-100%'))
    }
  plot2 <- lapply(df, function(frame) {})
  for (i in 1:length(df)) {
    plot2[[i]] <- ggplot(df[[i]], aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + scale_fill_brewer(guide="legend") + ggtitle(names(df[i])) + theme(legend.position = "top") + xlab("Daily percent changes in price") + scale_x_continuous(labels=percent)
  }
  return(multiplot(plot1,plot2[[1]],plot2[[2]],layout = matrix(c(1,1,2,3), nrow=2, byrow=TRUE)))
  }

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
