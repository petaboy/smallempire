## app.R ##
library(shinydashboard)
library(ggplot2)
library(tseries)
library(scales)
library(grid)
library(gridExtra)
library(Quandl,tseries)
library(reshape2)

Quandl.api_key("enter Quandl API key here")

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(
    textInput("stock","Add stocks to compare", "AAPL,GOOG"),
    textInput("index","Add NASDAQ indices to compare", "NQUSB"),
    checkboxInput("eps","Net Earnings (not available for all stocks)"),
    checkboxInput("short","Short Interest"),
    submitButton("Apply Changes")
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(
        h1("Price, Net Income and Short Interest trends"),
        p("The trends among stock prices, eps and short interests can all be analyzed here. The prices and net income (if available) are all divided by the most recent price or net income, respectively. The resulting values are all given as percent of the most recent value: This transformation also allows for a more straightforward comparison of price movements of different stocks because base prices of instruments are already taken into account. "),
        p("Short interests, if available, can be plotted against the eps and stock price trends. The short interest is the volume of stocks shorted divided by total traded volume of stock. Both of these values are taken from the FINRA Trading Reporting Facility (TRF). Reported as a percent, a relatively high short interest on a given day, may indicate an anticipation of breakout, whether positive or negative."),
        
        
        plotOutput("test_two", height = 500),
        wellPanel(
          textInput("from_date", "Date from:", "2010-01-01"),
          textInput("to_date", "Date to:", Sys.Date()),
          submitButton("Apply Changes")
        )
      ),
      box(
        h1("Max Drawdown analysis"),
        p("This module calculates and plots the max drawdown of a given stock or index. The max drawdown or max loss statistic is defined as the maximum value drop after one of the peaks. For financial instruments the max drawdown represents the worst investment loss for a buy-and-hold strategy invested in the stock. The maxdrawdown function, as given in the CRAN package, 'tseries' is used."),
        
        plotOutput("test_three",height=500),
        wellPanel(
          textInput("from_date2", "Date from:", "2010-01-01"),
          textInput("to_date2", "Date to:", Sys.Date()),
          submitButton("Apply Changes")
        )
      ),
    fluidRow(


      ),
      box(
        h1("Daily Value-at-Risk assessment"),
        p("Here, the daily percent changes in stock (or index) prices are plotted in a density curve. The graph on top plots the density curves of all the requested stocks and indices. A wide density curve indicates a higher variability in daily stock price movements, while a narrow density curve indicates a lower variability in daily stock price movements: Large cap stocks in general have a lower variability in daily price movements than that of small cap stocks."),
        p("The second and third plots are the individual density curves for the first two stocks requested on the sidebar. By design, this program only plots the density curves of the first two stocks (indices and other requested stocks won't be plotted). These density curves however are more detailed: The curve is divided into regions as shown in the legend. The area covered by the 5%-95% region correlate to the 90% of expected daily returns. Area covered by the 0%-5% and 95%-100% correspond to the tail regions, of low and high daily returns respectively. Let's say for example that for a given stock, a daily return of -5% falls within the 0-5% area (or right on the 5% area line). If future daily returns are extrapolated (assuming conditions are met) from this density curves, then, we can say that on any given day, there is at least a 5% chance that the daily return of that stock will be <5% or in a span of 20 days, there will be at least one day where the daily return of the stock will be <5%."),
        
        
        plotOutput("test_four",height=500)
      ),
      box(
        h1( "Correlation and Cointegration"),
        p("This module first performs a linear regression on the daily percent change in the price of each of the stock to another stock or index.  This regression's r-squared value and beta (also known as coefficient) are outputted below: A relatively high r-squared shows a strong correlation between two components, which validates the beta for use in further analysis: a beta higher than one indicates that the component's price volatility may be higher than that of the index. A beta between one and zero indicates that that component's price volatility is lower than that of an index."),
        p("The second part of this module performs augmented-dickey-fuller test, with a lag of zero, to check for stationarity between the price of a stock to that of another stock or index.  A low p-value (<0.05) indicates a high certainty of stationarity between the two components: This further indicates that the two components may be cointegrated. This information is useful for hedges and spreads."),
        
        
        tags$style(type='text/css', '#test_one {background-color: rgba(100,100,100,100); color: white;}'),
        verbatimTextOutput("test_one")
      )
        



          
      )
      
      
      )
  )



server <- function(input, output) {

  output$test_four<-renderPlot({
    ticker <- unlist(strsplit(input$stock,","))
    index_ticker <- unlist(strsplit(input$index,","))
    stock <- import(ticker,"stock")
    index <- import(index_ticker,"index")
    if (is.na(data) == FALSE) {data <- c(stock,index)}
    
    return(var(data))
  })
  output$test_three <-renderPlot({
    ticker <- unlist(strsplit(input$stock,","))
    index_ticker <- unlist(strsplit(input$index,","))
    stock <- import(ticker,"stock")
    index <- import(index_ticker,"index")
    if (is.na(data) == FALSE) {data <- c(stock,index)}
    
    return(red_flags(data,start=input$from_date2,end=input$to_date2))
  })
  
  output$test_two <- renderPlot({  
    ticker <- unlist(strsplit(input$stock,","))
    index_ticker <- unlist(strsplit(input$index,","))
    stock <- import(ticker,"stock")
    index <- import(index_ticker,"index")
    if (is.na(data) == FALSE) {data <- c(stock,index)}
    
    eps <- NA
    short <- NA
    if(input$eps == TRUE) {
      eps <- import(ticker, "eps")
    } 
    if (input$short == TRUE) {
      short <-import(ticker, "short")
    }
    return(trend_plot(data,eps, short,start=input$from_date,end=input$to_date))
  })
  

  
  output$test_one <- renderPrint({
    ticker <- unlist(strsplit(input$stock,","))
    index_ticker <- unlist(strsplit(input$index,","))
    stock <- import(ticker,"stock")
    index <- import(index_ticker,"index")
    if (is.na(data) == FALSE) {data <- c(stock,index)}
    
    return(cointegration(stock,index))})

##Functions
  
  #Primary Functions
  import <- function(ticker,type) {
    
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
  
  red_flags <- function(data,start,end){
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
    plot <- ggplot(data=meltframe, aes(x=date, y=value, group=variable, color = variable)) + geom_line(aes(group=variable)) + geom_point() + scale_x_date(labels = date_format("%y/%m"),limits = c(as.Date(start),as.Date(end))) + ylab('Percent Increase: Adjusted to most recent value') + scale_y_continuous(labels=percent)
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
  #Secondary functions
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
       b <-cbind('date'=x[-1,1],data.frame(diff(as.matrix(x[,-1]))/as.matrix(x[-1,-1])))
       colnames(b) <-colnames(x)
       return(b)
     })
     frame <- Reduce(function(x, y) merge(x, y, by="date", all=TRUE), a)
     return(frame)
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
}


shinyApp(ui, server)

