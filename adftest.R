#This is test one.adf. It performs a stationarity on returns of a stock and on log of returns of a stock.
#When a low p-value is reached, stationarity can be proven. Regressions are made for stocks with 
#stationarity. 
library(tseries)
start = as.integer(readline(prompt="start: "))
end <- c(length(test[,2]),0.75*length(test[,2])+start,0.50*length(test[,2])+start,0.25*length(test[,2])+start,0.1*length(test[,2])+start,0.05*length(test[,2])+start)

returns_result <- list()
log_returns_result <- list()
returns_regression <- list()
log_returns_regression <- list()

transform1 <- diff(test[,2])/test[-length(test[,2]),2]
transform2 <- log(1+ diff(test[,2])/test[-length(test$V2),2])

for (i in 1:length(end)) {
  name <- paste(i)
  
  returns_result[[name]] <- adf.test(transform1[start:(end[i]-1)], alternative = 'stationary', 6*trunc(((length(test[start:end[i],2])/100))^(1/4) ))
  log_returns_result[[name]] <- adf.test(transform2[start:(end[i]-1)], alternative = 'stationary', 6*trunc(((length(test[start:end[i],2])/100))^(1/4) ))
  
  par(mfrow=c(2,1))  
  plot(test$V1[start:end[i]],transform1[start:end[i]],main="Return of Stock", ylab = "times Return")
  if (returns_result[[name]]$p.value < 0.07) {
    returns_regression[[name]] <-lm(as.integer(transform1[start:end[i]])~test$V1[start:end[i]])
    abline(returns_regression[[name]])  
  }  
  
  plot(test$V1[start:end[i]],transform2[start:end[i]],main="Returns of Stock", ylab = "log of times Return")
  if (log_returns_result[[name]]$p.value < 0.07) {
    log_returns_regression[[name]] <- lm(as.integer(transform2[start:end[i]])~test$V1[start:(end[i])])
    abline(log_returns_regression[[name]])
  }
  }

