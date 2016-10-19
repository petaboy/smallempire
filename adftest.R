library(tseries)
start = as.integer(readline(prompt="start: "))
#end = as.integer(readline(prompt="end: "))
end <- c(length(test$V2),0.75*length(test$V2),0.50*length(test$V2),0.25*length(test$V2),0.1*length(test$V2),0.05*length(test$V2))

returns_result <- list()
log_returns_result <- list()
returns_regression <- list()
log_returns_regression <- list()


for (i in 1:length(end)) {
  name <- paste(i)
  transform1 <- test$V2[start:end[i]]/test$V2[end[i]]
  transform2 <- log(test$V2[start:end[i]]/test$V2[end[i]])
  tmp1 <- adf.test(transform1, alternative = 'stationary', 6*trunc(((length(test$V2[start:end[i]])/100))^(1/4) ))
  tmp2 <- adf.test(transform2, alternative = 'stationary', 6*trunc(((length(test$V2[start:end[i]])/100))^(1/4) ))
  returns_result[[name]] <- tmp1
  log_returns_result[[name]] <- tmp2
  
  par(mfrow=c(2,1))  
  plot(test$V1[start:end[i]],transform1,main="Return of Stock", ylab = "times Return")
  if (tmp1$p.value < 0.07) {
    tmp3 <-lm(as.integer(transform1)~test$V1[start:end[i]])
    returns_regression[[name]] <-tmp3
    abline(tmp3)  
  }  
  
  plot(test$V1[start:end[i]],transform2,main="Returns of Stock", ylab = "log of times Return")
  if (tmp2$p.value < 0.07) {
      tmp4 <- lm(as.integer(transform2)~test$V1[start:end[i]])
      log_returns_regression[[name]] <- tmp4
      abline(tmp4)
  }
  }

