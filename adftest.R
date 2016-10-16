library(tseries)
start = as.integer(readline(prompt="start: "))
#end = as.integer(readline(prompt="end: "))
end <- c(length(test$V2),0.75*length(test$V2),0.50*length(test$V2),0.25*length(test$V2),0.1*length(test$V2),0.05*length(test$V2))

price_result <- list()
returns_result <- list()
price_regression <- list()
returns_regression <- list()


for (i in 1:length(end)) {
  name <- paste(i)
  tmp1 <- adf.test(test$V2[start:end[i]], alternative = 'stationary', 6*trunc(((length(test$V2[start:end[i]])/100))^(1/4) ))
  tmp2 <- adf.test(test$V2[start:end[i]]/test$V2[end[i]], alternative = 'stationary', 6*trunc(((length(test$V2[start:end[i]])/100))^(1/4) ))
  
  price_result[[name]] <- tmp1
  returns_result[[name]] <- tmp2
  
  par(mfrow=c(2,1))  
  plot(test$V1[start:end[i]],test$V2[start:end[i]],main="Price of Stock")
  if (tmp1$p.value < 0.07) {
    tmp3 <-lm(as.integer(test$V2[start:end[i]])~test$V1[start:end[i]])
    price_regression[[name]] <-tmp3
    abline(tmp3)  
  }  
  
  plot(test$V1[start:end[i]],test$V2[start:end[i]]/test$V2[end[i]],main="Returns of Stock")
  if (tmp2$p.value < 0.07) {
      tmp4 <- lm(as.integer(test$V2[start:end[i]]/test$V2[end[i]])~test$V1[start:end[i]])
      returns_regression[[name]] <- tmp4
      abline(tmp4)
  }
  }

