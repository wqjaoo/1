Accuracy<-function(x,y,...){
  x <- as.matrix(x)
  y <- as.matrix(y)
  cc<-(2*sd(y)*sd(x)*(cor(y,x)))/(var(y)+var(x)+(mean(y)-mean(x))^2)
  r2 <- round(summary(lm(y~x))$r.squared,2)
  n <- nrow(x)
  me<-mean(y-x)
  SDE <- sd(y - x)
  SD <- sd(x)
  residual <- x-y
  m <- t(residual) %*% residual  
  rmse <- round(sqrt(m/n),2)
  rpd <- round(SD/rmse,2)
  rpiq <- round((quantile(x,0.75)-quantile(x,0.25))/rmse,2)
  return(data.frame(cc=cc, R2=r2,RMSE=rmse, RPIQ=rpiq,ME=me,SDE=SDE))
}


