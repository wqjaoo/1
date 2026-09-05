
setwd('D:/R/模型')
library(readxl)
library(xlsx)
library(caret)
library(e1071)
library("Matrix")
library(xgboost)
library(plyr)
#####读入数据####################################

data1 <- read_excel('地上碳密度数据集.xlsx', sheet=1, col_names=TRUE)
data <- data1[, -c(1,6)]

###########随机划分数据集，建模集和验证集#####################
set.seed(3333)
random <- sample(seq(1,nrow(data),1),2492,replace=FALSE)
data_train <- as.matrix(data[random, ])
data_test <- as.matrix(data[-random, ])


####建模型调参####
## fit models in a loop
## Generic settings for caret:
ctrl <- trainControl(method="cv", number=10)

cubist.tuneGrid <- expand.grid(committees=c(2,3,4,5,10,15,50), neighbors=c(2,4,6,9))

rf.tuneGrid <- expand.grid(mtry = seq(2,18,by=2))

svm.tuneGrid <- expand.grid(ranges = list(epsilon = seq(0,1,0.1,),cost = 2^(2:9)))

gb.tuneGrid <- expand.grid(eta= c(0.3,0.4,0.5), nrounds=c(50,100,150),
                           max_depth=3:10, gamma=0, colsample_bytree=0.8,
                           min_child_weight=10, subsample=1)


####建空集用于存储后面数据####
val.pred.cubist <- c()
cubist.cal.stats<-c();cubist.oob.stats  <-c();cubist.val.stats<-c()

val.pred.rf <- c()
rf.cal.stats<-c();rf.oob.stats  <-c();rf.val.stats<-c()

val.pred.svm <- c()
svm.cal.stats<-c();svm.oob.stats  <-c();svm.val.stats<-c()

val.pred.gb <- c()
gb.cal.stats<-c();gb.oob.stats  <-c();gb.val.stats<-c()



##### bootstrap for uncertainty analysis####
for (boots in 1:30){
  set.seed(boots)
  bin <- sample(1:nrow(data_train ),nrow(data_train ),replace=TRUE)
  xb <- data_train [bin,]
  xoob <- data_train [-unique(bin),]
  ################Cubist#############################################################################
  mcubist <- caret::train(AGBC~., data=xb, importance=TRUE, method="cubist", trControl=ctrl, tuneGrid=cubist.tuneGrid)
  
  oob.cubist.p <- predict(mcubist, xoob[,-1])
  cal.cubist.p<-predict(mcubist, xb[,-1])
  val.cubist.p<- predict(mcubist, data_test[,-1]) 
  val.pred.cubist<-cbind(val.pred.cubist,val.cubist.p)
  
  ################RF###################################################################################33
  mrf <- caret::train(AGBC~., data=xb, importance=TRUE, method="rf", trControl=ctrl, tuneGrid=rf.tuneGrid)
  
  oob.rf.p <- predict(mrf, xoob[,-1])
  cal.rf.p<-predict(mrf, xb[,-1])
  val.rf.p<- predict(mrf, data_test[,-1]) 
  val.pred.rf<-cbind(val.pred.rf,val.rf.p)
  
  ################SVM###################################################################################33
  msvm <- e1071::svm(AGBC~., data=xb, importance=TRUE, method="svm", trControl=ctrl, tuneGrid=svm.tuneGrid,kernel = "radial")
  
  oob.svm.p <- predict(msvm, xoob[,-1])
  cal.svm.p<-predict(msvm, xb[,-1])
  val.svm.p<- predict(msvm, data_test[,-1])
  val.pred.svm<-cbind(val.pred.svm,val.svm.p)
  
  ################XGBoost##
  mXGB <- caret::train(AGBC~., data=xb, importance=TRUE, method="xgbTree", trControl=ctrl, tuneGrid=gb.tuneGrid)
  
  oob.gb.p <- predict(mXGB, xoob[,-1])
  cal.gb.p<-predict(mXGB, xb[,-1])
  val.gb.p<- predict(mXGB, data_test[,-1]) 
  val.pred.gb<-cbind(val.pred.gb,val.gb.p)
  
  
  ##############评价指标######################3
  {source("Accuracy.R")
    
    
    cubist.stats_cal<-Accuracy(xb[,1],cal.cubist.p)
    cubist.stats_oob<-Accuracy(xoob[,1],oob.cubist.p)
    cubist.stats_val<-Accuracy(data_test[,1],val.cubist.p)
    
    cubist.cal.stats <- rbind(cubist.cal.stats, cubist.stats_cal)
    cubist.oob.stats <- rbind(cubist.oob.stats, cubist.stats_oob)
    cubist.val.stats <- rbind(cubist.val.stats, cubist.stats_val)
    
    
    rf.stats_cal<-Accuracy(xb[,1],cal.rf.p)
    rf.stats_oob<-Accuracy(xoob[,1],oob.rf.p)
    rf.stats_val<-Accuracy(data_test[,1],val.rf.p)
    
    rf.cal.stats <- rbind(rf.cal.stats, rf.stats_cal)
    rf.oob.stats <- rbind(rf.oob.stats, rf.stats_oob)
    rf.val.stats <- rbind(rf.val.stats, rf.stats_val)
    
    svm.stats_cal<-Accuracy(xb[,1],cal.svm.p)
    svm.stats_oob<-Accuracy(xoob[,1],oob.svm.p)
    svm.stats_val<-Accuracy(data_test[,1],val.svm.p)
    
    svm.cal.stats <- rbind(svm.cal.stats, svm.stats_cal)
    svm.oob.stats <- rbind(svm.oob.stats, svm.stats_oob)
    svm.val.stats <- rbind(svm.val.stats, svm.stats_val)
    
    gb.stats_cal<-Accuracy(xb[,1],cal.gb.p)
    gb.stats_oob<-Accuracy(xoob[,1],oob.gb.p)
    gb.stats_val<-Accuracy(data_test[,1],val.gb.p)
    
    gb.cal.stats <- rbind(gb.cal.stats,gb.stats_cal)
    gb.oob.stats <- rbind(gb.oob.stats,gb.stats_oob)
    gb.val.stats <- rbind(gb.val.stats,gb.stats_val)
    
  
  }
}



#验证集结果
write.xlsx( val.pred.cubist,file = "Prediction Result.xlsx",sheetName = 'cubist' )
write.xlsx( val.pred.rf,file = "Prediction Result.xlsx",sheetName = 'rf' ,append=T)
write.xlsx( val.pred.svm,file = "Prediction Result.xlsx",sheetName = 'svm' ,append=T)
write.xlsx( val.pred.gb,file = "Prediction Result.xlsx",sheetName = 'gb' ,append=T)
write.xlsx(data_test[,1],file = "Prediction Result.xlsx",sheetName = 'Actual value',append=T)

#精度
write.xlsx(cubist.cal.stats,file = "Precision.xlsx",sheetName = 'cub_cal')
write.xlsx(cubist.val.stats,file = "Precision.xlsx",sheetName = 'cub_val' ,append=T)
write.xlsx(cubist.oob.stats,file = "Precision.xlsx",sheetName = 'cub_oob' ,append=T)
write.xlsx(rf.cal.stats,file = "Precision.xlsx",sheetName = 'rf_cal',append=T)
write.xlsx(rf.val.stats,file = "Precision.xlsx",sheetName = 'rf_val' ,append=T)
write.xlsx(rf.oob.stats,file = "Precision.xlsx",sheetName = 'rf_oob' ,append=T)
write.xlsx(svm.cal.stats,file = "Precision.xlsx",sheetName = 'svm_cal',append=T)
write.xlsx(svm.val.stats,file = "Precision.xlsx",sheetName = 'svm_val' ,append=T)
write.xlsx(svm.oob.stats,file = "Precision.xlsx",sheetName = 'svm_oob' ,append=T)
write.xlsx(gb.cal.stats,file = "Precision.xlsx",sheetName = 'gb_cal',append=T)
write.xlsx(gb.val.stats,file = "Precision.xlsx",sheetName = 'gb_val' ,append=T)
write.xlsx(gb.oob.stats,file = "Precision.xlsx",sheetName = 'gb_oob' ,append=T)

