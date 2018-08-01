hw5q7_11<-function(){
  library(glmnet)
  data<-read.csv("abalone.data", header = FALSE, sep = ",")
  #print(data$V1)
  model<-lm(V9~V2+V3+V4+V5+V6+V7+V8, data)
  fitted<-predict(model, data)
  residual <- data[,9] - fitted
  #print(fitted)
  #print(residual)
  plot(fitted, residual)
  data[data[,1]=='M',10]=1.0
  #print(data)
  data[data[,1]=='F',10]<-0.0
  data[data[,1]=='I',10]<- -1.0
  #print(data)
  model<-lm(V9~V2+V3+V4+V5+V6+V7+V8+V10, data)
  fitted<-predict(model, data)
  residual <- data[,9] - fitted
  plot(fitted, residual)
  data[,11]<-log(data[,9])
  #print(data)
  model<-lm(V11~V2+V3+V4+V5+V6+V7+V8, data)
  fitted<-predict(model, data)
  residual <- data[,11] - fitted
  plot(fitted, residual)
  model<-lm(V11~V2+V3+V4+V5+V6+V7+V8+V10, data)
  fitted<-predict(model, data)
  residual <- data[,11] - fitted
  plot(fitted, residual)
  #print(as.double( data[,9]))
  #lambdas <- 300:400
  #lambdas <- lambdas/100
  #error<-vector(length = length(lambdas))
  #index<-0
  #index<-index+1
  #foldid=sample(1:10,size=length(data[,11]),replace=TRUE)
  model<- cv.glmnet(as.matrix(data[,c(2:8,10)]), data[,11])#, lambda = lambdas)#, foldid=foldid,alpha=1)#, family="mgaussian")
  fitted<-predict(model,as.matrix(data[,c(2:8,10)]))
  residual <- data[,11] - fitted
  #error[index]<- mean(residual*residual)
  #plot(log(lambdas), error)
  #print((model$cvm))
  plot(model, pch=19,col="blue",xlab="log(Lambda)",ylab=model$name)
 # lines(log(model$lambda), model$cvm, pch=19,col="blue",xlab="log(Lambda)",ylab=model$name)
#  
  #print(model)
}