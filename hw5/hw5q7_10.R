hw5q7_10<-function(){
  data<-read.csv("physical.txt", header = TRUE, sep = "\t")
  print(data)
  logData<-log(data)
  model<-lm(Mass ~ Fore +Bicep+Chest+ Neck+Shoulder+Waist+Height+Calf+Thigh+Head, data = data)
  #print(model)
  fitted<-predict(model, data)
  #print(fitted)
  residual<-data$Mass - fitted
  plot(fitted, residual)
  original<-(data$Mass)
  data$Mass <- (data$Mass)^1/3
  model<-lm(Mass ~ Fore +Bicep+Chest+ Neck+Shoulder+Waist+Height+Calf+Thigh+Head, data = data)
  cuberootfitted<-predict(model, data)
  residual<-data$Mass - cuberootfitted
  plot(cuberootfitted, residual)
  cuberootfitted_original<-cuberootfitted^3
  residual<-original - cuberootfitted_original
  plot(cuberootfitted_original, residual)
  
  
  #x<-as.vector(1:1000)
  #print(x)
  
  #logy<- model[1]$coefficient[2] * log(x) + model[1]$coefficient[1]
  #plot(logData$Hours,logData$Sulfate )
  #lines(log(x), logy)
  #plot(data$Hours,data$Sulfate)
  #lines(x, exp(logy))
  #fittedValue<- model[1]$coefficient[2] * logData$Hours + model[1]$coefficient[1]
  #residual<-logData$Hours-fittedValue
  #plot(fittedValue, residual)
  #plot(exp(fittedValue),exp(residual))
  
  
  #abline(model)
}