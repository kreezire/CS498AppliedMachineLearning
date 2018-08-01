hw5q7_9<-function(){
 data<-read.csv("brunhild.txt", header = TRUE, sep = "\t")
 logData<-log(data)
 model<-lm(logData$Sulfate ~ logData$Hours)
 x<-as.vector(1:1000)
 #print(x)

  logy<- model[1]$coefficient[2] * log(x) + model[1]$coefficient[1]
 plot(logData$Hours,logData$Sulfate )
 lines(log(x), logy)
 plot(data$Hours,data$Sulfate)
 lines(x, exp(logy))
 fittedValue<- model[1]$coefficient[2] * logData$Hours + model[1]$coefficient[1]
 residual<-logData$Hours-fittedValue
 plot(fittedValue, residual)
 plot(exp(fittedValue),exp(residual))
 
 
 #abline(model)
}