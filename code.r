#library(tidyverse) # utility functions
library(rpart) # for regression trees
library(randomForest) # for random forests

dlearn <- read.csv('data/Dlearn.csv')
sample <- read.csv('data/sample.csv')
xtest <- read.csv('data/Xtest.csv')

lm.fit = lm(Transaction ~ CategoryIII, data=Dlearn)
summary(lm.fit)
predicted <- predict(lm.fit, newdata=xtest)
predicted
for(i in 1:length(predicted)){
  if (predicted[i]<0.5){
    predicted[i]=0
  }
  else{
    predicted[i]=1
  }
}

predicted

submission <- data.frame('id' = xtest$id, 'Transaction' = predicted)

write.csv(predicted, 'submission.csv')


crossvalid <- function (set,predictor){
  set=set[sample(nrow(set)),]
  
  folds <- cut(seq(1,nrow(Dtrain)),breaks=10,labels=FALSE)
  
  errors=c()
  
  for(i in 1:10){
    
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- set[testIndexes, ]
    trainData <- set[-testIndexes, ]
    
    a <- "y~"
    for(i in 1:length(predictor)){
      if(i!=length(predictor)){
        a <- paste(a,predictor[i],"+") 
      }
      else{
        a <- paste(a,predictor[i])
      }
    }
    
    
    lm.fit=lm(a,data=trainData)
    
    prediction=predict(lm.fit,testData,type="response")
    for(i in 1:length(prediction)){
      prediction[i]=max(min(prediction[i],1-(10^-15)),(10^-15))
    }
    
    currentSum=0
    for(i in 1:nrow(testData)){
      currentSum=currentSum+(testData[i,"y"]*log(prediction[i],base=exp(1))+((1-testData[i,"y"])*log(1-prediction[i],base=exp(1))))
      
    }
    
    currenterr=-(1/nrow(testData))*currentSum        
    #mean((testData$y-predict(lm.fit ,testData))^2)
    errors=c(errors,currenterr)
  }  
  print(mean(errors))
}