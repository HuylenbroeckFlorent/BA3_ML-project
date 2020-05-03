#library(tidyverse) # utility functions
library(rpart) # for regression trees
library(randomForest) # for random forests

dlearn <- read.csv('data/Dlearn.csv')
sample <- read.csv('data/sample.csv')
xtest <- read.csv('data/Xtest.csv')

lm.fit = lm(Transaction ~ CategoryIII, data=Dlearn)
summary(lm.fit)
predicted <- predict(lm.fit, newdata=xtest)

submission <- data.frame('Id' = xtest$id, 'Predicted'=predicted)

for(i in 1:length(submission$Predicted)){
  if (submission$Predicted[i]<0.5){
    submission$Predicted[i]=0
  }
  else{
    submission$Predicted[i]=1
  }
}

write.csv(submission, 'submission.csv')