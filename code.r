#library(tidyverse) # utility functions
library(rpart) # for regression trees
library(randomForest) # for random forests
library(rpart.plot)
library(dplyr)
library(ipred)
library(plyr)
library(caret)

###check if needed
library(ggplot2)
library(gmodels)
library(corrplot)
library(caret)
library(fpc)
library(data.table)
library(cluster)


dlearn <- read.csv('data/Dlearn.csv')
sample <- read.csv('data/sample.csv')
xtest <- read.csv('data/Xtest.csv')

dlearn_transformed <- transform(dlearn,
                                CategoryI=as.integer(CategoryI),                                #1
                                CategoryI_Duration=as.numeric(CategoryI_Duration),              #2
                                CategoryII=as.integer(CategoryII),                              #3
                                CategoryII_Duration=as.numeric(CategoryII_Duration),            #4
                                CategoryIII=as.integer(CategoryIII),                            #5
                                CategoryIII_Duration=as.numeric(CategoryIII_Duration),          #6
                                Bounce_Rate=as.numeric(Bounce_Rate),                            #7
                                Exit_Rate=as.numeric(Exit_Rate),                                #8
                                Page_Value=as.numeric(Page_Value),                              #9
                                SpecialDay=as.numeric(SpecialDay),                              #10
                                Month=as.factor(Month),                                         #11
                                OS=as.factor(OS),                                               #12
                                Browser=as.factor(Browser),                                     #13
                                Region=as.factor(Region),                                       #14
                                TrafficType=as.factor(TrafficType),                             #15
                                VisitorType=as.factor(VisitorType),                             #16
                                Weekend=as.factor(Weekend),                                     #17
                                Transaction=as.logical(Transaction))                            #18
preds <- colnames(dlearn)

submit_prediction <- function(model){
  sub.fit <- model(dlearn)
  sub.pred <- predict(sub.fit, newdata=xtest)
  sub.pred <- round_pred(sub.pred)
  submission <- data.frame("Id" = xtest$id, "Predicted"=sub.pred)
  write.csv(submission, 'submission.csv', row.names=FALSE)
}

crossvalid <- function(model,nrep=1, print=FALSE){ #https://stats.stackexchange.com/a/105839
  means_f1s <- c()
  for(rep in 1:nrep){
    #Shuffles data
    cross_data<-dlearn_transformed[sample(nrow(dlearn_transformed)),]
    
    #Create 10 folds
    folds <- cut(seq(1,nrow(cross_data)),breaks=10,labels=FALSE)
    
    #F1 score vector
    f1s <- c()
    
    # main loop
    for(i in 1:10){
      #Segement data
      testIndexes <- which(folds==i,arr.ind=TRUE)
      testData <- cross_data[testIndexes, ]
      trainData <- cross_data[-testIndexes, ]
      
      #Train our model and then predict using it
      cross.fit <- model(testData)
      cross.pred <- predict(cross.fit, newdata=testData)
      
      #Round our prediction
      cross.pred <- round_pred(cross.pred)
      
      #Compute F1 score
      fold.f1 <- f1score(testData, cross.pred)
      f1s <- c(f1s,fold.f1)
    }
    means_f1s <- c(means_f1s, mean(f1s))
  }
  if(print==TRUE){print(mean(means_f1s))}
  return(mean(means_f1s))
}

round_pred <- function(pred){
  for(j in 1:length(pred)){
    if (pred[j]<0.5){
      pred[j]=0
    }
    else{
      pred[j]=1
    }
  }
  return(pred)
}

f1score <- function(data, pred) { #https://en.wikipedia.org/wiki/F1_score
  #true positive
  tp <- 0
  for(i in 1:nrow(data)){
    if(data$Transaction[i] == 1 && pred[i] == 1){
      tp <- tp + 1
    }
  }
  #true positive + false positive
  tpfp <- sum(pred)
  
  #true positive + false negative
  tpfn <- sum(data$Transaction)
  
  recall <- tp / tpfn
  precision <- tp / tpfp
  
  f1 <- 2*(precision*recall)/(precision+recall)
  if(is.nan(f1)){
    f1 <- 0
  }
  return(f1)
}

######################################## WORKSPACE ######################################## 
#Linear regression
linear_fit <- function(data){
  test.fit <- lm(Transaction ~ CategoryI_Duration + CategoryII + CategoryII_Duration + CategoryIII + CategoryIII_Duration + Exit_Rate + Page_Value + SpecialDay + Month + OS + TrafficType + VisitorType + Weekend, data=data)
  return(test.fit)
}

powerset = function(s){ #https://stackoverflow.com/questions/18715580/algorithm-to-calculate-power-set-all-possible-subsets-of-a-set-in-r
  len = length(s)
  l = vector(mode="list",length=2^len) ; l[[1]]=numeric()
  counter = 1L
  for(x in 1L:length(s)){
    for(subset in 1L:counter){
      counter=counter+1L
      l[[counter]] = c(l[[subset]],s[x])
    }
  }
  return(l)
}

#brute force for best linear model (takes time)
brute_force_glm <- function(){
  pwset <- powerset(colnames(dlearn_transformed[c(-12,-13,-18)]))
  
  best <- c(0,"")
  
  for(i in 2:length(pwset)) {
    if(i==2 || i%%1000==0){
      print(paste(i,'/',length(pwset)))
    }
    formula <- "Transaction ~"
    for(j in 1:length(pwset[[i]])){
      if(j==length(pwset[[i]])){
        formula <- paste(formula,pwset[[i]][j])
      }else{
        formula <- paste(formula,pwset[[i]][j],"+")
      }
      
    }
    tmp_score <- crossvalid(function(x) glm(as.formula(formula), data=x, family=binomial), 5, TRUE)
    
    if(tmp_score > best[1]){
      best <- c(tmp_score, formula, best)
      print(best[2])
    }
  }
  write.csv(best, 'best.csv')
  submit_prediction((function(x) glm(as.formula(best[2]), data=x, family=binomial)))
}

brute_force_lm_higher_dim <- function(){
  pwset <- powerset(c("CategoryI",
                      "I(CategoryI^2)",
                      "CategoryI_Duration",
                      "I(CategoryI_Duration^2)",
                      "CategoryII",
                      "I(CategoryII^2)",
                      "CategoryII_Duration",
                      "I(CategoryII_Duration^2)",
                      "CategoryIII",
                      "I(CategoryIII^2)",
                      "CategoryIII_Duration",
                      "I(CategoryIII_Duration^2)",
                      "Bounce_Rate",
                      "I(Bounce_Rate^2)",
                      "Exit_Rate",
                      "I(Exit_Rate^2)",
                      "Page_Value",
                      "I(Page_Value^2)",
                      "Month"))
  
  best <- c(0,"")
  
  for(i in sample(2:length(pwset))) {
    if(i==2 || i%%1000==0){
      print(paste(i,'/',length(pwset)))
    }
    formula <- "Transaction ~"
    for(j in 1:length(pwset[[i]])){
      if(j==length(pwset[[i]])){
        formula <- paste(formula,pwset[[i]][j])
      }else{
        formula <- paste(formula,pwset[[i]][j],"+")
      }
      
    }
    tmp_score <- crossvalid(function(x) lm(as.formula(formula), data=x), 5, TRUE)
    
    if(tmp_score > best[1]){
      best <- c(tmp_score, formula, best)
      print(best[2])
      write.csv(best, 'best.csv')
      submit_prediction((function(x) lm(as.formula(best[2]), data=x)))
    }
  }
}

dlearn_transformed$OS <- factor(dlearn_transformed$OS, order = TRUE, levels = c(6,3,7,1,5,2,4,8))
dlearn_transformed$Browser <- factor(dlearn_transformed$Browser, order = TRUE, levels = c(9,3,6,7,1,2,8,11,4,5,10,13,12))
dlearn_transformed$Region <- factor(dlearn_transformed$Region, order = TRUE, levels = c(8,6,3,4,7,1,5,2,9))
dlearn_transformed$TrafficType <- factor(dlearn_transformed$TrafficType, order = TRUE, levels = c(12,15,17,18,13,19,3,9,1,6,4,14,11,10,5,2,20,8,7,16))

tree.test <- rpart(Transaction ~ ., data=dlearn_transformed, method="class")
rpart.plot(tree.test)

tree.pred <- predict(tree.test, newdata=dlearn_transformed, type="class")

summary(tree.pred)

submit_prediction(function(x) rpart(Transaction ~ ., data=x, method="class"))
