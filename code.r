#library(tidyverse) # utility functions
library(rpart) # for regression trees
library(randomForest) # for random forests

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
preds <- colnames(Dlearn)

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
    
    #Perform 10 fold cross validation
    for(i in 1:10){
      #Segement your data by fold using the which() function 
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
brute_force_lm <- function(){
  pwset <- powerset(colnames(dlearn_transformed[c(-12,-13,-14,-18)]))
  
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
    tmp_score <- crossvalid(function(x) glm(as.formula(formula), data=x, family=binomial), 10, TRUE)
    
    if(tmp_score > best[1]){
      best <- c(tmp_score, formula)
      print(best)
    }
  }
  write.csv(best, 'best.csv')
  submit_prediction((function(x) glm(as.formula(best[2]), data=x)))
}

# brute_force_lm()

submit_prediction(function(x) glm(as.formula(Transaction ~ CategoryI + CategoryI_Duration + CategoryII + CategoryII_Duration + CategoryIII + CategoryIII_Duration + Bounce_Rate + Exit_Rate + Page_Value), data=x, family=binomial))