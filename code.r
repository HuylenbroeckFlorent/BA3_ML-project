#library(tidyverse) # utility functions
library(rpart) # for regression trees
library(randomForest) # for random forests
library(rpart.plot)

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

################ K-mean ################
## 5.1 Converting our Categorical Variables to Ordinal Factors. ##
dlearn_transformed$OS <- factor(dlearn_transformed$OS, order = TRUE, levels = c(6,3,7,1,5,2,4,8))
dlearn_transformed$Browser <- factor(dlearn_transformed$Browser, order = TRUE, levels = c(9,3,6,7,1,2,8,11,4,5,10,13,12))
dlearn_transformed$Region <- factor(dlearn_transformed$Region, order = TRUE, levels = c(8,6,3,4,7,1,5,2,9))
dlearn_transformed$TrafficType <- factor(dlearn_transformed$TrafficType, order = TRUE, levels = c(12,15,17,18,13,19,3,9,1,6,4,14,11,10,5,2,20,8,7,16))

library(plyr)

dlearn_transformed$Month <- factor(dlearn_transformed$Month, order = TRUE, levels =c('Feb', 'Mar', 'May', 'June','Jul', 'Aug', 'Sep','Oct', 'Nov','Dec'))
dlearn_transformed$Month_Numeric <-mapvalues(dlearn_transformed$Month, from = c('Feb', 'Mar', 'May', 'June','Jul', 'Aug', 'Sep','Oct', 'Nov','Dec'), to = c(1,2,3,4,5,6,7,8,9,10))


dlearn_transformed$VisitorType <- factor(dlearn_transformed$VisitorType, order = TRUE, levels = c('Returning_Visitor', 'Other', 'New_Visitor'))
dlearn_transformed$VisitorType_Numeric <-mapvalues(dlearn_transformed$VisitorType, from = c("Returning_Visitor", "Other", "New_Visitor"), to = c(1,2,3))

library(dplyr)

## 5.2 Creating Appropriate Dummy Variables ##
dlearn_transformed <- dlearn_transformed %>%
  mutate(Weekend_binary = ifelse(Weekend == "FALSE",0,1))

## 5.3 Normalizing Numerical Data ##
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

## Creating a copy of the original data.
dlearn_transformed_norm <- dlearn_transformed

## Normalizing our 10 variables.
dlearn_transformed_norm$Administrative <- normalize(dlearn_transformed$Administrative)
dlearn_transformed_norm$Administrative_Duration <- normalize(dlearn_transformed$Administrative_Duration)
dlearn_transformed_norm$Informational <- normalize(dlearn_transformed$Informational_Duration)
dlearn_transformed_norm$Informational_Duration <- normalize(dlearn_transformed$Administrative)
dlearn_transformed_norm$ProductRelated <- normalize(dlearn_transformed$ProductRelated)
dlearn_transformed_norm$ProductRelated_Duration <- normalize(dlearn_transformed$ProductRelated_Duration)
dlearn_transformed_norm$BounceRates <- normalize(dlearn_transformed$BounceRates)
dlearn_transformed_norm$ExitRates <- normalize(dlearn_transformed$ExitRates)
dlearn_transformed_norm$PageValues <- normalize(dlearn_transformed$PageValues)
dlearn_transformed_norm$SpecialDay <- normalize(dlearn_transformed$SpecialDay)

dlearn_transformed_clust <- dlearn_transformed_norm[-c(11,16:19)]

## 5.2 Creating Test and Train Data ##
dlearn_transformed_class <- dlearn_transformed[-c(19:22)]

set.seed(1984)
library(caret)
training <- createDataPartition(dlearn_transformed_class$Revenue, p = 0.8, list=FALSE)

train_data <- dlearn_transformed_class[training,]
test_data <- dlearn_transformed_class[-training,]

## 6. Clustering ##
## 6.1 K-Means Clustering ##
summary(dlearn_transformed_clust)

str(dlearn_transformed_clust)

k_mean_clust <- kmeans(dlearn_transformed_clust, centers = 2, iter.max = 100)

## Size of our clusters
k_mean_clust$size

## Our cluster centers (means)
k_mean_clust$centers

## Between cluster sum of squares
k_mean_clust$betweenss

## Total cluster sum of squares
k_mean_clust$totss

## Whithin clusters sum of squares
k_mean_clust$betweenss / k_mean_clust$totss

t1 <- table(k_mean_clust$cluster, dlearn_transformed_norm$Revenue)
t1

pca_cluster_data <- prcomp(dlearn_transformed_clust[c(1:10)], scale. = TRUE)
plot(pca_cluster_data, main = "Principal Components")

shopper_components_data <- as.data.frame(pca_cluster_data$x)

## Show first two PCs for out shoppers
head(shopper_components_data[1:2], 5)

## Plotting
plot(PC1~PC2, data=shopper_components_data,
     cex = .1, lty = "solid")
text(PC1~PC2, data=shopper_components_data, 
     labels=rownames(dlearn_transformed_clust[c(1:10)]),
     cex=.8)

plot(PC1~PC2, data=shopper_components_data, 
     main= "Online Shopper Intent: PC1 vs PC2 - K-Means Clusters",
     cex = .1, lty = "solid", col=k_mean_clust$cluster)
text(PC1~PC2, data=shopper_components_data, 
     labels=rownames(dlearn_transformed_clust[c(1:10)]),
     cex=.8, col=k_mean_clust$cluster)

presicion_kmeans<- t1[1,1]/(sum(t1[1,]))
recall_kmeans<- t1[1,1]/(sum(t1[,1]))
## Precision
presicion_kmeans

recall_kmeans

F1_kmeans<- 2*presicion_kmeans*recall_kmeans/(presicion_kmeans+recall_kmeans)
F1_kmeans

## 6.2 K-Medoids Clustering ##
k_med_clust <- pam(x = dlearn_transformed_clust, k = 2)

k_med_clust$id.med

k_med_clust$mediods

k_med_clust$objective

k_med_clust$clusinfo

t1b <- table(k_med_clust$clustering, dlearn_transformed_norm$Revenue)
t1b

plot(PC1~PC2, data=shopper_components_data, 
     main= "Online Shopper Intent: PC1 vs PC2 - K-Medoids Clusters",
     cex = .1, lty = "solid", col=k_med_clust$clustering)
text(PC1~PC2, data=shopper_components_data, 
     labels=rownames(dlearn_transformed_clust[c(1:10)]),
     cex=.8, col=k_med_clust$clustering)

presicion_kmed<- t1b[1,1]/(sum(t1b[1,]))
recall_kmed<- t1b[1,1]/(sum(t1b[,1]))
## Precision
presicion_kmed

recall_kmed

F1_kmed<- 2*presicion_kmed*recall_kmed/(presicion_kmed+recall_kmed)
F1_kmed