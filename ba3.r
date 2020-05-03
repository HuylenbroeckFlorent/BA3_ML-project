

#age p-value=0.053
summary(lm(y~age,data=Dtrain))

#age+I(age^2) p-value <2.2e-16
summary(lm(y~age+I(age^2),data=Dtrain))
crossvalid(finalset,c("age","I(age^2)"))


plot(age,y,pch=16,main="Regression non linéaire de y en fonction de age+age^2+age^3+age^4+age^5")
coeff=coefficients(lm(y~age+I(age^2)+I(age^3)+I(age^4)+I(age^5),data=Dtrain))
curve(coeff[1]+coeff[2]*x+coeff[3]*x^2+coeff[4]*x^3+coeff[5]*x^4+coeff[6]*x^5,add=TRUE)

summary(lm(y~age+I(age^2)+I(age^3)+I(age^4)+I(age^5),data=Dtrain))
crossvalid(finalset,c("age","I(age^2)","I(age^3)","I(age^4)","I(age^5)"))

#job (bcp de job >0.05 p value)
summary(lm(y~job,data=Dtrain))

#marital (married et unknown >0.05)
summary(lm(y~marital,data=Dtrain))
crossvalid(Dtrain,"marital")
#contact p-value<2.2e-16
summary(lm(y~contact,data=Dtrain))

#default (defaultyes=0.59)
summary(lm(y~default,data=Dtrain))

#housing p-value 0.2822
summary(lm(y~housing,data=Dtrain))

#loan pvalue 0.6765
summary(lm(y~loan,data=Dtrain))

#month (monthdec=0.286)
summary(lm(y~month,data=Dtrain))

#day_of_week (bcp >0.05)
summary(lm(y~day_of_week,data=Dtrain))

#edu (bcp >0.05)
summary(lm(y~edu,data=Dtrain))

#poutcome (success ok, nonexistent pas ok)
summary(lm(y~poutcome,data=Dtrain))

#campaign
summary(lm(y~campaign,data=Dtrain))

#contact
summary(lm(y~contact,data=Dtrain))

#previous
summary(lm(y~previous,data=Dtrain))

#pdays
summary(lm(y~I(pdays^2),data=Dtrain))

#ndefault dummies
ndefaultv=Dtrain[,"default"]
ndefault <- as.numeric(ndefaultv=="no")
finalset=data.frame(cbind(ndefault,y))
summary(lm(y~ndefault,data=finalset))

#nhousing dummies
nhousingv=Dtrain[,"housing"]
nhousing <- as.numeric(nhousingv=="no")
finalset=data.frame(cbind(nhousing,y))

summary(lm(y~nhousing,data=finalset))

#nloan dummies p-value=0.493
nloanv=Dtrain[,"loan"]
nloan <- as.numeric(nloanv=="no")
finalset=data.frame(cbind(nloan,y))

summary(lm(y~nloan,data=finalset))

#contact dummies
contactv=Dtrain[,"contact"]
contact <- as.numeric(contactv=="telephone")

y=Dtrain[,"y"]
finalset=data.frame(cbind(contact,y))
summary(lm(y~contact,data=finalset))

crossvalid(finalset,"contact")

#married dummies
marriedv=Dtrain[,"marital"]
married <- as.numeric(marriedv=="married")

y=Dtrain[,"y"]

finalset=data.frame(cbind(married,y))
summary(lm(y~married,data=finalset))

crossvalid(finalset,"married")

#student dummies
studentv=Dtrain[,"job"]
student=as.numeric(studentv=="student")
y=Dtrain[,"y"]
finalset=data.frame(cbind(student,y))
summary(lm(y~student,data=finalset))
crossvalid(finalset,c("student"))

#housemaid dummies
housemaidv=Dtrain[,"job"]
housemaid=as.numeric(housemaidv=="housemaid")
y=Dtrain[,"y"]
finalset=data.frame(cbind(housemaid,y))
summary(lm(y~housemaid,data=finalset))

#services dummies
servicesv=Dtrain[,"job"]
services=as.numeric(servicesv=="services")
y=Dtrain[,"y"]
finalset=data.frame(cbind(services,y))
summary(lm(y~services,data=finalset))



#retired dummies
retiredv=Dtrain[,"job"]
retired=as.numeric(retiredv=="retired")
y=Dtrain[,"y"]
finalset=data.frame(cbind(retired,y))
summary(lm(y~retired,data=finalset))
crossvalid(finalset,c("retired"))

#blue-collar dummies
bcv=Dtrain[,"job"]
bc=as.numeric(bcv=="blue-collar")
y=Dtrain[,"y"]
finalset=data.frame(cbind(bc,y))
summary(lm(y~bc,data=finalset))

#self-employed dummies
sev=Dtrain[,"job"]
se=as.numeric(sev=="self-employed")
y=Dtrain[,"y"]
finalset=data.frame(cbind(se,y))
summary(lm(y~se,data=finalset))

#divorce dummies p-value=0.539
divorcedv=Dtrain[,"marital"]
divorced <- as.numeric(divorcedv=="divorced")

y=Dtrain[,"y"]

finalset=data.frame(cbind(divorced,y))
summary(lm(y~divorced,data=finalset))

#single dummies
singlev=Dtrain[,"marital"]
single <- as.numeric(singlev=="single")

y=Dtrain[,"y"]

finalset=data.frame(cbind(single,y))
summary(lm(y~single,data=finalset))
crossvalid(finalset,c("single"))

#eduuniversity.degree dummies
univ=Dtrain[,"edu"]
uni <- as.numeric(univ=="university.degree")
finalset=data.frame(cbind(uni,y))
summary(lm(y~uni,data=finalset))
crossvalid(finalset,c("uni"))

#eduprofessionalcourse dummies #p-value=0.764
profv=Dtrain[,"edu"]
prof <- as.numeric(profv=="professional.course")
finalset=data.frame(cbind(prof,y))
summary(lm(y~prof,data=finalset))

#edu4 dummies #p-value=0.0289
edu4v=Dtrain[,"edu"]
edu4 <- as.numeric(edu4v=="4")
finalset=data.frame(cbind(edu4,y))
summary(lm(y~edu4,data=finalset))

#edu6 dummies #p-value=0.13
edu6v=Dtrain[,"edu"]
edu6 <- as.numeric(edu6v=="6")
finalset=data.frame(cbind(edu6,y))
summary(lm(y~edu6,data=finalset))

#edu9 dummies #p-value=0.0325
edu9v=Dtrain[,"edu"]
edu9 <- as.numeric(edu9v=="9")
finalset=data.frame(cbind(edu9,y))
summary(lm(y~edu9,data=finalset))

#edu0 dummies #p-value=0.355
edu0v=Dtrain[,"edu"]
edu0 <- as.numeric(edu0v=="0")
finalset=data.frame(cbind(edu0,y))
summary(lm(y~edu0,data=finalset))

#highschool dummies #p-value=0.24
hcv=Dtrain[,"edu"]
hc <- as.numeric(hcv=="high.school")
finalset=data.frame(cbind(hc,y))
summary(lm(y~hc,data=finalset))

#successpoutcome dummies
successv=Dtrain[,"poutcome"]
success <- as.numeric(successv=="success")
finalset=data.frame(cbind(success,y))
summary(lm(y~success,data=finalset))
crossvalid(finalset,c("success"))

#failurepoutcome dummies p-value=0.3735
failurev=Dtrain[,"poutcome"]
failure <- as.numeric(failurev=="failure")
finalset=data.frame(cbind(failure,y))
summary(lm(y~failure,data=finalset))
crossvalid(finalset,c("failure"))

#october dummies 
octv=Dtrain[,"month"]
oct <- as.numeric(octv=="oct")
finalset=data.frame(cbind(oct,y))
summary(lm(y~oct,data=finalset))
crossvalid(finalset,c("oct"))

#march dummies
marv=Dtrain[,"month"]
mar <- as.numeric(novv=="mar")
finalset=data.frame(cbind(mar,y))
summary(lm(y~mar,data=finalset))
crossvalid(finalset,c("mar"))

#may dummies
mayv=Dtrain[,"month"]
may <- as.numeric(mayv=="may")
finalset=data.frame(cbind(may,y))
summary(lm(y~may,data=finalset))
crossvalid(finalset,c("may"))

#jun dummies
junv=Dtrain[,"month"]
jun <- as.numeric(junv=="jun")
finalset=data.frame(cbind(jun,y))
summary(lm(y~jun,data=finalset))
crossvalid(finalset,c("jun"))

#jul dummies #p-value=0.811
julv=Dtrain[,"month"]
jul <- as.numeric(julv=="jul")
finalset=data.frame(cbind(jul,y))
summary(lm(y~jul,data=finalset))
crossvalid(finalset,c("jul"))

#aug dummies 
augv=Dtrain[,"month"]
aug <- as.numeric(augv=="aug")
finalset=data.frame(cbind(aug,y))
summary(lm(y~aug,data=finalset))
crossvalid(finalset,c("aug"))

#april dummies
aprv=Dtrain[,"month"]
apr <- as.numeric(aprv=="apr")
finalset=data.frame(cbind(apr,y))
summary(lm(y~apr,data=finalset))
crossvalid(finalset,c("apr"))

#thursday dummies
thuv=Dtrain[,"day_of_week"]
thu <- as.numeric(thuv=="thu")
finalset=data.frame(cbind(thu,y))
summary(lm(y~thu,data=finalset))
crossvalid(finalset,c("thu"))

#monday dummies
monv=Dtrain[,"day_of_week"]
mon <- as.numeric(monv=="mon")
finalset=data.frame(cbind(mon,y))
summary(lm(y~mon,data=finalset))
crossvalid(finalset,c("mon"))

#tuesday dummies
tuev=Dtrain[,"day_of_week"]
tue <- as.numeric(tuev=="tue")
finalset=data.frame(cbind(tue,y))
summary(lm(y~tue,data=finalset))
crossvalid(finalset,c("tue"))


#backward

finalset=data.frame(cbind(ndefault,hc,prof,bc,married,retired,student,failure,success,single,uni,job,age,marital,default,housing,loan,contact,month,dow,campaign,pdays,previous,poutcome,edu,y))
test.lm=lm(y~marital+default+contact+campaign+pdays,data=finalset)

bestpred=backward(c("ndefault","hc","prof","bc","married","retired","student","failure","success","single","uni","job","age","marital","default","housing","loan","contact","month","dow","campaign","pdays","previous","poutcome","edu"),finalset)



crossvalid(finalset,bestpred)
summary(test.lm)

backward <- function(predictors,set){
  currentPred=predictors
  for(i in 1:length(predictors)){
    cross=crossvalid(finalset,currentPred)
    idtodel=0
    for(j in 1:length(currentPred)){
      currentcross=0
      for(h in 1:5){
        currentcross=currentcross+crossvalid(finalset,currentPred[-j])
      }
      currentcross=currentcross/5
      if(currentcross<cross){
        idtodel=j
        cross=currentcross
      }
    }
    if(idtodel==0){
      return(currentPred)
    }
    else{
      currentPred <- currentPred[-idtodel]
    }
  }
  
}



#age+married
summary(lm(y~campaign+age+I(age^2)+pdays+ndefault+uni+retired+student+contact,data=finalset))

finalset=data.frame(cbind(age,campaign,single,ndefault,married,contact,uni,pdays,retired,student,y))
crossvalid(finalset,c("ndefault","pdays","contact","uni","married","retired","student","age","I(age^2)"))
#married+contact

finalset=data.frame(cbind(contact,married,y))
summary(lm(y~contact+married,data=finalset))

crossvalid(finalset,c("contact","married"))

#student+married
finalset=data.frame(cbind(student,married,y))
summary(lm(y~student+married,data=finalset))
crossvalid(finalset,c("student","married"))

#married+student+contact
finalset=data.frame(cbind(student,married,contact,y))
summary(lm(y~student+married+contact,data=finalset))
crossvalid(finalset,c("student","married","contact"))

#student+contact+age+(age^2)
age=Dtrain[,"age"]
finalset=data.frame(cbind(student,contact,age,y))

summary(lm(y~student+contact+age+I(age^2),data=finalset))
crossvalid(finalset,c("student","contact","age","I(age^2)"))


#student+retired+contact+age+age(^2)
age=Dtrain[,"age"]
finalset=data.frame(cbind(student,retired,contact,age,y))

summary(lm(y~student+contact+retired+age+I(age^2),data=finalset))
crossvalid(finalset,c("student","retired","contact","age","I(age^2)"))


#single+student+retired+contact+age+age(^2)
age=Dtrain[,"age"]
finalset=data.frame(cbind(single,student,retired,contact,age,y))

summary(lm(y~student+single+contact+retired+age+I(age^2),data=finalset))
crossvalid(finalset,c("student","single","retired","contact","age","I(age^2)"))


#uni+single+student+retired+contact+age+age(^2)
age=Dtrain[,"age"]
finalset=data.frame(cbind(single,uni,student,retired,contact,age,y))

summary(lm(y~student+contact+uni+retired+age+I(age^2),data=finalset))
crossvalid(finalset,c("student","uni","retired","contact","age","I(age^2)"))

#uni+success+student+retired+contact+age+age(^2)
age=Dtrain[,"age"]
finalset=data.frame(cbind(single,success,uni,student,retired,contact,age,y))

summary(lm(y~student+contact+uni+success+retired+age+I(age^2),data=finalset))
crossvalid(finalset,c("student","uni","success","retired","contact","age","I(age^2)"))


#uni+success+student+retired+contact+age+age(^2)+campaign
age=Dtrain[,"age"]
campaign=Dtrain[,"campaign"]
finalset=data.frame(cbind(single,success,uni,student,campaign,retired,contact,age,y))

summary(lm(y~campaign+student+contact+uni+success+retired+age+I(age^2),data=finalset))
crossvalid(finalset,c("campaign","student","uni","success","retired","contact","age","I(age^2)"))


#uni+student+retired+contact+age+age(^2)+campaign+pdays
age=Dtrain[,"age"]
campaign=Dtrain[,"campaign"]
pdays=Dtrain[,"pdays"]
finalset=data.frame(cbind(success,campaign,ndefault,pdays,uni,student,retired,contact,age,y))

summary(lm(y~ndefault+student+contact+uni+pdays+retired+age+I(age^2),data=finalset))
crossvalid(finalset,c("student*contact","ndefault","student","uni","pdays","retired","contact","age","I(age^2)"))


#uni+student+retired+contact+age+age(^2)+campaign+pdays+october
age=Dtrain[,"age"]
campaign=Dtrain[,"campaign"]
pdays=Dtrain[,"pdays"]
finalset=data.frame(cbind(oct,single,pdays,uni,student,campaign,retired,contact,age,y))

summary(lm(y~campaign+oct+student+contact+uni+pdays+retired+age+I(age^2),data=finalset))
crossvalid(finalset,c("oct","campaign","student","uni","pdays","retired","contact","age","I(age^2)"))

#uni+student+retired+contact+age+age(^2)+campaign+pdays+october+march
age=Dtrain[,"age"]
campaign=Dtrain[,"campaign"]
pdays=Dtrain[,"pdays"]
finalset=data.frame(cbind(oct,single,pdays,uni,student,campaign,mar,retired,contact,age,y))

summary(lm(y~campaign+oct+student+contact+uni+pdays+retired+mar+age+I(age^2),data=finalset))
crossvalid(finalset,c("mar","oct","campaign","student","uni","pdays","retired","contact","age","I(age^2)"))

#uni+student+retired+contact+age+age(^2)+campaign+pdays+october+march+may
age=Dtrain[,"age"]
campaign=Dtrain[,"campaign"]
pdays=Dtrain[,"pdays"]
finalset=data.frame(cbind(oct,single,pdays,uni,student,campaign,mar,may,retired,contact,age,y))

summary(lm(y~campaign+oct+student+contact+uni+pdays+retired+mar+age+may+I(age^2),data=finalset))
crossvalid(finalset,c("may","mar","oct","campaign","student","uni","pdays","retired","contact","age","I(age^2)"))

#uni+student+retired+contact+age+age(^2)+campaign+pdays+october+march+may+april
age=Dtrain[,"age"]
campaign=Dtrain[,"campaign"]
pdays=Dtrain[,"pdays"]
finalset=data.frame(cbind(oct,single,pdays,uni,student,campaign,mar,may,apr,retired,contact,age,y))

summary(lm(y~campaign+oct+student+contact+uni+pdays+retired+mar+age+apr+may+I(age^2),data=finalset))
crossvalid(finalset,c("may","mar","oct","campaign","student","uni","apr","pdays","retired","contact","age","I(age^2)"))

#uni+student+retired+contact+age+age(^2)+campaign+pdays+october+march+may+april+thu
age=Dtrain[,"age"]
campaign=Dtrain[,"campaign"]
pdays=Dtrain[,"pdays"]
finalset=data.frame(cbind(oct,pdays,uni,student,campaign,mar,may,apr,thu,retired,contact,age,y))

summary(lm(y~campaign+oct+student+contact+uni+pdays+retired+mar+age+apr+may+thu+I(age^2),data=finalset))
crossvalid(finalset,c("may","mar","oct","campaign","student","uni","apr","pdays","thu","retired","contact","age","I(age^2)"))
test.lm=lm(y~campaign+oct+student+contact+uni+pdays+retired+mar+age+apr+may+thu+I(age^2),data=finalset)

#age+age(^2)+march+april
finalset=data.frame(cbind(mar,apr,age,y))
test.lm=lm(y~mar+apr+age+I(age^2),data=finalset)
summary(test.lm)
crossvalid(finalset,c("mar","apr","age","I(age^2)"))

#uni+success+student+retired+contact+age+age^5+campagin^3
age=Dtrain[,"age"]
finalset=data.frame(cbind(success,failure,uni,previous,campaign,student,retired,contact,age,y))

summary(lm(y~student+contact+uni+success+retired+campaign+I(campaign^2)+I(campaign^3)+age+I(age^2)+I(age^3)+I(age^4)+I(age^5),data=finalset))
crossvalid(finalset,c("student","uni","I(campaign)","I(campaign^2)","I(campaign^3)","success","retired","contact","age","I(age^2)","I(age^3)","I(age^4)","I(age^5)"))

#uni+success+student+retired+contact+age+age(^2)+may+april
age=Dtrain[,"age"]
campaign=Dtrain[,"campaign"]
pdays=Dtrain[,"pdays"]
finalset=data.frame(cbind(uni,success,student,may,retired,contact,age,y))

summary(lm(y~student+success+contact+uni+retired+age+may+I(age^2),data=finalset))
crossvalid(finalset,c("may","success","student","uni","retired","contact","age","I(age^2)"))
test.lm=lm(y~student+contact+uni+retired+age+apr+may+I(age^2),data=finalset)

#uni+married+student+retired+contact+age+age(^2)+ndefault+pdays
age=Dtrain[,"age"]
previous=Dtrain[,"previous"]
finalset=data.frame(cbind(pdays,poutcome,ndefault,uni,student,retired,contact,age,y))

summary(lm(y~student+contact+pdays+ndefault+uni+retired+age+I(age^2),data=finalset))
crossvalid(finalset,c("ndefault","student","pdays","uni","retired","contact","age","I(age^2)"))

#uni+married+student+retired+contact+age+age(^2)+ndefault+pdays (sans unknown)


Dtrain2=Dtrain
Dtrain2=Dtrain2[Dtrain2$marital!="unknown",]
Dtrain2=Dtrain2[Dtrain2$edu!="unknown",]
Dtrain2=Dtrain2[Dtrain2$job!="unknown",]
pdays2=Dtrain2[,"pdays"]
single2v=Dtrain2[,"marital"]
single2=as.numeric(single2v=="single")
ndefault2v=Dtrain2[,"default"]
ndefault2=as.numeric(ndefault2v=="no")
uni2v=Dtrain2[,"edu"]
uni2=as.numeric(uni2v=="university.degree")
student2v=Dtrain2[,"job"]
student2=as.numeric(student2v=="student")
retired2v=Dtrain2[,"job"]
retired2=as.numeric(retired2v=="retired")
contact2v=Dtrain2[,"contact"]
contact2=as.numeric(contact2v=="telephone")
age2=Dtrain2[,"age"]
y2=Dtrain2[,"y"]
finalset2=data.frame(cbind(pdays2,single2,ndefault2,uni2,student2,retired2,contact2,age2,y2))
summary(lm(y2~student2+contact2+I(pdays2^2)+ndefault2+uni2+retired2+age2+I(age2^2),data=finalset2))
crossvalid(finalset2,c("ndefault2","student2","pdays2","uni2","retired2","contact2","age2","I(age2^2)"))

#uni+student+retired+contact+age+age(^5)+ndefault+pdays 

finalset=data.frame(cbind(month,pdays,admin,single,ndefault,uni,student,retired,contact,age,y))

summary(lm(y~month+student+contact+pdays+ndefault+uni+retired+age+I(age^2)+I(age^3)+I(age^4)+I(age^5),data=finalset))
crossvalid(finalset,c("month","ndefault","student","pdays","uni","retired","contact","age","I(age^2)","I(age^3)","I(age^4)","I(age^5)"))

#
job=Dtrain[,"job"]
housing=Dtrain[,"housing"]
loan=Dtrain[,"loan"]
default=Dtrain[,"default"]
marital=Dtrain[,"marital"]
month=Dtrain[,"month"]
dow=Dtrain[,"day_of_week"]
edu=Dtrain[,"edu"]
poutcome=Dtrain[,"poutcome"]
finalset=data.frame(cbind(month,default,failure,poutcome,housing,loan,marital,job,edu,married,month,uni,pdays,ndefault,student,retired,contact,age,y))

summary(lm(y~ndefault+uni+student+poutcome+retired+month+I(month^2)+contact+pdays+age+I(age^2),data=finalset))
crossvalid(finalset,c("month","ndefault","poutcome","student","retired","uni","age","I(age^2)","pdays","contact"))






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




#test married+student+contact
marriedv=Xtest[,"marital"]
marriedTest <- as.numeric(marriedvTest=="married")
studentv=Xtest[,"job"]
studentTest <- as.numeric(studentvTest=="student")
contactv=Xtest[,"contact"]
contactTest <- as.numeric(contactvtest=="telephone")



test.lm=lm(y~student+married+contact,data=finalset)


finalTestSet=data.frame(cbind(married,student,contact))

proba=predict(test.lm,finalTestSet, type="response")


#test uni+success+student+retired+contact+age+age(^2) (best pred)
studentvT=Xtest[,"job"]
studentT=as.numeric(studentvT=="student")
retiredvT=Xtest[,"job"]
retiredT=as.numeric(retiredvT=="retired")
univT=Xtest[,"edu"]
uniT <- as.numeric(univT=="university.degree")
contactvT=Xtest[,"contact"]
contactT <- as.numeric(contactvT=="telephone")
ageT=Xtest[,"age"]
successvT=Xtest[,"poutcome"]
successT <- as.numeric(successvT=="success")


test.lm=lm(y~student+contact+uni+success+retired+age+I(age^2),data=finalset)

finalTestSet=data.frame(cbind(student=studentT,retired=retiredT,uni=uniT,contact=contactT,success=successT,age=ageT))

proba=predict(test.lm,finalTestSet,type="response")


#test uni+student+retired+contact+age+age(^2)+campaign+pdays+october+march+may+april+thu (moins bon)
studentvT=Xtest[,"job"]
studentT=as.numeric(studentvT=="student")
retiredvT=Xtest[,"job"]
retiredT=as.numeric(retiredvT=="retired")
univT=Xtest[,"edu"]
uniT <- as.numeric(univT=="university.degree")
contactvT=Xtest[,"contact"]
contactT <- as.numeric(contactvT=="telephone")
ageT=Xtest[,"age"]
campaignT=Xtest[,"campaign"]
pdaysT=Xtest[,"pdays"]
octvT=Xtest[,"month"]
octT=as.numeric(octvT=="oct")
marvT=Xtest[,"month"]
marT=as.numeric(marvT=="mar")
mayvT=Xtest[,"month"]
mayT=as.numeric(mayvT=="may")
aprvT=Xtest[,"month"]
aprT=as.numeric(aprvT=="apr")
thuvT=Xtest[,"day_of_week"]
thuT=as.numeric(thuvT=="thu")

test.lm=lm(y~pdays+uni+oct+student+campaign+mar+may+apr+thu+retired+contact+age+I(age^2),data=finalset)

finalTestSet=data.frame(cbind(pdays=pdaysT,uni=uniT,oct=octT,student=studentT,campaign=campaignT,mar=marT,may=mayT,apr=aprT,thu=thuT,retired=retiredT,contact=contactT,age=ageT))

proba=predict(test.lm,finalTestSet, type = "response")


#test age+age(^2)+mar+apr (nul) 0.6...
ageT=Xtest[,"age"]
marvT=Xtest[,"month"]
marT=as.numeric(marvT=="mar")
aprvT=Xtest[,"month"]
aprT=as.numeric(aprvT=="apr")

test.lm=lm(y~apr+mar+age+I(age^2),data=finalset)

finalTestSet=data.frame(cbind(mar=marT,apr=aprT,age=ageT))

proba=predict(test.lm,finalTestSet,type="response")

#test uni+success+student+retired+contact+age+age(^5)+campaign^3 (moins bien) 
studentvT=Xtest[,"job"]
studentT=as.numeric(studentvT=="student")
retiredvT=Xtest[,"job"]
retiredT=as.numeric(retiredvT=="retired")
univT=Xtest[,"edu"]
uniT <- as.numeric(univT=="university.degree")
contactvT=Xtest[,"contact"]
contactT <- as.numeric(contactvT=="telephone")
ageT=Xtest[,"age"]
successvT=Xtest[,"poutcome"]
successT <- as.numeric(successvT=="success")
campaignT=Xtest[,"campaign"]

test.lm=lm(y~student+contact+uni+success+retired+age+I(age^5)+I(campaign^3),data=finalset)

finalTestSet=data.frame(cbind(student=studentT,retired=retiredT,campaign=campaignT,uni=uniT,contact=contactT,success=successT,age=ageT))

proba=predict(test.lm,finalTestSet,type="response")

#test uni+success+student+retired+contact+age+age(^2)+apr+may (moins bon) 0.56...
studentvT=Xtest[,"job"]
studentT=as.numeric(studentvT=="student")
retiredvT=Xtest[,"job"]
retiredT=as.numeric(retiredvT=="retired")
univT=Xtest[,"edu"]
uniT <- as.numeric(univT=="university.degree")
contactvT=Xtest[,"contact"]
contactT <- as.numeric(contactvT=="telephone")
ageT=Xtest[,"age"]
successvT=Xtest[,"poutcome"]
successT <- as.numeric(successvT=="success")
aprvT=Xtest[,"month"]
aprT <- as.numeric(aprvT=="apr")
mayvT=Xtest[,"month"]
mayT <- as.numeric(mayvT=="may")
thuvT=Xtest[,"day_of_week"]
thuT <- as.numeric(thuvT=="thu")

test.lm=lm(y~student+contact+uni+success+retired+age+I(age^2)+apr+may,data=finalset)

finalTestSet=data.frame(cbind(apr=aprT,may=mayT,student=studentT,retired=retiredT,uni=uniT,contact=contactT,success=successT,age=ageT))

proba=predict(test.lm,finalTestSet,type="response")

#test uni+married+student+retired+contact+age+age(^2)+ndefault+pdays (best pred) 0.54!!!!!
ndefaultvT=Xtest[,"default"]
ndefaultT <- as.numeric(ndefaultvT=="no")
pdaysT=Xtest[,"pdays"]
finalset=data.frame(cbind(pdays,single,ndefault,uni,student,retired,contact,age,y))

test.lm=lm(y~student+contact+uni+retired+age+I(age^2)+pdays+ndefault,data=finalset)

finalTestSet=data.frame(cbind(student=studentT,retired=retiredT,uni=uniT,contact=contactT,age=ageT,pdays=pdaysT,ndefault=ndefaultT))

proba=predict(test.lm,finalTestSet,type="response")


#test uni+married+student+retired+contact+age+age(^2)+ndefault+pdays+may (moins bien 0.5433)
ndefaultvT=Xtest[,"default"]
ndefaultT <- as.numeric(ndefaultvT=="no")
pdaysT=Xtest[,"pdays"]
finalset=data.frame(cbind(pdays,single,ndefault,uni,student,retired,may,contact,age,y))

test.lm=lm(y~student+contact+uni+retired+age+I(age^2)+pdays+ndefault+may,data=finalset)

finalTestSet=data.frame(cbind(student=studentT,retired=retiredT,uni=uniT,contact=contactT,may=mayT,age=ageT,pdays=pdaysT,ndefault=ndefaultT))

proba=predict(test.lm,finalTestSet,type="response")



#test uni+married+student+retired+contact+age+age(^2)+ndefault+pdays sans unknown (moins bine 0.5436...)


test.lm=lm(y2~student2+contact2+uni2+retired2+age2+I(age2^2)+pdays2+ndefault2,data=finalset2)

finalTestSet=data.frame(cbind(student2=studentT,retired2=retiredT,uni2=uniT,contact2=contactT,age2=ageT,pdays2=pdaysT,ndefault2=ndefaultT))

proba=predict(test.lm,finalTestSet,type="response")



#test uni+married+student+retired+contact+age+age(^5)+ndefault+pdays 0.54359

finalset=data.frame(cbind(pdays,single,ndefault,uni,student,retired,contact,age,y))

test.lm=lm(y~student+contact+uni+retired+age+I(age^5)+pdays+ndefault,data=finalset)

finalTestSet=data.frame(cbind(student=studentT,retired=retiredT,uni=uniT,contact=contactT,age=ageT,pdays=pdaysT,ndefault=ndefaultT))

proba=predict(test.lm,finalTestSet,type="response")


#test uni+student+retired+contact+age+age^2+age^3+age^4+age(^5)+ndefault+pdays 0.548

finalset=data.frame(cbind(pdays,single,ndefault,uni,student,retired,contact,age,y))

test.lm=lm(y~student+contact+uni+retired+age+I(age^2)+I(age^3)+I(age^4)+I(age^5)+pdays+ndefault,data=finalset)

finalTestSet=data.frame(cbind(student=studentT,retired=retiredT,uni=uniT,contact=contactT,age=ageT,pdays=pdaysT,ndefault=ndefaultT))

proba=predict(test.lm,finalTestSet,type="response")


#test avec month et poutcome 0.56...

monthT=Xtest[,"month"]
poutcomeT=Xtest[,"poutcome"]

finalTestSet=data.frame(cbind(poutcome=poutcomeT,month=monthT,student=studentT,retired=retiredT,uni=uniT,contact=contactT,age=ageT,pdays=pdaysT,ndefault=ndefaultT))

test.lm=lm(y~ndefault+uni+student+poutcome+retired+month+I(month^2)+contact+pdays+age+I(age^2),data=finalset)


proba=predict(test.lm,finalTestSet,type="response")



for(i in 1:length(proba)){
  proba[i]=max(min(proba[i],1-(10^-15)),(10^-15))
}


res=data.frame(id=1:nrow(Xtest),prob=proba)




write.csv(res,"C:/Users/Charly/Desktop/ProjetBD/Res.csv", row.names = FALSE)









