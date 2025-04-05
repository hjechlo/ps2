#kNN classification

##ALWAYS FACTOR OUTCOME VARIABLE
##CONVERT THOSE VAR IN CHAR TO NUMERIC VARIABLES THEN NROMALISE
##^only if more than one predictor variable is used

#packages
library(dplyr)
library(mlbench)
library(class)

#import the data
gradadmit = read.csv("gradadmit.csv",header=TRUE,sep=",")
dim(gradadmit)
summary(gradadmit)
str(gradadmit)

#convert variables gre and rank to numeric
grad<-gradadmit%>%mutate(gre=as.numeric(gre), rank=as.numeric(rank))

#normalize all numeric variable
nor = function(x){
  (x-min(x))/(max(x)-min(x))
}
grad[,2:4] <- sapply(grad[,2:4], nor)

##convert outcome admit to factor
grad$admit <- factor(grad$admit)

#split into test and training sets
set.seed(100)
training.idx = sample(1:nrow(grad),size = nrow(grad)*0.8)
train.data = grad[training.idx,]
test.data = grad[-training.idx,]

#kNN classification
set.seed(101)
knn1=knn(train.data[,2:4],test.data[,2:4],cl=train.data$admit,k=2)
mean(knn1 == test.data$admit)
##0.6625
table(knn1,actual=test.data$admit)

#find value of k for best classifier
ac = rep(0,30)
for(i in 1:30){
  set.seed(101)
  knn.i=knn(train.data[,2:4],test.data[,2:4],cl=train.data$admit,k=i)
  ac[i]=mean(knn.i == test.data$admit)
  cat("k=", i, "accuracy=",ac[i],"\n")
}

#accuracy plot
plot(ac,type="b",xlab="K",ylab="Accuracy")
##k=1 gives highest accuracy of 0.725
set.seed(101)
knn2=knn(train.data[,2:4],test.data[,2:4],cl=train.data$admit,k=1)
mean(knn2 == test.data$admit)
#[1] 0.725
table(knn2,actual=test.data$admit)

#compare with logistic regression
mlogit = glm(admit~.,data=train.data,family="binomial")
#predicted probability P(Y=1)
Pred.p = predict(mlogit,newdata=test.data,type="response")
y_pred_num = ifelse(Pred.p >0.5,1,0)
y_pred = factor(y_pred_num,levels=c(0,1))
mean(y_pred==test.data$admit)
table(y_pred,actual=test.data$admit)

#Compare
#The accuracy of kNN is 72.5% while the accuracy of 
#logistic regression is 68.75%
#in terms of misclassification rate, knn yields a higher
#false positive rate but lower false negative rate than that in logistic regression. 

##Note: false positive rate=no.of false positive/total no.of actual negative
##       =12/(43+12) in kNN2 and 6/(49+6) in logistic regression
##false negative rate=no.of false negative/total no.of actual positive
## =10/(10+15) in kNN2 and 19/(19+6) in logistic regression.
##

## Extra information: For two categorical variables x and y, function table(x,y) creates a table using x as row variable, y as column variable

#misclassification rate
#false positive: FP/FP+TN
#false negative: FN?FN+TP
#choose model with lower false negative rate assuming similar accuracies
