# ng zi en chloe ps2 assignment 2
install.packages("survival")
install.packages("e1071")
library(e1071)
library(survival)
library(class)
library(dplyr)

#1 familiarise and prepare data
#inspecting summary statistics and structure of dataset
str(rotterdam)
dim(rotterdam)
head(rotterdam)
summary(rotterdam)

#new dataframe containing only relevant predictors and recur
rotter = rotterdam %>% filter(death==0) %>%
  select(age,size,grade,nodes,pgr,er,recur)

#converting into factor var
rotter$recur = factor(rotter$recur)
head(rotter)

#2 split into training and testing sets
set.seed(100)
training.idx = sample(1:nrow(rotter), size = nrow(rotter)*0.8)
train.data = rotter[training.idx,]
test.data = rotter[-training.idx,]

#3.1 logistic regression
mlogit = glm(recur~.,data=train.data,family="binomial")
summary(mlogit)
# grade and nodes are significant predictors at level=0.05
# for every one-unit increase in tumor grade, 
# the odds of relapse increase by 1.61 times
# for every additional affected lymph node, 
# the odds of relapse increase by 1.09 times

#logistic predicted probabilities
pred.logp = predict(mlogit, newdata=test.data,type="response")
y_pred_num = ifelse(pred.logp > 0.5,1,0)
y_pred = factor(y_pred_num,levels=c(0,1))
#accuracy of classification
mean(y_pred==test.data$recur)
# 0.745614 = 74.56% correctly classified
#confusion matrix
table(y_pred,actual=test.data$recur)
#      actual
# y_pred   0   1
#     0 252  81
#     1   6   3
# based on the LR results, the two significant predictors
# of breast cancer relapse are grades and nodes
# misclassification rates:
# false positive rate: 2.33%
# false negative rate: 96.43%



#3.2 kNN classification
#convert to numeric
rotter2 = rotter %>% mutate(age=as.numeric(age),pgr=as.numeric(pgr),
                            er=as.numeric(er),nodes=as.numeric(nodes),
                            grade=as.numeric(grade),size=as.numeric(size))
#normalisation
nor = function(x){
  (x-min(x))/(max(x)-min(x))
}
rotter2[,1:6] = sapply(rotter2[,1:6],nor)
str(rotter2)
train.data1 = rotter2[training.idx,]
test.data1 = rotter2[-training.idx,]
str(test.data1)

ac = rep(0,30)
for(i in 1:30){
  set.seed(101)
  knn.i=knn(train.data1[,1:6],test.data1[,1:6],cl=train.data1$recur,k=i)
  ac[i]=mean(knn.i == test.data1$recur)
  cat("k=", i, "accuracy=",ac[i],"\n")
}
#accuracy plot
plot(ac, type="b", xlab="K",ylab="Accuracy")
#k=25 with highest accuracy=0.7602339 i.e 76.02% correctly classified
set.seed(101)
mknn<-knn(train.data1[,1:6], test.data1[,1:6], cl=train.data1$recur, k=25)
mean(mknn ==test.data1$recur) 
#confusion matrix
table(pred_knn=mknn,actual=test.data1$recur)
#        actual
#pred_knn   0   1
#       0 257  81
#       1   1   3'
# misclassification rates:
# false positive rate: 0.388% smaller than LR
# false negative rate: 96.43% same as LR



#3.3 svm
m.svm<-svm(recur~., data = train.data1, kernel = "linear")
summary(m.svm)
# predict
pred.svm <- predict(m.svm, newdata=test.data1[,1:6])

# Check accuracy:
table(predict=pred.svm, actual=test.data1$recur)
mean(pred.svm ==test.data1$recur)
# 0.754386 = 75.44% correctly classified
#       actual
#predict   0   1
#      0 258  84
#      1   0   0'
# misclassification rates:
# false positive rate: 0% smaller than LR and kNN
# false negative rate: 100% bigger than LR and kNN

#Try radial kernel and tune its parameters 
set.seed(123) #tune requires random splitting for cross validation so set a seed
m.svm.tune1<-tune.svm(recur~., data=train.data1, kernel="radial", 
                      cost=10^(-1:2), gamma=c(.1,.5,1,2))
summary(m.svm.tune1)
#classification performance
plot(m.svm.tune1)
best.svm <- m.svm.tune1$best.model
pred.svm.tune <- predict(best.svm, newdata=test.data1[,1:6])
table(pred.svm.tune, test.data1$recur)
mean(pred.svm.tune ==test.data1$recur)
# 0.745614 i.e 74.56% correctly classified, same as LR, not as good as linear model
#pred.svm.tune   0   1
#            0 254  83
#            1   4   1'


#4 comparison of performance between LR, kNN (best choice) and SVM (best choice)
# in terms of accuracy, the 3 methods perform similarly well.

# LR achieved an accuracy of 74.56%. It has a false negative rate of 96.43% and 
# a false positive rate of 2.33%

# kNN classification with k=25 performs slightly better (more accurate) at 
# 76.02% accuracy than LR and SVM. It has a false negative rate of 96.43%,
# which is the same as that of LR, and a false positive rate of 0.388%

# SVM linear model achieved an accuracy of 75.44%. It has a false negative rate 
# of 100% as it failed to classify any positive cases and a false positive rate
# of 0%

# misclassification rates include the false positive rate and false negative rate.
# In the breast cancer test, a small false negative rate is more desirable than 
# a small false positive rate because failing to identify an actual cancer 
# patient (false negative)  is worse than a false alarm (positive). 
# Therefore, in terms of false negative rate, logistic regression and kNN 
# perform equally well. 


