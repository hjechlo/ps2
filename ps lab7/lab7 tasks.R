install.packages("mlbench")
library(mlbench)
library(caret)

#1 familiarise dataset
grad = read.csv("gradadmit.csv",header=TRUE,sep=",")
dim(grad)
str(grad)

#convert rank & admit to catergorical variable
grad$admit = factor(grad$admit)
grad$rank = factor(grad$rank)

#split into test and training
set.seed(100)
training.idx = sample(1:nrow(grad),size=nrow(grad)*0.8)
train.data = grad[training.idx,]
test.data = grad[-training.idx,]

#logistic regression
mlogit = glm(admit~.,data=train.data,family="binomial")
summary(mlogit)
#rank and gpa are significant variables affecting admit while not so for gre

#predicted probabilities
pred.p = predict(mlogit, newdata=test.data,type="response")
y_pred_num = ifelse(pred.p > 0.5,1,0)
y_pred = factor(y_pred_num,levels=c(0,1))
#accuracy of classification
mean(y_pred==test.data$admit)
# 67.5% correctly classified
#confusion matrix
confusionMatrix(y_pred, test.data$admit)