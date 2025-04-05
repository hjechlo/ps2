install.packages("dplyr")
install.packages("datarium")
install.packages("caret")
install.packages("psych")
install.packages("corrplot")
library(caret)
library(dplyr)
library(ggplot2)
library(corrplot)


#details of marketing dataset
??marketing

data(marketing,package = "datarium")
dim(marketing)
str(marketing) #show data type of each var and first 5 obs

#1. get familiar with the dataset
summary(marketing)
ggplot(marketing,aes(sales)) + geom_histogram()
#histogram to know distribution of outcome variables

library(psych) #for small number of var, pairs.panels better graphic
#to show rs btwn var
pairs.panels(marketing,method="pearson",hist.col="steelblue",pch=21,
             density = TRUE,ellipses=FALSE)
#or use corrplot (usually corrplot for large number of variables)
#can see that variables youtube & facebook are highly related to sales
#while newspaper not so much
#impt variables highly related to sales: facebook and youtube (maybe 2nd order
# predictor variables)

#2. split into training and test sets
set.seed(100)
training.idx = sample(1: nrow(marketing),nrow(marketing)*0.8)
train.data2 = marketing[training.idx, ]
test.data2 = marketing[-training.idx, ]

#3.1 kNN
set.seed(101)
model.knn2 = train(sales~., data=train.data2, method = "knn",
                   trControl = trainControl("cv",number=4),
                   preProcess = c("center","scale"),
                   tuneLength = 10)
plot(model.knn2)
model.knn2$bestTune
#k=5
predictions = predict(model.knn2,test.data2)
RMSE(predictions,test.data2$sales)
# 1.366957
plot(test.data2$sales, predictions,main="Prediction performance of kNN regression")
abline(0,1,col="red")

#3.2 linear regression
lmodel2 = lm(sales~.,data=train.data2)
summary(lmodel2)
#r-sq=0.8925 the linear reg model with 3 predictors explains
#89% variation in the sales data
predictions=predict(lmodel2,test.data2)
RMSE(predictions,test.data2$sales)
# 1.95369 higher than that of kNN (hence kNN better without improvement)
plot(test.data2$sales,predictions,main="Prediction performance of linear regression")
abline(0,1,col="red")
#accurate prediction is achieved but prediction of kNN is slightly better

#4 check residuals & improving linear reg
par(mfrow=c(2,2))
plot(lmodel2)
par(mfrow=c(1,1))
#residual plot shows outlying point 131 and a quadratic pattern
#indicating that 2nd order predictors may be needed (one improvement)
#another improvement can be removing outliers then replot
#how to know which var to use as 2nd order predictor?
#use a correlation plot (corrplot) use facebook and youtube as 2nd order predictor
corrplot(cor(train.data2),type="upper",method="color",addCoef.col = "black",number.cex = 0.6)
par(mfrow=c(1,1))

#improvement 1: removing row 131, add youtube and facebook as 2nd order terms
marketing1 = marketing[-131,]
set.seed(100)
training.idx1 = sample(1:nrow(marketing1),nrow(marketing)*0.8)
train.data3 = marketing1[training.idx1,]
test.data3 = marketing1[-training.idx1,]

lmodel3 = lm(sales~youtube+facebook+newspaper+I(facebook^2)+I(youtube^2),data = train.data3)
summary(lmodel3) #R^2 = 0.9255
predictions=predict(lmodel3,test.data3)
RMSE(predictions,test.data3$sales)

#improvement 2: introduce youtube*facebook into linear model as second order term
lmodel4 = lm(sales~youtube+facebook+newspaper+I(facebook^2)+I(youtube^2),I(facebook*youtube),data = train.data3)
summary(lmodel4)


#5 comment on which model to choose
#choose kNN coz RMSE value is smaller than that of linear reg.
