install.packages("dplyr")
install.packages("nycflights13")
install.packages("caret")
library(caret)
library(dplyr)
library(nycflights13)
library(ggplot2)

head(flights)

#PREPARE DATA
flights=mutate(flights, gain = arr_delay - dep_delay)
flights.ua = flights %>%filter(carrier == "UA", air_time>550)%>%
  select(gain,air_time)
#inspect data
flights.ua
summary(flights.ua)
#visualize the relationship
ggplot(flights.ua, aes(x=air_time, y=gain)) + geom_point()+geom_smooth()

#split into training and test sets
set.seed(100)
training.idx = sample(1:nrow(flights.ua), nrow(flights.ua)*0.8)
train.data = flights.ua[training.idx,]
test.data = flights.ua[-training.idx,]

#fit the model
set.seed(101)
model.knn = train(gain~., data=train.data,method="knn", trControl = trainControl("cv",number=6),
                  preProcess=c("center","scale"), tuneLength =10)



##plot model error RMSE vs different values of k
plot(model.knn)

#best tuning parameter k that minimises the RMSE 
#(lowest value)
model.knn$bestTune

#make prediction on test data
predictions=predict(model.knn,test.data)
head(predictions)

#compute prediction error RMSE
RMSE(predictions,test.data$gain)

#plot (compare) predicted gain vs gain in test data
plot(test.data$gain,predictions,main="Prediction performance of kNN regression")
##add reference line x=y
abline(0,1,col="red")
##comment: most data points close to the line y=x, implying a good prediction
##of gain using air_time provided by the knn model


##Extra: using linear regression model
lmodel = lm(gain~.,data=train.data)
summary(lmodel)
#make predictions on the test data
predictions = predict(lmodel,test.data)
plot(test.data$gain,predictions,main="Prediction performance of linear regression")
abline(0,1,col="red")
#prediction error RMSE
RMSE(predictions,test.data$gain)
##comment: linear regression performs better than kNN on prediction in terms of
# RMSE (lower value) and prediction plot