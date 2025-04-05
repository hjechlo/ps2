#mcq: d b a b b

#coding question (2)
library(MASS)
library(dplyr)
library(caret)
library(ggplot2)

data("Cars93",package="MASS")
str(Cars93)

# (ia)
car.stats = group_by(Cars93,Type) %>% 
  summarise(count=n(),meanHorsepower=mean(Cars93$Horsepower))

#(ib)
ggplot(car.stats,aes(x=Type,y=meanHorsepower))+geom_bar(stat="identity")
##comment:All types have about the same average horsepower

# (ii)
cars1 = Cars93 %>% select(EngineSize, Horsepower, Rev.per.mile, Fuel.tank.capacity,Price)

#(iii)
#kNN
set.seed(100)
training.idx=sample(1:nrow(cars1),nrow(cars1)*0.8)
train.data = cars1[trainingidx,]
test.data = cars1[-training.idx,]

set.seed(101)
model = train(Price~EngineSize+Horsepower+Rev.per.mile+Fuel.tank.capacity,
              data=train.data, method="knn",
              trControl=trainControl("cv",number=10),
              preProcess = c("center","scale"),
              tuneLength = 10)

plot(model)
model$bestTune

predictions = predict(model,test.data)
head(predictions)
RMSE(predictions,test.data$Price)

plot(yest.data$Price,predictions,main="Prediction Performance of kNN regression")
abline(0,1,col="red")

#linear reg
set.seed(100)
training.idx=sample(1:nrow(cars1),nrow(cars1)*0.8)
train.data2 = cars1[trainingidx,]
test.data2 = cars1[-training.idx,]

lmodel = lm(Price~EngineSize+Horsepower+Rev.per.mile+Fuel.tank.capacity,
            data=train.data2)

predictions2 = predict(lmodel,test.data)
plot(test.data2$Price,predictions,main="Prediction Performance of linear regression")
abline(0,1,col="red")
RMSE(predictions,test.data2$Price)

# (iv)
#a.
#b.
#c.this means
#d.kNN because lower RMSE