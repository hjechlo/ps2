data("Boston",package="MASS") #download pacakage immediately
Boston = Boston[complete.cases(Boston),] #extract all complete rows in Boston
#no missing or na values
head(Boston)
dim(Boston)
summary(Boston)

set.seed(100) #set.seed is to fix a random sample (for reproducability)
training.idx=sample(1:nrow(Boston),nrow(Boston)*0.8) 
#extract 80% of rows from Boston dataset
train.data = Boston[training.idx,]
test.data = Boston[-training.idx,]

#medv~. (. means use every other var except medv as predictor variable)
#cv -> cross validation
#number -> number of subsamples i want to split my data into 
#number = # of trng data/~40-50 (each sub-sample has about 40-50 objs)
#1 sub-sample as validation data, remaining 9 sub-samples used to create the model
#pre-process -> normalisation process

set.seed(101)
model=train(medv~.,data=train.data,method = "knn",
            trControl = trainControl("cv",number = 10),
            preProcess = c("center","scale"),
            tuneLength = 10
            )

plot(model)
#best tuning parameter that minimizes the RMSE
model$bestTune
#7 nearest neighbours give lowest RMSE

predictions = predict(model,test.data)
head(predictions) #for first 6 rows

#ways to evaluate prediction performance
#1. find RMSE value
RMSE(predictions,test.data$medv)
#2. plot (actual against predicted)
plot(test.data$medv,predictions, main="Prediction Performance of kNN Regression")
#we want line to intersect at 0 with gradient 1 (i.e line y=x)
abline(0,1,col="red")
#hence this model is good




