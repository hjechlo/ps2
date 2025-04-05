#MCQ : a,d,b,a,c

#2 :
#(1)
str(iris)
library(dplyr)
iris2 = iris %>% filter(Species == c("setosa","virginica"))
summary(iris2)

#(2)
library(ggplot2)
ggplot(iris2,aes(x = Sepal.Length, y = Sepal.Width)) + geom_point(aes(col = Species))

#(3)
library(class)
iris2 = iris2 %>% mutate(set = factor(ifelse(Species == "setosa", 1 , 0)))
nor = function(x) { (x -min(x))/(max(x)-min(x)) }
iris2nor = iris2
iris2nor[,1:4] = sapply(iris2nor[,1:4], nor)

set.seed(100)
training.idx = sample(1:nrow(iris2), 0.8*nrow(iris2))
train.knn = iris2nor[training.idx,]
test.knn = iris2nor[-training.idx,]

ac = rep(0, 20)
for(i in 1:20){
  set.seed(101)
  knn.i=knn(as.matrix(train.knn[,2]), as.matrix(test.knn[,2]), cl=train.knn$set, k=i)
  ac[i]=mean(knn.i ==test.knn$set)
  cat("k=", i, " accuracy=", ac[i], "\n")
}
plot(ac, type="b", xlab="K",ylab="Accuracy")

#best k = 4

set.seed(101)
knn2=knn(as.matrix(train.knn[,2]), as.matrix(test.knn[,2]), cl=train.knn$set, k=4)
mean(knn2 ==test.knn$set)

table(knn2,actual=test.knn$set)
#    actual
#knn2 0 1
#   0 6 1
#   1 0 3

library(e1071)
train.svm = iris2[training.idx,]
test.svm = iris2[-training.idx,]

m.svm = svm(set~Sepal.Width, data = train.svm, kernel = "linear")
summary(m.svm)
pred.svm <- predict(m.svm, newdata=test.svm[,2])
table(pred.svm, actual = test.svm$set)
#        actual
#pred.svm 0 1
#       0 6 1
#       1 0 3

#(4)
#since the result form both knn and svm is exactly the same, they are both good.
#for both : accurary : 9/10 = 90%
#confusion matrix for both : 
#  0 1
#0 6 1
#1 0 3
#Form the plot in (2), drawing a line at Sepal.Width =3.25 will put most of setosa above the line and most virginica below the line
#We can say that Sepal.Width is a good predictor for species between the 2.