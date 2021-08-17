library("e1071")

#gunakan data iris
head(iris,5)
attach(iris)

#subset data iris
x <- subset(iris, select=-Species)
y <- Species

#buat model svm - cara1
svm_model <- svm(Species ~ ., data=iris)
summary(svm_model)

#buat model svm - cara2
svm_model1 <- svm(x,y)
summary(svm_model1)

#buat prediksinya
pred <- predict(svm_model1,x)
system.time(pred <- predict(svm_model1,x))

#lihat performanya
library(caret)
confusionMatrix(pred,y)

#tuning parameternya pakai gamma
svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
print(svm_tune)

#buat ulang model svm
svm_model_after_tune <- svm(Species ~ ., data=iris, kernel="radial", cost=1, gamma=0.5)
summary(svm_model_after_tune)

#hitung prediksi model svm terbaru dan lihat performanya
pred <- predict(svm_model_after_tune,x)
system.time(predict(svm_model_after_tune,x))
confusionMatrix(pred,y)
