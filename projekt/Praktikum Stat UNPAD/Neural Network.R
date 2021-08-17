set.seed(1234567890)

#install.packages("neuralnet")
library("neuralnet")

library(dplyr)
credit <-"https://gist.githubusercontent.com/Bart6114/8675941/raw/ac4cddcc0909c15ceada2d8c6a303206b10796d9/creditset.csv"

dataset <- read.csv(credit)
head(dataset)

## extract a set to train the NN
trainset <- dataset[1:800, ]

## select the test set
testset <- dataset[801:2000, ]

## build the neural network (NN)
creditnet <- neuralnet(default10yr ~ LTI + age, trainset, hidden = 4, lifesign = "minimal", 
                       linear.output = FALSE, threshold = 0.1)
## plot the NN
plot(creditnet, rep = "best")

## test the resulting output
temp_test <- subset(testset, select = c("LTI", "age"))

creditnet.results <- neuralnet::compute(creditnet, temp_test)

results <- data.frame(actual = testset$default10yr, prediction = creditnet.results$net.result)
results[100:115, ]

results$prediction <- round(results$prediction)
results[100:115, ]

# Accuracy (training set)
original_values <- max.col(results$actual)
pr.nn_2 <- max.col(results$prediction)
mean(pr.nn_2 == original_values)

##OVERFITTED!!!##
set.seed(500)

# 10 fold cross validation
k <- 10
# Results from cv
outs <- NULL

# Crossvalidate, go!
#for(i in 1:k)
{
#  trainset_cv <- dataset[1:800, ]
#  testset_cv <- dataset[801:2000, ]
#  
#  nn_cv <- neuralnet(default10yr ~ LTI + age, trainset_cv, hidden = 4, lifesign = "minimal", 
#                     linear.output = FALSE, threshold = 0.1)
  
  # Compute predictions
#  temp_test_cv <- subset(testset_cv, select = c("LTI", "age"))
#  pr.nn <- neuralnet::compute(nn_cv, temp_test_cv)
#  # Extract results
#  results_cv <- data.frame(actual = testset$default10yr, prediction = pr.nn$net.result)
#  results_cv[100:115, ]
  
#  results_cv$prediction <- round(results_cv$prediction)
#  results_cv[100:115, ]
  
  # Accuracy (test set)
#  original_values <- max.col(results_cv$actual)
#  pr.nn_2 <- max.col(results_cv$prediction)
#  outs[i] <- mean(pr.nn_2 == original_values)
}

#mean(outs)
