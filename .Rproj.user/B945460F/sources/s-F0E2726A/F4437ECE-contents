#install caret
install.packages("caret", dependencies = c("Depends", "Suggests"))

#load libraries
library(ggplot2)
library(dplyr)
library(caret)
library(lattice)
library(mlbench)
library(modeldata)

set.seed(107)

# read data
surveyData <- read.csv("data/surveyBelkinElago.csv", sep=";", dec =",")

#preprocessing the data
survey <- preProcess(surveyData,   method = c("center", "scale"))
newSurvey <- predict(survey, surveyData)

#splits up data into data about individuals whose brand preferences are known, 
#and data about individuals whose preferences are not known
incomplete <- subset(newSurvey, brand == " ")
complete <- subset(newSurvey, brand !=" ")

#dropping NA values
incomplete$brand <- droplevels(incomplete$brand)
complete$brand <- droplevels(complete$brand)

#partitioning the data in 3/4 and 1/4 to use to split the data into training and test set
inTrain <- createDataPartition(
  y = complete$brand,
  p = .75,
  list = FALSE
)

#splitting the data
training <- complete[ inTrain,]
testing  <- complete[-inTrain,]

#creating folds
inputs <- training[,1:6]
output <- training[,7]

#training the knn model
knnFit1 <- train(inputs, output,
                 method="knn",
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))

#setting up the train control
ctrl <- trainControl(
  method = "repeatedcv", 
  repeats = 3,
  classProbs = TRUE, 
  summaryFunction = twoClassSummary
)

##training the PLS model
plsFit <- train(
  brand ~ .,
  data = complete,
  method = "pls",
  ## Center and scale the predictors for the training
  ## set and all future samples.
  preProc = c("center", "scale"),
  tuneLength = 15,
  trControl = ctrl,
  metric = "ROC"
)

#predicting my two models on the test set
knnClasses <-predict(knnFit1, newdata = testing)
plsClasses <- predict(plsFit, newdata = testing)

#aggregating the test results back to the incomplete data
incompleteKNN <- cbind(incomplete[-7], knnClasses)
incompletePLS <- cbind(incomplete[-7], plsClasses)

#predicting the probabilities for the two models
plsProbs <- predict(plsFit, newdata = testing, type = "prob")
knnProbs <- predict(knnFit1, newdata = testing, type = "prob")