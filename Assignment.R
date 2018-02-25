####################################################
# Practical Machine Learning Assignment
####################################################
####################################################

# Regression Models Course Project
# Background
#
# Using devices such as Jawbone Up, Nike FuelBand, and Fitbit
# it is now possible to collect a large amount of data about personal
# activity relatively inexpensively.
# These type of devices are part of the quantified self movement -
# a group of enthusiasts who take measurements about themselves
# regularly to improve their health, to find patterns in their
# behavior, or because they are tech geeks. One thing that people
# regularly do is quantify how much of a particular activity they do,
# but they rarely quantify how well they do it. In this project,
# your goal will be to use data from accelerometers on the belt,
# forearm, arm, and dumbell of 6 participants.
# They were asked to perform barbell lifts correctly and incorrectly
# in 5 different ways. More information is available from the website
# here: http://groupware.les.inf.puc-rio.br/har (see the section on
# the Weight Lifting Exercise Dataset).
#
# Data
#
# The training data for this project are available here:
# https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
# The test data are available here:
# https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv
# The data for this project come from this source: 
# http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.ummary?

# load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(AppliedPredictiveModeling)
library(rpart)
library(elasticnet)
library(gbm)
library(forecast)

# Define Variables for simulation
set.seed(100) # set the seed value for reproducibility

# Load and Clean Data
#
# Load data
training<-read.csv("./pml-training.csv")
testing<-read.csv("./pml-testing.csv")

# Clean data
#
# Delete variables with multiple NA using Non Zero Variance
NZV<-nearZeroVar(training)
trainSet<-training[,-NZV]
testing<-testing[,-NZV]


# Delete variables that are mostly NA
multiNA<-sapply(trainSet, function(x) mean(is.na(x))) > 0.95
trainSet<-trainSet[,multiNA==FALSE]
testing<-testing[,multiNA==FALSE]

# First 6 columns don't possess relevant information so we can delete them
trainSet<-trainSet[,-(1:6)]
testing<-testing[,-(1:6)]

# From the training set, create a train and validation set.
# The latter will be a validation set
inTrain<-createDataPartition(trainSet$classe,p=0.7,list=FALSE)
trainSet<-trainSet[inTrain,]
validationSet<-trainSet[-inTrain,]

# Clear memory
rm(inTrain)

# Model Selection
#
# Let's check the accuracy for the most commons models to choose the best
#

#
# Model LDA

dt1<-Sys.time()
modfitLDA<-train(classe~.,method="lda",data=trainSet)
dt2<-Sys.time()

predLDA<-predict(modfitLDA,newdata=validationSet)

confMatLDA<-confusionMatrix(predLDA,validationSet$classe)
confMatLDA

ldaTime<-difftime(dt2,dt1,units="secs")
ldaTime

#
# Model Generalized Boosting Model  

dt1<-Sys.time()
modfitGBM<-train(classe~.,method="gbm",data=trainSet,verbose=FALSE)
dt2<-Sys.time()

predGBM<-predict(modfitGBM,newdata=validationSet)

confMatGBM<-confusionMatrix(predGBM,validationSet$classe)
confMatGBM

gbmTime<-difftime(dt2,dt1,units="secs")
gbmTime

#
# Model Random Forest

dt1<-Sys.time()
modfitRF<-train(classe~.,method="rf",data=trainSet,tr=trainControl(method="cv"),number=3)
dt2<-Sys.time()

predRF<-predict(modfitRF,newdata=validationSet)

confMatRF<-confusionMatrix(predRF,validationSet$classe)
confMatRF

rfTime<-difftime(dt2,dt1,units="secs")
rfTime

#final prediction

finalPrediction<-predict(modfitRF,newdata=testing)
finalPrediction
