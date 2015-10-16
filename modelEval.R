#
# @author: Nagiza Samatova
#
# load kernlab package <-- install if you do not have it
library(kernlab)

#-----------------------------------------------
# Ex.1: Training error
# Lesson learned: Model is too good to be true
#   Measured the training error, 
#            bias: validation data = training data
#-----------------------------------------------
data (iris)
head (iris)
names (iris)
modelSVM <- ksvm(x=iris[,-5], y=iris[,5])
# should get an error
help(ksvm) # x should be a matrix
modelSVM <- ksvm(x=as.matrix(iris[,-5]), y=iris[,5])
predictions <- predict(modelSVM,iris[,-5])
summary(predictions)
summary(iris[,5])
table (predictions,iris[,5]) 
error(modelSVM) 
# How many samples out of how many total were misclassified?
# What is the error of the model? 
# Is this a good model?

#-----------------------------
# Ex.2: Training vs. Test data 
# Lesson learned: Sampling bias
# Training and test data should have representative
# samples for each class-type
# More general implications:
# how much data@deployment ~ data@model_building phases
#-----------------------------
trainingData <- iris[1:100,]
testData <- iris[101:150,]
modelSVM <- ksvm(x=as.matrix(trainingData[,-5]), y=trainingData[,5])
predictions <- predict(modelSVM,testData[,-5])
table (predictions,testData[,5]) 
# What is the test error?
# Is this a good model?

#-------------------------------
# Ex.3: Cross-validation
#-------------------------------
# perform 3-fold cross-validation
modelSVM <- ksvm(Species~., data=iris, cross=3)
# which is equivalent to (check help(ksvm)):
modelSVM <- ksvm(x=as.matrix(iris[,-5]), y=iris[,5], cross=3)
modelSVM
cross(modelSVM)
# What is the training error?
# What is 3-fold cross-validation error?
# What is 10-fold cross-validation error?
# What is the leave-one-out cross-validation error?

#------------------------------------------------
# Ex.4: Bias is good sometimes
#   Bias your model toward a particular class
#     E.g., give higher weight 
#           to misclassification of a certain class
#--------------------------------------------
data(spam)
dim(spam)
names(spam)
# view the first sample
spam[1,]
t(spam[1,]) # view as a column (transpose)
# note: type is spam or nonspam is in the last column, "type"
spam[,"type"]
spam[, 58] # view last column: response variable

# create response variable
response <- spam[, "type"]
summary(response)
# How many spam and non-spam samples?
class(response)

# create test and training set
index <- sample(1:dim(spam)[1])
spamtrain <- spam[index[1:floor(dim(spam)[1]/2)], ]
spamtest <- spam[index[((ceiling(dim(spam)[1]/2)) + 1):dim(spam)[1]], ]

# build the SVM model 
modelSVM <- ksvm(type~., data=spamtrain) 
error(modelSVM) # check training 'error' 

# measure performance on spamtest data
predictions <- predict (modelSVM, spamtest)

# check contingency table
table(predictions, spamtest[,"type"])
# How many non-spam emails were classified as spam?

# explicitly control the trade-off between false positive
# and false negative errors
# non-spam classified as spam should be considered as
# expensive mistake
modelSVM <- ksvm(type~., data=spamtrain, class.weights=c('spam'=1,'nonspam'=10))

predictions <- predict (modelSVM, spamtest)
table(predictions, spamtest[,"type"])
# How many non-spam emails were classified as spam?


#------------------------------------------------
# Ex.5: Pre-processing bias
#------------------------------------------------
library(graphics)
plot (prcomp(spam[,-58]))
# What is the conclusion about the variance 
# preserved by the first two principal components?

# project spam multi-dimensional (57) data
# onto eigenvectors
pcs <- prcomp(spam[,-58],retx=TRUE)
newspam <- data.frame(pcs$x,spam[,58])
dim(newspam)

# divide data into training and testing data
index <- sample(1:dim(newspam)[1])
newspamtrain <- newspam[index[1:floor(dim(newspam)[1]/2)], c(1:2,58) ]
newspamtest <- newspam[index[((ceiling(dim(newspam)[1]/2)) + 1):dim(newspam)[1]], c(1:2,58)]
dim(newspamtrain)
dim(newspamtest)
class(spamtrain)
class(newspamtrain)
newspamtrain
names(newspamtrain) <- c("PC1","PC2","type")
names(newspamtest) <- c("PC1","PC2","type")
# How did the class labels change?

# use only the first two features to build the model
modelSVM <- ksvm(type~., data=newspamtrain, class.weights=c('spam'=1,'nonspam'=10))
predictions <- predict (modelSVM, newspamtest)
table(predictions, newspamtest[,"type"])
# What is wrong with the entire pipeline?

#------------------------------------------------
# Ex.6: Modeling assumptions: 
# 		linear vs non-linear decision boundary
#     Model complexity: number of support vectors
#------------------------------------------------
data (spirals)
# perform spectral clustering
# to identify two spirals
sc <- specc(spirals, centers=2) 
spiraldata <- data.frame(x=spirals[,1], y=spirals[,2], class=as.factor(sc))
plot(spiraldata[,1],spiraldata[,2])
ksvm(class~., spiraldata, kernel="vanilladot", cross=3)
ksvm(class~., spiraldata, kernel="rbfdot", cross=3)
# How many support vectors are for each model?
# What is the cross-validation error for each model?

#------------------------------------------------
# Ex.7: Modeling assumptions: 
#   	type of decision boundary
#     Type of patterns model is learning
#------------------------------------------------
library(C50)
modelTree <- C5.0(x=spiraldata[,-3],y=spiraldata[,3])
summary(modelTree)
# What type of decision boundary does it capture?
# How reliable are some of the rules?
# Does C5.0 have cross-validation option?

#------------------------------------------------
# Ex.8: Model overfitting: 
#     type of decision boundary
#     Type of patterns model is learning
#     Model complexity: number of support vectors
#------------------------------------------------
x = rbind(matrix(rnorm(120),,2),
          matrix(rnorm(60,mean=3),,2),
          matrix(rnorm(60, mean=-3), , 2))
dim(x)

y = c(rep("1",60), rep("2",60))
cl = c (rep("purple",60), rep("red",60))
plot (x, col=cl, pch=19)
ksvm(x,y,kernel="vanilladot")
# How many support vectors in each case?
# Can you reduce model complexity 
# with a different kernel parameter?

#------------------------------------------------
# Ex.9: Model assumptions: 
#     type of decision boundary
#     Type of patterns model is learning
#------------------------------------------------
x = rbind(cbind(rnorm(30,mean=3),rnorm(30,mean=-3)),
          cbind(rnorm(30,mean=-3),rnorm(30,mean=3)),
          matrix(rnorm(60,mean=3),,2),
          matrix(-rnorm(60, mean=3),,2))
dim(x)

y = as.factor(c(rep("1",60), rep("2",60)))
cl = c (rep("purple",60), rep("red",60))
plot (x, col=cl, pch=19)
ksvm(x,y,kernel="vanilladot")
# Is this model over-fitting? Why?
# Do you think that decision tree 
# should be ok with this data?

# let's clean the environment
rm(list=ls())

