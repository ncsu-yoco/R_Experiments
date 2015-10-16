#
# @author: Nagiza Samatova
#
# load kernlab package <-- install if you do not have it
library(kernlab)

#-----------------------------------------------
# Ex.1: Too good to be true
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
Ans. 4 out of 150 samples where misclassified
# What is the error of the model? 
Ans. Eroor for above model is 2.66%
# Is this a good model?
Ans. It is not a good model as we are testing our model on the trained data.
(Our training data is already biased towards our data we are going to test)

#-----------------------------
# Ex.2: Looks like legitimate validation
#-----------------------------
trainingData <- iris[1:100,]
testData <- iris[101:150,]
modelSVM <- ksvm(x=as.matrix(trainingData[,-5]), y=trainingData[,5])
predictions <- predict(modelSVM,testData[,-5])
table (predictions,testData[,5]) 
# What is the test error?
Ans. Test Error : 0%
# Is this a good model?
Ans. It is not a good model. The the training data and test data contained different classes.
The training data had no examples similar to tes data.

#-------------------------------
# Ex.3: Cross-validation
#-------------------------------
# perform 3-fold cross-validation
modelSVM <- ksvm(Species~., data=iris, cross=3)
# which is equivalent to (check help(ksvm)):
modelSVM <- ksvm(x=as.matrix(iris[,-5]), y=iris[,5], cross=150)
modelSVM
cross(modelSVM)
# What is the training error?
Ans. Training error is 2.66%
# What is 3-fold cross-validation error?
Ans. 3-fold cross validation error is 6%
# What is 10-fold cross-validation error?
Ans. 10-fold cross validation error is 4.66%
# What is the leave-one-out cross-validation error?
Ans. leave-one-out cross validation error is 3.33%

#------------------------------------------------
# Ex.4: Can bias be useful?
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
Ans. spam : 1813, non-spam : 2788
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
Ans. 58 non-spam emails were incorrectly classified as spam

# explicitly control the trade-off between false positive
# and false negative errors
# non-spam classified as spam should be considered as
# expensive mistake
modelSVM <- ksvm(type~., data=spamtrain, class.weights=c('spam'=1,'nonspam'=10))

predictions <- predict (modelSVM, spamtest)
table(predictions, spamtest[,"type"])
# How many non-spam emails were classified as spam?
Ans. 21 non-spam emails were incorrectly classified as spam

#------------------------------------------------
# Ex.5: Multi-step analytical pipeline
#------------------------------------------------
library(graphics)
plot (prcomp(spam[,-58]))
# What is the conclusion about the variance 
# preserved by the first two principal components?
Ans. Most of the examples can be classified based on the first two components

# project spam multi-dimensional (57) data
# onto eigenvectors
pcs <- prcomp(spam[,-58],retx=TRUE)
newspam <- data.frame(pcs$x,spam[,58])
dim(newspam)

# divide data into training and testing data
# project onto the first two principal components
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
Ans. names() functions updated the column names of data frame

# use only the first two features to build the model
modelSVM <- ksvm(type~., data=newspamtrain, class.weights=c('spam'=1,'nonspam'=10))
predictions <- predict (modelSVM, newspamtest)
table(predictions, newspamtest[,"type"])
# What is wrong with the entire pipeline?
Ans. The data was classified at very begining

#------------------------------------------------
# Ex.6: Parameter Selection (see slide)
#------------------------------------------------


#------------------------------------------------
# Ex.7: Model Complexity
#------------------------------------------------
data (spirals)
# perform spectral clustering
# to identify two spirals
sc <- specc(spirals, centers=2) 
spiraldata <- data.frame(x=spirals[,1], y=spirals[,2], class=as.factor(sc))
plot(spiraldata[,1],spiraldata[,2])
ksvm(class~., spiraldata, kernel="vanilladot", cross=3)
ksvm(class~., spiraldata, kernel="rbfdot", cross=3)
ksvm(class~., spiraldata, cross=3)
# How many support vectors are for each model?
Ans. Number of support vectors
    vanilladot : 228
    rbfdot : 161
    default : 190
# What is the cross-validation error for each model?
Ans. Cross-validation error
    vanilladot : 37%
    rbfdot : 4%
    default : 23%
# Which model is more complex?
Ans. model using vanilladot kernel is most complex as it has most number of support vectors

#------------------------------------------------
# Ex.8: Model complexity and reliability 
#------------------------------------------------
library(C50)
modelTree <- C5.0(x=spiraldata[,-3],y=spiraldata[,3])
summary(modelTree)
# What type of decision boundary does it capture?
Ans. Boundaries based on values of x and y
# How reliable are some of the rules?
Ans. Some of the classifications rules were established based on very few number 
  of samples(say 5 samples). Rules based on low number of samples are not very realiable
  
# Does C5.0 have cross-validation option?
Ans. No. It does not have a cross-validation option

#------------------------------------------------
# Ex.9: Model overfitting 
#------------------------------------------------
x = rbind(matrix(rnorm(120),,2),
          matrix(rnorm(60,mean=3),,2),
          matrix(rnorm(60, mean=-3), , 2))
dim(x)

y = c(rep("1",60), rep("2",60))
cl = c (rep("purple",60), rep("red",60))
plot (x, col=cl, pch=19)
ksvm(x,y,kernel="vanilladot")
ksvm(x,y,kernel="rbfdot")
# How many support vectors in each case?
Ans. Number of support vectors
      vanilladot : 118
      rbfdot : 27
# Can you reduce model complexity? 
# with a different kernel parameter?
Ans. The model uses less number of support vectors with ANOVA RBF kernel

#------------------------------------------------
# Ex.10: Model assumptions 
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
Ans. Yes. It uses all the available columns as features
# Do you think that decision tree 
# should be ok with this data?
Ans. Yes


# let's clean the environment
rm(list=ls())

