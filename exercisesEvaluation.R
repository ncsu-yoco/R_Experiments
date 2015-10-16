#
# @author: Nagiza Samatova
#

#-----------------------------------------------
# Ex.1: Build and apply logistic regression model
#-----------------------------------------------
getwd() # check you working directory
setwd("/Users/capsci/Documents/AdvancedAlgos591/Set4/")
# place spamD.tsv file into working directory
spamD <- read.table('spamD.tsv', header=T, sep='\t')
names(spamD)
dim(spamD)

spamTrain <- subset (spamD, spamD$rgroup >= 10)
spamTest <- subset (spamD, spamD$rgroup<10)
dim(spamTrain)
dim(spamTest)

spamVars <- setdiff(colnames(spamD), list('rgroup','spam'))
spamVars
spamFormula <- as.formula(paste('spam=="spam"',
	paste(spamVars, collapse=' + '), sep=' ~ '))
spamFormula

# build a logistic regression model
# that evaluates probability of belonging to each class
spamModel <- glm(spamFormula, family=binomial(link='logit'),
		data=spamTrain)

# generate predictions for training and test data
# using the spamModel
spamTrain$pred <- predict(spamModel, newdata=spamTrain,
	type='response')
spamTest$pred <- predict(spamModel, newdata=spamTest,
	type='response')

# note: columns are predicted class and rows are actual class
print(with(spamTest, table(y=spam, glmPred=pred>0.5)))

#-----------------------------------------------
# Ex.2: Create confusion matrix
#-----------------------------------------------
cM <- table(truth=spamTest$spam,prediction=spamTest$pred>0.5)
print(cM)

#-----------------------------------------------
# Ex.3: Performance Measures derived from confusion matrix
#-----------------------------------------------
# enter confusion matrix data by hand
# for the Akismet filter that uses link destination clues
# and determination from other websites
# in addition to text features

#ct <- as.table(matrix(data=c(288-1,17,1,13882-17), nrow=2, ncol=2))
# New table based on calculated values
ct <- as.table(matrix(data=c(264, 22 , 14, 158), nrow=2, ncol=2))
rownames(ct) <- rownames(cM)
colnames(ct) <- colnames(cM)
print(ct)

#Write an R code to compute performance measures 
#(Accuracy, Precision, Recall, Sensitivity, Specifcity, F1-measure)
#for the Akismet filter for spam vs. non-spam emails .

TP <- ct["spam","TRUE"]
TN <- ct["non-spam","FALSE"]
FP <- ct["non-spam","TRUE"]
FN <- ct["spam","FALSE"]

# Accuracy : fraction of correct predictions
accuracy <- (TP + TN) / (TP + TN + FP + FN)
# Precision : fraction of predicted positive which are actually positive
precision <- TP / (TP + FP)
# Recall : fraction of correctly identified positives
recall <- TP / (TP + FN)
# Sensitiviy : same as recall
sensitivity <- recall
# Specificity : fraction of correctly identified negative
specificity <- TN / (TN + FP)
# F1-measure : lower Precision/Recall implies lower F1-measure
f1_measure <- ( 2 * precision * recall ) / ( precision + recall )

#Report the values of each performance measure.

print(paste("Accuracy : ", signif(accuracy,4)))
#Ans. "Accuracy :  0.9214"
print(paste("Precision : ", signif(precision,4)))
#Ans. "Precision :  0.9186"
print(paste("Recall : ", signif(recall,4)))
#Ans. "Recall :  0.8778"
print(paste("Sensitivity : ", signif(sensitivity,4)))
#Ans. "Sensitivity :  0.8778"
print(paste("Specificity : ", signif(specificity,4)))
#Ans. "Specificity :  0.9496"
print(paste("F1-measure : ",  signif(f1_measure,4)))
#Ans. "F1-measure :  0.8977"
#How do these values compare with the measures derived
#from cM of the glmModel?
Ans. They always evaluate less than the values derived from confusion matrix
which was orginally given(Akismet filter).
#-----------------------------------------------
# Ex.4: ROC Curve
#-----------------------------------------------
# install ROCR package and load it
library('ROCR')
eval <- prediction(spamTest$pred, spamTest$spam)

plot(performance(eval,"tpr","fpr"))

print(attributes(performance(eval,'auc'))$y.values[[1]])

# Given the confusion matrix for the Akismet spam filter,
# is the ROC curve what one would expect?
Ans. Yes

# What is the AUC value of this filter?
# Is it close to a perfect classifier? Why?
Ans. Yes. Area under curve is 0.96, which is very close to one.
    Hence, it is close to perfect classifier

#-----------------------------------------------
# Ex.5: Log Likelihood estimation for the Null model
#-----------------------------------------------
# the number of known spam emails
pNull <- sum(ifelse(spamTest$spam=='spam',1,0))
pNull

# rescale pNull by the number of points
# to give a rough average surprize per point
dim(spamTest)[1]
pNull <- pNull/dim(spamTest)[1]
pNull


sum(ifelse(spamTest$spam=='spam',1,0))*log(pNull)+
  sum(ifelse(spamTest$spam=='spam',0,1))*log(1-pNull)
          
# What is the log likelihood estimation for the Null model?
Ans. -306.8952

#-----------------------------------------------
# Ex.6: Log Likelihood estimation for the glm spam model
#-----------------------------------------------
sum(ifelse(spamTest$spam=='spam',
           log(spamTest$pred),
           log(1-spamTest$pred)))

# What is the log likelihood estimation for the glm model?
Ans. -134.9487
# Is it better than the null model? How do you know that?
Ans. Value of LL should be maximum. Hence glm model is better than null model
# scaled by the number of data points
sum(ifelse(spamTest$spam=='spam',
           log(spamTest$pred),
           log(1-spamTest$pred))) / dim(spamTest)[1]
