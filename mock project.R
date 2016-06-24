install.packages("caret", dependencies = c("Depends", "Suggests"))

#set seed to make partition reproductible, use 2/3 of original data for train data
set.seed(1234)
ind<-sample(2, nrow(chowdary), replace=TRUE, prob=c(0.67, 0.33))

#define train and test datasets
train<-`chowdary`[ind==1, 2:183] #use value in ind to define train & test sets
test<-`chowdary`[ind==2, 2:183] #2:183 leaves out type b/c we want to predict that

#store class labels in factor vectors & divide over train/test datasets
chowdary.trainLabels<- `chowdary`[ind==1, 1] 
chowdary.testLabels<- `chowdary`[ind==2, 1]

#result is a factor vector w/ predicted classes for each row of test data
chow_pred<- knn(train = train, test = test, cl = chowdary.trainLabels, k=2)
chow_pred

#evaluation of training model
#one way is to compare results of chow_pred to test labels previously defined

chowdary.testLabels #appears that the model predicts w/ 100% accuracy

#deeper analysis of training model

library(gmodels) 

CrossTable(x = chowdary.testLabels, y = chow_pred, prop.chisq=FALSE) #cross tabulation
#if all predictions is correct for a species, there will only be one box with a number and the rest 0

#Create confusion matrix

library(MASS)

chowdary.lda <- lda(type ~ . , data = chowdary)
table(predict(chowdary.lda, type="class")$class, chowdary$type)

#ROC Curve

install.packages("pROC")
library(pROC)


