setwd("C:\\Users\\kdoyl\\OneDrive\\Documents\\IST 707")

library(readr)
titanic_train <- read_csv("titanic-train.csv")
titanic_test <- read_csv("titanic-test.csv")
install.packages("ggplot2")
install.packages("caret")
library(ggplot2)
library(caret)
install.packages("RWeka")
library("RWeka")

str(titanic_train)
titanic_train$Sex=factor(titanic_train$Sex)
titanic_train$Survived=(titanic_train$Survived)
titanic_train$Pclass=factor(titanic_train$Pclass)
set.seed(9999)
# 
# ## Randomly sample cases to create independent training and test data
Partition <- createDataPartition(y = titanic_train$Survived,p = 0.7, list = FALSE)
Training = titanic_train[Partition,] # Create the training sample
dim(Training)
Test = titanic_train[-Partition,] # Create the test sample
dim(Test)
# 
prop.table(table(Training$Survived))
prop.table(table(Test$Survived))
#Pruned
Pruned <- J48(Survived~., data = Training, control = Weka_control())
# Pruned
summary(Pruned)
# 
# #test
 Test_Titanic <- predict(Pruned,Test)
# 
# 
CrossTable(Test$Survived,Test_Titanic,
           prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,dnn = c('Actual Survival','Predicted Survival'))