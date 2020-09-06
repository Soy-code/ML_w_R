setwd("D:\\BITAmin\\Machine Learning with R, Second Edition_Code\\Chapter 05")

## Collecting data
credit=read.csv('credit.csv') 
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default)

# Data preparation - creating random training and test datasets
set.seed(123)
train_sample=sample(1000, 900)
str(train_sample)
credit_train=credit[train_sample, ]
credit_test=credit[-train_sample, ]
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

## Training a model on the data
library(C50)
credit_model = C5.0(credit_train[-17], credit_train$default)
credit_model
summary(credit_model)

## Evaluating model performance
credit_pred = predict(credit_model, credit_test)
library(gmodels)
CrossTable(credit_test$default, credit_pred, prop.chisq=FALSE, prop.c=FALSE, prop.r=FALSE, 
           dnn=c('actual default', 'predicted default'))

## Improving model performance
# Boosting the accurace of decision trees
credit_boost10 = C5.0(credit_train[-17], credit_train$default, trials= 10)
credit_boost10
summary(credit_boost10)
credit_boost_pred10 = predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = F, prop.c = F, prop.r = F, 
           dnn = c('actual default', 'predicted default'))

# Making mistakes more costlier than others 
matrix_dimensions = list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) = c("predicted", "actual")
matrix_dimensions
error_cost=matrix(c(0,1,4,0), nrow=2, dimnames=matrix_dimensions)
error_cost
credit_cost=C5.0(credit_train[-17], credit_train$default, costs=error_cost)
credit_cost_pred=predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred, prop.chisq = F, prop.c=F, prop.r=F, 
           dnn=c('actual default', 'predicted default'))

## Collecting data
## Exploring and preparing the data
mushrooms =read.csv('mushrooms.csv', stringsAsFactors = T)
str(mushrooms)
mushrooms$veil_type = NULL 
table(mushrooms$type)

## Training a model on the data
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_191")
library(rJava)
library(RWeka)
mushroom_1R=OneR(type ~., data=mushrooms)
mushroom_1R

## Evaluating model performance
summary(mushroom_1R)

## Improving model performance
mushroom_JRip = JRip(type~., data=mushrooms)
mushroom_JRip