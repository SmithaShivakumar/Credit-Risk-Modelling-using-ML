
#setting up the data and performing high level analysis#
########################################################
#downloading the data
#https://raw.githubusercontent.com/obaidpervaizgill/CreditRiskModelling/master/credit.csv

#loading data
credit <- read.csv("credit.csv")

#identifying the structure of variables
str(credit)

#getting summary of the variables
summary(credit)

#getting the column names
colnames(credit)
#[1] "checking_balance"     "months_loan_duration" "credit_history"       "purpose"              "amount"               "savings_balance"
#[7] "employment_duration"  "percent_of_income"    "years_at_residence"   "age"                  "other_credit"         "housing"
#[13] "existing_loans_count" "job"

#tabulating dependent variables
table(credit$default)

#No missing values in the data
#Havent had to normalize or standardize data
#Would have removed correlated features if there was 80% correlation

#spliting data into test and train
library(caTools)
split <- sample.split(credit$default, SplitRatio = 0.70)
train <- subset(cbind(credit,split), cbind(credit,split)$split == TRUE)
test <- subset(cbind(credit,split), cbind(credit,split)$split == FALSE)

#checking proportions across train and test
prop.table(table(train$default))
prop.table(table(test$default))

#training a model using decision trees#
#######################################
library("rpart")
library("rpart.plot")

#training a model
creditDecTree <- rpart(train$default ~ ., data = train[,c(-17,-18)], method = "class", minbucket = 1) #min bucket is minimum number of observations in a terminal nore
summary(creditDecTree) #summary of the model output

#plotting a decision tree to see splits
prp(creditDecTree)

#predicting on test data
predictCreditDecTree <- predict(creditDecTree, newdata = test[,c(-17,-18)], type = "class") #getting classes rather than probability

#computing the accuracy of the model
table(test$default,predictCreditDecTree) #since we dont have a probability here so we dont set a threshold

accuracyCreditDecTree <- ((as.matrix(table(test$default, predictCreditDecTree))[1,1]) + (as.matrix(table(test$default, predictCreditDecTree))[2,2]))/nrow(test)

#computing the baseline model for comparison
baseLineAccuracy <- max(table(test$default))/nrow(test)

print(accuracyCreditDecTree)
print(baseLineAccuracy)
#Our decision tree model beats the basline model in terms of accuracy

#assesing the robustness of model
library(ROCR)
rocrPredictCreditDecTree <- prediction((predict(creditDecTree, newdata = test[,c(-17,-18)])[,2]), test$default) #getting probability and then picking predicted class
areaUnderCurve <- as.numeric(performance(rocrPredictCreditDecTree, "auc")@y.values) #out of sample auc
print(areaUnderCurve)

#tuning a model using decision trees#
#####################################
library(caret)

#tuning for complexity parameter, this penalizes model complexity and avoids overfitting
tuneGridDecTree <- expand.grid(.cp=seq(0.01,0.5,0.01))

#creating a list of parameters to be passed onto the model
fitControlDecTree <- trainControl(method = "cv", number = 10)


tunedCreditDecTree <- train(train$default ~., data = train[,c(-17,-18)],
                            method = "rpart",
                            trControl = fitControlDecTree,
                            tuneGrid = tuneGridDecTree)

tunedPredictCreditDecTree <- predict(tunedCreditDecTree, newdata=test[,c(-17,-18)], type="raw")

#copmuting the accuracy of the model
table(test$default,tunedPredictCreditDecTree) #since we dont have a probability here so we dont set a threshold

accuracyTunedCreditDecTree <- ((as.matrix(table(test$default, tunedPredictCreditDecTree))[1,1]) + (as.matrix(table(test$default, tunedPredictCreditDecTree))[2,2]))/nrow(test)
