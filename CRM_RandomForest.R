
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

#training a model using random forest#
#######################################
library(randomForest)

#training a model
creditRandFor <- randomForest(as.factor(train$default) ~., data = train[,c(-17,-18)],nodesize =25, ntree = 200)
summary(creditRandFor) #summary of the model output

#identifying the most important variables based on mean gini decrease
varImpPlot(creditRandFor)
#Note : Show how each split result in low impurities or increased homogeneity

#predicting on test data
predictCreditRandFor <- predict(creditRandFor, newdata = test[,c(-17,-18)])

#computing the accuracy of the model
table(test$default,predictCreditRandFor) #since we dont have a probability here so we dont set a threshold

accuracyCreditRandFor <- ((as.matrix(table(test$default, predictCreditRandFor))[1,1]) + (as.matrix(table(test$default, predictCreditRandFor))[2,2]))/nrow(test)

#computing the baseline model for comparison
baseLineAccuracy <- max(table(test$default))/nrow(test)

print(accuracyCreditRandFor)
print(baseLineAccuracy)
#Note: Our random forest model beats the basline model in terms of accuracy

#assesing the robustness of model
library(ROCR)
rocrPredictCreditRandFor <- prediction((predict(creditRandFor, newdata = test[,c(-17,-18)], type = "prob")[,2]), test$default) #getting probability and then picking predicted class
areaUnderCurve <- as.numeric(performance(rocrPredictCreditRandFor, "auc")@y.values) #out of sample auc
print(areaUnderCurve)
#Very high area under the curve but slighltly less than logistic regression
#Very high accuracy as good as logistic regression

#tuning a model using random forest#
#######################################
#Note : We can tune it using tuneRF package but repeated cross validation using caret produces much better results
library(caret)

#tuning for mtry, this the number of variables randomly sampled for splits
tuneGridRandFor <- expand.grid(.mtry=c(1:sqrt(ncol(train[,c(-17,-18)]))))

#creating a list of parameters to be passed onto the model
fitControlRandFor <- trainControl(method = "repeatedcv",
                             number = 5, repeats = 3,
                             #fivefold cross validation repeated 10 times
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary)

tunedCreditRandFor <- train(as.factor(train$default) ~., data = train[,c(-17,-18)],
                            method = "rf",
                            trControl = fitControlRandFor,
                            verbose = TRUE,
                            metric = "ROC",
                            tuneGrid = data.frame(tuneGridRandFor),
                            importance = TRUE)

tunedPredictCreditRandFor <- predict(tunedCreditRandFor, newdata = test[,c(-17,-18)])

#computing the accuracy of the model
table(test$default,tunedPredictCreditRandFor) #since we dont have a probability here so we dont set a threshold

accuracyTunedCreditRandFor <- ((as.matrix(table(test$default, tunedPredictCreditRandFor))[1,1]) + (as.matrix(table(test$default, tunedPredictCreditRandFor))[2,2]))/nrow(test)