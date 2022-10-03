
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

#training a model using logistic regression#
############################################

#training a model
creditLogReg <- glm(train$default ~ ., data = train[,c(-17,-18)], family = "binomial" ) #removing split feature and dependent variable
summary(creditLogReg) #summary of the model output
#In theory I should rerun the model removing the non-significant features but since I want to demonstrate multiple model usage I would let it slide

#predicing on test data
predCreditLogReg  0.5)
#we want our model to be optimally sensitive hence we use 0.5 as the threshold, redudcing the threshold will make the model more sensitive

#computing the accuracy of the model
accuracyCreditLogReg  0.5))[1,1]) + (as.matrix(table(test$default, predCreditLogReg > 0.5))[2,2]))/nrow(test)

#computing the baseline model for comparison
baseLineAccuracy <- max(table(test$default))/nrow(test)

print(accuracyCreditLogReg)
print(baseLineAccuracy)
#Our simple logistic regression model beats the baseline model

#assesing the robustness of model
library(ROCR)
rocrPredCreditLogReg <- prediction(predCreditLogReg,test$default)
areaUnderCurve <- as.numeric(performance(rocrPredCreditLogReg, "auc")@y.values) #out of sample auc
print(areaUnderCurve)
#Closer to 1 is better, 0.78 here is not bad for a first model