# Import required packages
library(ggplot2)
library(tidyverse)
library(readr)
library(caTools)
library(rmarkdown)
library(knitr)
library(caret)

# Importing the dataset
census <- read_csv("my document/Find Donor for Charity/census.csv")

# Inspect the data
colnames(census)
dim(census)
str(census)
summary(census)

# Definition of target variable:
# 1 -  if the salary bucket is above 50K
# 0 -  if the salary bucket is at most 50K

# From the objective of the model, clearly logistic regrestion is the suitable model beacuse the target variabe is a dichotmous (0,1) variable.

# Let's convert dependent variable to numeric, this will help us when we use one hot encoding in Caret to create
# dummy variables for each level of a categorical variable.
census$Salary_Flag<- NA
census$Salary_Flag[census$income=="<=50K"]<- 0
census$Salary_Flag[census$income==">50K"]<- 1

# We won't need income variable anymore now that we created the Salary_Flag variable, 
# Note Education_level and education_num are the same so we have to eliminate one i.e. education_level
Data <- census[,-c(3,14)]

# Rename colunms 'capital-gain' and 'capital-loss' to be in a friendly structure
colnames(Data)[9]<- "capital_gain"
colnames(Data)[10]<- "capital_loss"

# Apply logarithm algorhm on skewed continuos features (capital_gain & capital_loss)
Data$CG <- ifelse(Data$capital_gain==0,log(Data$capital_gain+1),log(Data$capital_gain))
Data$CL <- ifelse(Data$capital_loss==0,log(Data$capital_loss+1),log(Data$capital_loss))

newdata <- Data[,-c(9,10)]


# one hot encoding 
dummy <- dummyVars(" ~ .", data=newdata)
newdata <- data.frame(predict(dummy, newdata = newdata))
# Data partition
view(newdata)
# Split the data into Train (70%) and Test datasets (30%)

## 75% of the sample size
smp_size <- floor(0.7 * nrow(newdata))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(newdata)), size = smp_size)

train <- newdata[train_ind, ]
test <- newdata[-train_ind, ]
names(newdata)
# We develop the model from Train dataset and test dataset will be use to measure the model accuracy

model <- glm(Salary_Flag~.,data=train,family = "binomial")
summary(model)

# Remove all the insignificant in the model with the aid of p-value i.e 5% as a cutoff.
newdata1 <- newdata[,c(1,2,9,11,12,14,22:29,31:35,37,38,42,44,47,68,69,79,81,82,85:88)]
newdata2 <- newdata1[,-c(26,25,32,30,13)]
set.seed(123)
train_ind2<- sample(seq_len(nrow(newdata2)),size = smp_size)

train2<- newdata2[train_ind2,]
tes2 <- newdata2[-train_ind2,]

model1 <- glm(Salary_Flag~.,data=train2, family = binomial)
summary(model1)

trn_pred <- ifelse(predict(model1, newdata = tes2, type = "response") > 0.5, 1, 0)


trn_tab <- table(predicted = trn_pred, tes2 = tes2$Salary_Flag)
trn_tab

# Model accuracy
library(caret)
library(e1071)
confusionMatrix(trn_tab)
# Predict the salary flag as the response variable
library("pROC")
test_prob <- predict(model1, newdata = tes2, type = "response")
test_roc <- roc(tes2$Salary_Flag ~ test_prob, plot = TRUE, print.auc = TRUE)
