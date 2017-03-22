# R library's import
library(randomForest)
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(ROCR)

# This's loading TRAIN and TEST data
train  <- read.csv("./../../Kaggle_data/titanic/train.csv",stringsAsFactors = F,na.strings = "NULL")
test   <- read.csv("./../../Kaggle_data/titanic/test.csv",stringsAsFactors = F,na.strings = "NULL")
gender <- read.csv("./../../Kaggle_data/titanic/gender_submission.csv",stringsAsFactors = F,na.strings = "NULL")
test <- merge(gender,test)

# Data Processing (Add Title)
train$Title <- gsub('(.*, )|(\\..*)', '', train$Name)
test$Title  <- gsub('(.*, )|(\\..*)', '', test$Name)

# Show title counts by sex
print(table(train$Sex, train$Title))
print(table(test$Sex, test$Title))

# Titles with very low cell counts to be combined to "rare" level
mis_title  <- c('Dr', 'Ms', 'Dona')
male_title <- c('Capt', 'Col', 'Don', 'Jonkheer', 'Major', 'Master', 'Rev', 'Sir')

###################
# Mr.→未婚既婚関係なく男性
# Mrs.→既婚女性
# Ms.→未婚既婚関係なく女性
# Miss→未婚女性
###################
# Data Processing (Tittle)
train$Title[train$Title == 'Dr' & train$Sex == 'male']  <- 'Mr'
train$Title[train$Title == 'Lady']  <- 'Mrs'
train$Title[train$Title == 'Mlle']  <- 'Miss'
train$Title[train$Title == 'Mme']   <- 'Mrs'
train$Title[train$Title == 'the Countess']  <- 'Mrs'
train$Title[train$Title %in% male_title] <- 'Mr'
train$Title[train$Title %in% mis_title] <- 'Ms'
print(table(train$Sex, train$Title))

test$Title[test$Title == 'Dr' & test$Sex == 'male']  <- 'Mr'
test$Title[test$Title == 'Lady']  <- 'Mrs'
test$Title[test$Title == 'Mlle']  <- 'Miss'
test$Title[test$Title == 'Mme']   <- 'Mrs'
test$Title[test$Title == 'the Countess']  <- 'Mrs'
test$Title[test$Title %in% male_title] <- 'Mr'
test$Title[test$Title %in% mis_title] <- 'Ms'
print(table(test$Sex, test$Title))

# Delete Data (PassengerId,Name)
train <- train[,c(2,3,7,8,13)]
test  <- test[,c(2,3,7,8,13)]
train[,1] <- as.factor(train[,1])
test[,1]  <- as.factor(test[,1])
train[,5] <- as.factor(train[,5])
test[,5]  <- as.factor(test[,5])

num <- sample(nrow(train), nrow(train)*0.7, replace = F)
train1 <- train[num,]
train2 <- train[-num,]

# randomforest tuneRF
tune <- tuneRF(train1[,-1], train1[,1], doBest=T, ntreeTry = 500, stepFactor = T)
print("[tune-train1] Confusion Matrix:")
print(tune$confusion)

## train1
# predict
pred.tune <- predict(tune, newdata=train1, type='class')
prob.tune <- predict(tune, newdata=train1, type='prob')
# ROC and AUC
pred <- prediction(predictions = prob.tune[,2], labels = train1[,1])  #確率値、正解ラベル
perf <- performance(pred, "tpr", "fpr")
plot(perf)
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
print("[tune-train1]AUC")
print(auc)

## train2
# predict
pred.tune <- predict(tune, newdata=train2, type='class')
prob.tune <- predict(tune, newdata=train2, type='prob')
# modelとtuneのConfution Matrixを比較
print("[tune-train2] Confusion Matrix:")
print(table(pred.tune, train2[,1]))
# ROC and AUC
pred <- prediction(predictions = prob.tune[,2], labels = train2[,1])  #確率値、正解ラベル
perf <- performance(pred, "tpr", "fpr")
plot(perf)
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
print("[tune-train2]AUC")
print(auc)

# Kaggle Data create
pred.tune <- predict(tune, newdata=test, type='class')
pred_data <- cbind(gender[,1],pred.tune)
pred_data[,2] <- pred_data[,2]-1
colnames(pred_data) <- c("PassengerId", "Survived")
write.table(pred_data, "./pred_data.txt", quote=F, row.names = F, col.names=T, append=F,sep = ",")
