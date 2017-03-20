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
mis_title <- c('Dr', 'Ms', 'Dona')
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

# Data strings trance to integer
train$Embarked[train$Embarked == 'S'] <- 0
train$Embarked[train$Embarked == 'Q'] <- 1
train$Embarked[train$Embarked == 'C'] <- 2
train$Embarked[train$Embarked == ''] <- -1
train$Title[train$Title == 'Mr'] <- 0
train$Title[train$Title == 'Ms'] <- 1
train$Title[train$Title == 'Mrs'] <- 2
train$Title[train$Title == 'Miss'] <- 3

test$Embarked[test$Embarked == 'S'] <- 0
test$Embarked[test$Embarked == 'Q'] <- 1
test$Embarked[test$Embarked == 'C'] <- 2
test$Title[test$Title == 'Mr'] <- 0
test$Title[test$Title == 'Ms'] <- 1
test$Title[test$Title == 'Mrs'] <- 2
test$Title[test$Title == 'Miss'] <- 3

# Delete Data (PassengerId,Name)
train <- train[,c(2,3,7,8,13)]
test  <- test[,c(2,3,7,8,13)]
train[,1] <- as.factor(train[,1])
test[,1]  <- as.factor(test[,1])

# randomforest tuneRF
tune <- tuneRF(train[,-1],train[,1],doBest=T,ntreeTry = 500,stepFactor = T)
# predict
pred.tune <- predict(tune, newdata=test, type='class')
pred.tune <- as.numeric(pred.tune)-1
prob.tune <- predict(tune, newdata=test, type='prob')
roc_table <- cbind(prob.tune, pred.tune)
pred_data <- cbind(gender[,1],pred.tune)
colnames(pred_data) <- c("PassengerId", "Survived")
write.table(pred_data, "./pred_data.txt", quote=F, row.names = F, col.names=T, append=F,sep = ",")
write.table(roc_table[,-1], "./roc_data.txt", quote=F, row.names = F, col.names=F, append=F,sep = ",")

# modelとtuneのConfution Matrixを比較
print("[tune] Confusion Matrix:")
print(table(pred.tune, test[,1]))

# ROC
rocdata <- read.delim("roc_data.txt",sep = ",",header = F)
pred <- prediction(predictions = rocdata$V1, labels = rocdata$V2)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
print("**** AUC ****")
print(auc)

