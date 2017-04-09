# R library's import
library(randomForest)
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(ROCR)
library(e1071)

# This's loading TRAIN and TEST data
train  <- read.csv("./../../Kaggle_data/titanic/train.csv", stringsAsFactors = F, na.strings = "NULL")
test   <- read.csv("./../../Kaggle_data/titanic/test.csv",  stringsAsFactors = F, na.strings = "NULL")
train_ <- train
gender <- read.csv("./../../Kaggle_data/titanic/gender_submission.csv", stringsAsFactors = F, na.strings = "NULL")
test <- merge(gender,test)

# Processing missing values
train$Fare <- matrix(train$Fare) %>% impute(what = "median")
test$Fare  <- matrix(test$Fare)  %>% impute(what = "median")
train$Age  <- matrix(train$Age)  %>% impute(what = "mean")
test$Age   <- matrix(test$Age)   %>% impute(what = "mean")

# This temporary is used for conversion for Hist Gram
hist_tmp <- train

# ->>>>>> Attention male/female
hist_tmp$Sex[hist_tmp$Sex == "male"]   <- 0
hist_tmp$Sex[hist_tmp$Sex == "female"] <- 1
hist(as.integer(hist_tmp$Sex[hist_tmp$Survived == 0]), breaks=2,
     xlab ="Sex", ylab = "Survive", main = "Sex" , col = "#ff00ff50", labels = T, ylim = c(0,500))
hist(as.integer(hist_tmp$Sex[hist_tmp$Survived == 1]), breaks=2,
     xlab ="Sex", ylab = "Survive", main = "Sex" , col = "#00000050", labels = T, add = T)
# ->>>>>> Attention male/female
# ->>>>>> Attention Master Miss Mr Mrs
hist_tmp$Title <- gsub('(.*, )|(\\..*)', '', hist_tmp$Name)
table(hist_tmp$Sex, hist_tmp$Title)
#    Capt Col Don  Dr Jonkheer Lady Major Master Miss Mlle Mme  Mr Mrs  Ms Rev Sir the Countess
# 0    1   2   1   6        1    0     2     40    0    0   0 517   0   0   6   1            0
# 1    0   0   0   1        0    1     0      0  182    2   1   0 125   1   0   0            1
hist_tmp$Title[hist_tmp$Title == "Master"] <- 1
hist(as.integer(hist_tmp$Title[hist_tmp$Survived == 0 & hist_tmp$Title == "1"]), breaks=2,
     xlab ="Master", ylab = "Survive", main = "Master" , col = "#ff00ff50", labels = T, ylim = c(0,25))
hist(as.integer(hist_tmp$Title[hist_tmp$Survived == 1 & hist_tmp$Title == "1"]), breaks=2,
     xlab ="Master", ylab = "Survive", main = "Master" , col = "#00000050", labels = T, add = T)
# ->>>>>> It isn't a good result
hist_tmp$Title[hist_tmp$Title == "Miss"] <- 2
hist(as.integer(hist_tmp$Title[hist_tmp$Survived == 0 & hist_tmp$Title == "2"]), breaks=2,
     xlab ="Miss", ylab = "Survive", main = "Miss" , col = "#ff00ff50", labels = T, ylim = c(0,150))
hist(as.integer(hist_tmp$Title[hist_tmp$Survived == 1 & hist_tmp$Title == "2"]), breaks=2,
     xlab ="Miss", ylab = "Survive", main = "Miss" , col = "#00000050", labels = T, add = T)
# ->>>>>> It's a good result
hist_tmp$Title[hist_tmp$Title == "Mr"] <- 3
hist(as.integer(hist_tmp$Title[hist_tmp$Survived == 0 & hist_tmp$Title == "3"]), breaks=2,
     xlab ="Mr", ylab = "Survive", main = "Mr" , col = "#ff00ff50", labels = T, ylim = c(0,520))
hist(as.integer(hist_tmp$Title[hist_tmp$Survived == 1 & hist_tmp$Title == "3"]), breaks=2,
     xlab ="Mr", ylab = "Survive", main = "Mr" , col = "#00000050", labels = T, add = T)
# ->>>>>> It's a very good result
hist_tmp$Title[hist_tmp$Title == "Mrs"] <- 4
hist(as.integer(hist_tmp$Title[hist_tmp$Survived == 0 & hist_tmp$Title == "4"]), breaks=2,
     xlab ="Mrs", ylab = "Survive", main = "Mrs" , col = "#ff00ff50", labels = T, ylim = c(0,130))
hist(as.integer(hist_tmp$Title[hist_tmp$Survived == 1 & hist_tmp$Title == "4"]), breaks=2,
     xlab ="Mrs", ylab = "Survive", main = "Mrs" , col = "#00000050", labels = T, add = T)
# ->>>>>> It's a very good result
# ->>>>>> Attention Master Miss Mr Mrs
# ->>>>>> Attention Emarked
hist_tmp$Embarked[hist_tmp$Embarked == "C"] <- 1
hist(as.integer(hist_tmp$Embarked[hist_tmp$Survived == 0 & hist_tmp$Embarked == "1"]), breaks=2,
     xlab ="C", ylab = "Survive", main = "C" , col = "#ff00ff50", labels = T, ylim = c(0,500))
hist(as.integer(hist_tmp$Embarked[hist_tmp$Survived == 1 & hist_tmp$Embarked == "1"]), breaks=2,
     xlab ="C", ylab = "Survive", main = "C" , col = "#00000050", labels = T, add = T)
# ->>>>>> It is not a very good result
hist_tmp$Embarked[hist_tmp$Embarked == "S"] <- 2
hist(as.integer(hist_tmp$Embarked[hist_tmp$Survived == 0 & hist_tmp$Embarked == "2"]), breaks=2,
     xlab ="C", ylab = "Survive", main = "S" , col = "#ff00ff50", labels = T, ylim = c(0,500))
hist(as.integer(hist_tmp$Embarked[hist_tmp$Survived == 1 & hist_tmp$Embarked == "2"]), breaks=2,
     xlab ="C", ylab = "Survive", main = "S" , col = "#00000050", labels = T, add = T)
# ->>>>>> It is a good result
hist_tmp$Embarked[hist_tmp$Embarked == "Q"] <- 3
hist(as.integer(hist_tmp$Embarked[hist_tmp$Survived == 0 & hist_tmp$Embarked == "3"]), breaks=2,
     xlab ="Q", ylab = "Survive", main = "Q" , col = "#ff00ff50", labels = T, ylim = c(0,500))
hist(as.integer(hist_tmp$Embarked[hist_tmp$Survived == 1 & hist_tmp$Embarked == "3"]), breaks=2,
     xlab ="Q", ylab = "Survive", main = "Q" , col = "#00000050", labels = T, add = T)
# ->>>>>> It is not a very good result
# ->>>>>> Attention Emarked
# ->>>>>> Attention SibSp/Parch
hist(as.integer(hist_tmp$SibSp[hist_tmp$Survived == 0]), breaks=seq(0,10,1),
     xlab ="SibSp", ylab = "Survive", main = "SibSp" , col = "#ff00ff50", labels = T, ylim = c(0,550))
hist(as.integer(hist_tmp$SibSp[hist_tmp$Survived == 1]), breaks=seq(0,10,1),
     xlab ="SibSp", ylab = "Survive", main = "SibSp" , col = "#00000050", labels = T, add = T)
table(hist_tmp$Survived, hist_tmp$SibSp)
hist(as.integer(hist_tmp$Parch[hist_tmp$Survived == 0]), breaks=seq(0,10,1),
     xlab ="Parch", ylab = "Survive", main = "Parch" , col = "#ff00ff50", labels = T, ylim = c(0,550))
hist(as.integer(hist_tmp$Parch[hist_tmp$Survived == 1]), breaks=seq(0,10,1),
     xlab ="Parch", ylab = "Survive", main = "Parch" , col = "#00000050", labels = T, add = T)
table(hist_tmp$Survived, hist_tmp$Parch)
hist_tmp$SibSpAndParch <- hist_tmp$SibSp + hist_tmp$Parch
hist(as.integer(hist_tmp$SibSpAndParch[hist_tmp$Survived == 0]), breaks=seq(0,10,1),
     xlab ="SibSpAndParch", ylab = "Survive", main = "SibSpAndParch" , col = "#ff00ff50", labels = T, ylim = c(0,550))
hist(as.integer(hist_tmp$SibSpAndParch[hist_tmp$Survived == 1]), breaks=seq(0,10,1),
     xlab ="SibSpAndParch", ylab = "Survive", main = "SibSpAndParch" , col = "#00000050", labels = T, add = T)
table(hist_tmp$Survived, hist_tmp$SibSpAndParch)
# ->>>>>> Attention SibSp/Parch
# ->>>>>> Attention Cabin
hist_tmp$CabinA <- gsub('(A.*)', '0', hist_tmp$Cabin)
hist_tmp$CabinB <- gsub('(B.*)', '1', hist_tmp$CabinA)
hist_tmp$CabinC <- gsub('(C.*)', '2', hist_tmp$CabinB)
hist_tmp$CabinD <- gsub('(D.*)', '3', hist_tmp$CabinC)
hist_tmp$CabinE <- gsub('(E.*)', '4', hist_tmp$CabinD)
hist_tmp$CabinF <- gsub('(F.*)', '5', hist_tmp$CabinE)
hist_tmp$Cabin2 <- gsub('([G-Z].*)', '6', hist_tmp$CabinF)
hist(as.integer(hist_tmp$Cabin2[hist_tmp$Survived == 0]), breaks=6,
     xlab ="Cabin2", ylab = "Survive", main = "Cabin2" , col = "#ff00ff50", labels = T, ylim = c(0,550))
hist(as.integer(hist_tmp$Cabin2[hist_tmp$Survived == 1]), breaks=6,
     xlab ="Cabin2", ylab = "Survive", main = "Cabin2" , col = "#00000050", labels = T, add = T)
# ->>>>>> last tuning
# ->>>>>> Attention Cabin
# ->>>>>> Attention Ticket
table_tmp <- as.data.frame(table(hist_tmp$Survived,hist_tmp$Ticket))
# ->>>>>> Attention Ticket
# ->>>>>> Attention Pclass
hist(as.integer(hist_tmp$Pclass[hist_tmp$Survived == 0 & hist_tmp$Pclass == "1"]), breaks=2,
     xlab ="Pclass", ylab = "Survive", main = "Pclass" , col = "#ff00ff50", labels = T, ylim = c(0,500))
hist(as.integer(hist_tmp$Pclass[hist_tmp$Survived == 1 & hist_tmp$Pclass == "1"]), breaks=2,
     xlab ="Pclass", ylab = "Survive", main = "Pclass" , col = "#00000050", labels = T, add = T)
hist(as.integer(hist_tmp$Pclass[hist_tmp$Survived == 0 & hist_tmp$Pclass == "2"]), breaks=2,
     xlab ="Pclass", ylab = "Survive", main = "Pclass" , col = "#ff00ff50", labels = T, ylim = c(0,500))
hist(as.integer(hist_tmp$Pclass[hist_tmp$Survived == 1 & hist_tmp$Pclass == "2"]), breaks=2,
     xlab ="Pclass", ylab = "Survive", main = "Pclass" , col = "#00000050", labels = T, add = T)
hist(as.integer(hist_tmp$Pclass[hist_tmp$Survived == 0 & hist_tmp$Pclass == "3"]), breaks=2,
     xlab ="Pclass", ylab = "Survive", main = "Pclass" , col = "#ff00ff50", labels = T, ylim = c(0,500))
hist(as.integer(hist_tmp$Pclass[hist_tmp$Survived == 1 & hist_tmp$Pclass == "3"]), breaks=2,
     xlab ="Pclass", ylab = "Survive", main = "Pclass" , col = "#00000050", labels = T, add = T)
# ->>>>>> uum
# ->>>>>> Attention Pclass
# ->>>>>> Attention Pclass/Age
hist_tmp$Age <- as.integer(hist_tmp$Age)
hist_tmp$Age[0  <= hist_tmp$Age & hist_tmp$Age < 5] <- 0
hist_tmp$Age[5  <= hist_tmp$Age & hist_tmp$Age < 10] <- 5
hist_tmp$Age[10 <= hist_tmp$Age & hist_tmp$Age < 15] <- 10
hist_tmp$Age[15 <= hist_tmp$Age & hist_tmp$Age < 20] <- 15
hist_tmp$Age[20 <= hist_tmp$Age & hist_tmp$Age < 25] <- 20
hist_tmp$Age[25 <= hist_tmp$Age & hist_tmp$Age < 30] <- 25
hist_tmp$Age[30 <= hist_tmp$Age & hist_tmp$Age < 35] <- 30
hist_tmp$Age[35 <= hist_tmp$Age & hist_tmp$Age < 40] <- 35
hist_tmp$Age[40 <= hist_tmp$Age & hist_tmp$Age < 45] <- 40
hist_tmp$Age[45 <= hist_tmp$Age & hist_tmp$Age < 50] <- 45
hist_tmp$Age[50 <= hist_tmp$Age & hist_tmp$Age < 100] <- 50
table(hist_tmp$Survived,hist_tmp$Age)
hist(as.integer(hist_tmp$Age[hist_tmp$Survived == 0]), breaks=0:10*5,
     xlab ="Age", ylab = "Survive", main = "Age" , col = "#ff00ff50", labels = T, ylim = c(0,200))
hist(as.integer(hist_tmp$Age[hist_tmp$Survived == 1]), breaks=0:10*5,
     xlab ="Age", ylab = "Survive", main = "Age" , col = "#00000050", labels = T, add = T)

hist(as.integer(hist_tmp$Age[hist_tmp$Survived == 0 & hist_tmp$Pclass == 1]), breaks = 0:10*5,
     xlab ="Pclass1", ylab = "Survive", main = "Pclass1" , col = "#ff00ff50", labels = T, ylim = c(0,50))
hist(as.integer(hist_tmp$Age[hist_tmp$Survived == 1 & hist_tmp$Pclass == 1]), breaks = 0:10*5,
     xlab ="Pclass1", ylab = "Survive", main = "Pclass1" , col = "#00000050", labels = T, add = T)
hist(as.integer(hist_tmp$Age[hist_tmp$Survived == 0 & hist_tmp$Pclass == 2]), breaks = 0:10*5,
     xlab ="Pclass2", ylab = "Survive", main = "Pclass2" , col = "#ff00ff50", labels = T, ylim = c(0,50))
hist(as.integer(hist_tmp$Age[hist_tmp$Survived == 1 & hist_tmp$Pclass == 2]), breaks = 0:10*5,
     xlab ="Pclass2", ylab = "Survive", main = "Pclass2" , col = "#00000050", labels = T, add = T)
hist(as.integer(hist_tmp$Age[hist_tmp$Survived == 0 & hist_tmp$Pclass == 3]), breaks = 0:10*5,
     xlab ="Pclass3", ylab = "Survive", main = "Pclass3" , col = "#ff00ff50", labels = T, ylim = c(0,250))
hist(as.integer(hist_tmp$Age[hist_tmp$Survived == 1 & hist_tmp$Pclass == 3]), breaks = 0:10*5,
     xlab ="Pclass3", ylab = "Survive", main = "Pclass3" , col = "#00000050", labels = T, add = T)
# ->>>>>> Attention Pclass/Age

# Data Processing (Add Title)
train$Title <- gsub('(.*, )|(\\..*)', '', train$Name)
test$Title  <- gsub('(.*, )|(\\..*)', '', test$Name)
train$Miss[train$Title == "Miss"] <- 1; train$Miss[is.na(train$Miss)] <- 0
train$Mr[train$Title == "Mr"]     <- 1; train$Mr[is.na(train$Mr)]     <- 0
train$Mrs[train$Title == "Mrs"]   <- 1; train$Mrs[is.na(train$Mrs)]   <- 0
test$Miss[test$Title == "Miss"] <- 1; test$Miss[is.na(test$Miss)] <- 0
test$Mr[test$Title == "Mr"]     <- 1; test$Mr[is.na(test$Mr)]     <- 0
test$Mrs[test$Title == "Mrs"]   <- 1; test$Mrs[is.na(test$Mrs)]   <- 0
train$Embarked_S[train$Embarked == "S"] <- 1; train$Embarked_S[is.na(train$Embarked_S)] <- 0
test$Embarked_S[test$Embarked == "S"] <- 1; test$Embarked_S[is.na(test$Embarked_S)] <- 0
train$SibSpAndParch <- train$SibSp + train$Parch
test$SibSpAndParch <- test$SibSp + test$Parch
train$CabinA <- gsub('(A.*)', '0', train$Cabin)
train$CabinB <- gsub('(B.*)', '1', train$CabinA)
train$CabinC <- gsub('(C.*)', '2', train$CabinB)
train$CabinD <- gsub('(D.*)', '3', train$CabinC)
train$CabinE <- gsub('(E.*)', '4', train$CabinD)
train$CabinF <- gsub('(F.*)', '5', train$CabinE)
train$Cabin2 <- gsub('([G-Z].*)', '6', train$CabinF)
train$Cabin2[train$Cabin2 == ""] <- 0
train <- train[,-which (colnames(train) %in% 
                          c("CabinA","CabinB","CabinC","CabinD","CabinE","CabinF","CabinG"))]
test$CabinA <- gsub('(A.*)', '0', test$Cabin)
test$CabinB <- gsub('(B.*)', '1', test$CabinA)
test$CabinC <- gsub('(C.*)', '2', test$CabinB)
test$CabinD <- gsub('(D.*)', '3', test$CabinC)
test$CabinE <- gsub('(E.*)', '4', test$CabinD)
test$CabinF <- gsub('(F.*)', '5', test$CabinE)
test$Cabin2 <- gsub('([G-Z].*)', '6', test$CabinF)
test$Cabin2[test$Cabin2 == ""] <- 0
test <- test[,-which (colnames(test) %in% 
                          c("CabinA","CabinB","CabinC","CabinD","CabinE","CabinF","CabinG"))]

# Fare/Age log scale
train$Age <- as.integer(train$Age)
test$Age  <- as.integer(test$Age)
train$Age[0  <= train$Age & train$Age < 5] <- 0
train$Age[5  <= train$Age & train$Age < 10] <- 5
train$Age[10 <= train$Age & train$Age < 15] <- 10
train$Age[15 <= train$Age & train$Age < 20] <- 15
train$Age[20 <= train$Age & train$Age < 25] <- 20
train$Age[25 <= train$Age & train$Age < 30] <- 25
train$Age[30 <= train$Age & train$Age < 35] <- 30
train$Age[35 <= train$Age & train$Age < 40] <- 35
train$Age[40 <= train$Age & train$Age < 45] <- 40
train$Age[45 <= train$Age & train$Age < 50] <- 45
train$Age[50 <= train$Age & train$Age < 100] <- 50
test$Age[0  <= test$Age & test$Age < 5] <- 0
test$Age[5  <= test$Age & test$Age < 10] <- 5
test$Age[10 <= test$Age & test$Age < 15] <- 10
test$Age[15 <= test$Age & test$Age < 20] <- 15
test$Age[20 <= test$Age & test$Age < 25] <- 20
test$Age[25 <= test$Age & test$Age < 30] <- 25
test$Age[30 <= test$Age & test$Age < 35] <- 30
test$Age[35 <= test$Age & test$Age < 40] <- 35
test$Age[40 <= test$Age & test$Age < 45] <- 40
test$Age[45 <= test$Age & test$Age < 50] <- 45
test$Age[50 <= test$Age & test$Age < 100] <- 50
train$Fare <- as.integer(train$Fare)
test$Fare  <- as.integer(test$Fare)
train$Fare <- log(train$Fare+1) # +1 is not -Inf
test$Fare  <- log(test$Fare+1)  # +1 is not -Inf

for(i in 0:9){
  for(j in 0:9){
    for(z in 0:9){
      for(l in 0:9){
      target <- paste(c(as.character(i), as.character(j), as.character(z),as.character(l)), collapse = "")
      label  <- target
      target <- paste(c(as.character(target), ".*"), collapse = "")
      train$Tiket2[regexpr(target, train$Ticket) != -1] <- label
      test$Tiket2[regexpr(target, test$Ticket) != -1] <- label
      }
    }
  }
}
train$Tiket2[is.na(train$Tiket2)] <- 0
test$Tiket2[is.na(test$Tiket2)] <- 0

# Delete Data (PassengerId,Name)
train <- train[,-which (colnames(train) %in% 
                          c("Name","Title","Cabin","Ticket","Embarked","PassengerId","SibSp","Parch","Title"
                            ,"Mrs","Miss","Embarked_S","Cabin2","Pclass"))]
test  <- test[,-which  (colnames(test)  %in% 
                          c("Name","Title","Cabin","Ticket","Embarked","PassengerId","SibSp","Parch","Title"
                            ,"Mrs","Miss","Embarked_S","Cabin2","Pclass"))]
train[,"Survived"] <- as.factor(train[,"Survived"])
test[,"Survived"]  <- as.factor(test[,"Survived"])
train[,"Sex"] <- as.factor(train[,"Sex"])
test[,"Sex"]  <- as.factor(test[,"Sex"])
train$Age <- as.factor(train$Age)
test$Age <- as.factor(test$Age)

num <- sample(nrow(train), nrow(train)*0.7, replace = F)
train1 <- train[num,]
train2 <- train[-num,]

# randomforest tuneRF
tune <- tuneRF(train1[,-1], train1[,1], doBest=T, ntreeTry = 1000, stepFactor = T)
varImpPlot(tune)
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

# all train
tune <- tuneRF(train[,-1], train[,1], doBest=T, ntreeTry = 1000, stepFactor = T)
varImpPlot(tune)
print("[tune-test] Confusion Matrix:")
print(tune$confusion)
# test predict
pred.tune <- predict(tune, newdata=test, type='class')
prob.tune <- predict(tune, newdata=test, type='prob')
# ROC and AUC
pred <- prediction(predictions = prob.tune[,2], labels = test[,1])  #確率値、正解ラベル
perf <- performance(pred, "tpr", "fpr")
plot(perf)
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
print("[tune-test]AUC")
print(auc)

# Kaggle Data create
pred.tune <- predict(tune, newdata=test, type='class')
pred_data <- cbind(gender[,1],pred.tune)
pred_data[,2] <- pred_data[,2]-1
colnames(pred_data) <- c("PassengerId", "Survived")
write.table(pred_data, "./pred_data.txt", quote=F, row.names = F, col.names=T, append=F,sep = ",")
