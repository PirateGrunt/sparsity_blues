library(rpart)
library(randomForest)
library(ggplot2)
library(ROCR)

load('data/all_arrests.rda')

# Generate indices to split data into 2 sets
num_rec <- nrow(dfPlayers)
set.seed(1234)
train <- base::sample(num_rec, size = 0.8 * num_rec)
test <- setdiff(seq.int(num_rec), train)
dfTrain <- dfPlayers[train, ]
dfTest <- dfPlayers[test, ]

# Very simple fit
fit <- rpart(
  formula = MultiArrest ~ Team_Division + Position + Day_of_Week
  , data = dfTrain
)
fit
summary(fit)
plot(fit)
text(fit, pretty = 1)

dfTrain$Pred1 <- predict(fit)
dfTest$Pred1 <- predict(fit, newdata = dfTest)

pred_train <- prediction(dfTrain$Pred1, dfTrain$MultiArrest)
roc_train = performance(pred_train, measure = "tpr", x.measure = "fpr")
plot(roc_train)
abline(a=0, b= 1)

pred_test <- prediction(dfTest$Pred1, dfTest$MultiArrest)
roc_test = performance(pred_test, measure = "tpr", x.measure = "fpr")
plot(roc_test)
abline(a=0, b= 1)

perf <- performance(prediction(dfTest$Pred1, dfTest$MultiArrest), 'auc')
as.numeric(perf@y.values)

perf <- performance(prediction(dfTrain$Pred1, dfTrain$MultiArrest), 'auc')
as.numeric(perf@y.values)

# More complicated fit
fit2 <- rpart(
    formula = MultiArrest ~ Team_city + Encounter + Position_name + ArrestSeasonState + Day_of_Week
  , data = dfTrain
)
dfTrain$Pred2 <- predict(fit2)
dfTest$Pred2 <- predict(fit2, newdata = dfTest)

pred_train <- prediction(dfTrain$Pred2, dfTrain$MultiArrest)
roc_train = performance(pred_train, measure = "tpr", x.measure = "fpr")
plot(roc_train)
abline(a=0, b= 1)
perf <- performance(prediction(dfTrain$Pred2, dfTrain$MultiArrest), 'auc')
as.numeric(perf@y.values)

pred_test <- prediction(dfTest$Pred2, dfTest$MultiArrest)
roc_test = performance(pred_test, measure = "tpr", x.measure = "fpr")
plot(roc_test)
abline(a=0, b= 1)
perf <- performance(prediction(dfTest$Pred2, dfTest$MultiArrest), 'auc')
as.numeric(perf@y.values)
