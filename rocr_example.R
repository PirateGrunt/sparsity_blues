library(ROCR)

dfToy <- data.frame(
    Prediction = c(0.9, 0.1, 0.1, 0.9, 0.9)
  , Observation = c(FALSE, FALSE, FALSE, TRUE, TRUE)
)

pred <- prediction(dfToy$Prediction, dfToy$Observation)

roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)

perf <- performance(pred, measure = 'auc')
perf
perf@y.values
