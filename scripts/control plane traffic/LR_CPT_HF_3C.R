loadRData <- function(fileName){ ### from https://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# TopologyZoo
metrics <- loadRData("../../data/r-files/hyperflow/metrics_HF_3C.Rda")

# LinkRemover
metrics_LR <- loadRData("../../data/r-files/hyperflow/metrics_HF_3C_LR.Rda") 

# Daten zuweisen
train <- data.frame(metrics_LR)
test <- data.frame(metrics)


lm <- lm(Control.Plane.Traffic ~ Betweenness + Links, data = train) # HyperFlow


pred <- predict(lm, test)
### see http://r-statistics.co/Linear-Regression.html
actuals_preds <- data.frame(cbind(actuals=test$Control.Plane.Traffic, predicteds=pred))

correlation_accuracy <- cor(actuals_preds, method="pearson")
min_max_accuracy <- mean(apply(abs(actuals_preds), 1, min) / apply(abs(actuals_preds), 1, max))
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals)/actuals_preds$actuals))

print(summary(lm))
print(paste0("correlation_accuracy: ",round(correlation_accuracy[[2]],digits=4)))
print(paste0("min_max_accuracy: ",round(min_max_accuracy,digits=4)))
print(paste0("mape: ",round(mape, digits=4)))