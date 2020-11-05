##
## Predictions for Fake News
##

## Libraries
library(tidyverse)
library(caret)

clean.train <- read_csv("./CleanTrain.csv")
clean.test <- read_csv("./CleanTest.csv")
clean.fakenews <- read_csv("./CleanFakeNews.csv")

sum(is.na(clean.train))
## replace the NAs with 0
clean.train[is.na(clean.train)] = 0
clean.train$isFake <- as.factor(clean.train$isFake)


### XGB TREE PREDICTIONS
gbmod <- train(form=as.factor(isFake)~.,
               data=clean.train %>% select(-Id),
               method="xgbTree",
               trControl=trainControl(method="cv",
                                      number=5),
               tuneGrid = expand.grid(nrounds=100, # Boosting Iterations
                                      max_depth=3, #Max Tree Depth
                                      eta=0.3, #(Shrinkage)
                                      gamma=1,
                                      colsample_bytree=1,# (Subsample Ratio of Columns)
                                      min_child_weight=1,# (Minimum Sum of Instance Weight)
                                      subsample=1)# (Subsample Percentage)0)
)


preds <- predict(gbmod, newdata=clean.test)
predframe <- data.frame(id=clean.test$Id, label=preds)


#predict
predictions <- data.frame(id=clean.test$id, label=(predict(rf_default, newdata=clean.test)))
write.csv(predframe,"/Users/graceedriggs/Documents/STAT 495/Fake-News/GD_XGB_Predictions.csv", row.names = FALSE)


