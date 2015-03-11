fitResultModel <- function(featuredMatches, seed = 1) {
    require(caret)
    
    # Split data
    set.seed(seed)
    testIndex <- createDataPartition(featuredMatches$goalDiff, p = 0.2,
                                     list = FALSE,
                                     times = 1)
    test <- modelData[ testIndex, ]
    train <- modelData[ -testIndex, ]
    set.seed(seed)
    goalDiffTrainIndex <- createDataPartition(train$goalDiff, p = 0.5,
                                              list = FALSE,
                                              times = 1)
    goalDiffTrain <- train[ goalDiffTrainIndex, ]
    resultTrain <- train[ -goalDiffTrainIndex, ]
    
    repCVControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
    
    set.seed(seed)
    lmGoalDiffFit <- train(goalDiff ~ priceDiff + logPriceRate + formDiff + expGoalDiff, method = 'lm',
                           data = goalDiffTrain, trControl = repCVControl)
    lmGoalDiffFit
    summary(lmGoalDiffFit)
    
    
    
    
}