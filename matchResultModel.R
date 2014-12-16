calcErgModelFit <- function(modelMatches) {
    ergModelData <- modelMatches[apply(
        modelMatches[, 12:23],1,function(x)!any(is.na(x))), c(1, 10:23)]
    
    corMatrix <- cor(ergModelData[, -c(1, 2)])
    print('Correlation Matrix:')
    print(corMatrix)
    
    require(caret)
    highlyCorrelated <- findCorrelation(corMatrix, cutoff=0.8)
    print('Predictors which are highly correlated (> 0.8):')
    print(highlyCorrelated)
    
    # At least one of priceDiff and logPriceFraction should be removed
    repCVControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 3, 
                                 allowParallel = TRUE)
    cvControl <- trainControl(method = 'cv', number = 10, allowParallel = TRUE)
    
    # Split in training and testing data set
    set.seed(1)
    trainIndex <- createDataPartition(ergModelData$goalDiff, p = .8,
                                      list = FALSE,
                                      times = 1)
    train <- ergModelData[ trainIndex,]
    test  <- ergModelData[-trainIndex,]
    
    set.seed(2)
    
    ### lm - Fit ###
    lmFit <- train(goalDiff ~ ., data = train[, c(-1, -2)], method = 'lm', 
                   trControl = repCVControl,
                   #preProcess = c('center', 'scale'), 
                   )
    lmFit
    
    lmImportance <- varImp(lmFit, scale = FALSE)
    print(lmImportance)
    
    ### RF - Fit ###
    rfFit <- train(goalDiff ~ ., data = train[, c(-1, -2)], method = 'rf', 
                   trControl = cvControl, importance = TRUE)
    rfFit
    
    rfImportance <- varImp(rfFit, scale = FALSE)
    print(rfImportance)
    
    ### gbm - Fit ###
    gbmGrid <- expand.grid(.interaction.depth = (1:5) * 2,
                           .n.trees = (1:10)*25, .shrinkage = .1)
    gbmFit <- train(goalDiff ~ ., data = train[, c(-1, -2)], method = 'gbm',
                    verbose = FALSE, trControl = cvControl) #, bag.fraction = 0.5)
    gbmFit
    
    ### Combined Model ###
    models <- list(lm = lmFit, rf = rfFit, gbm = gbmFit)
    trainPred <- predict(models, newdata = train)

    
    combInput <- cbind(train[, c('matchId', 'matchResult')], 
                       lm = trainPred$lm, rf = trainPred$rf, gbm = trainPred$gbm)
    
    combFit <- train(matchResult ~ ., data = combInput[, -c(1)], method = 'rf',
                     trControl = cvControl, importance = TRUE)
    combFit
    
    combImportance <- varImp(combFit, scale = FALSE)
    print(combImportance)
    
    ### Getting Predictions on train set ###
    
    trainMatchResult <- predict(combFit, combInput)
    trainResultProb <- predict(combFit, combInput, type = 'prob')
    trainResultProb
    trainResult <- cbind(train[, c('matchId', 'matchResult', 'goalDiff')],
                         predResult = trainMatchResult, 
                         trainResultProb)
    trainResult
    
    ### Getting Predictions on testset ###
    testCombFeaturesInput <- predict(models, newdata = test)
    testCombInput <- cbind(test[, c('matchId', 'matchResult')], 
                           lm = testCombFeaturesInput$lm, rf = testCombFeaturesInput$rf, 
                           gbm = testCombFeaturesInput$gbm)
    
    testCombPredictions <- predict(combFit, newdata = testCombInput)
    testProbPredictions <- predict(combFit, newdata = testCombInput, type = 'prob')
    
    testResult <- cbind(test[, c('matchId', 'matchResult', 'goalDiff')], 
          predResult = testCombPredictions, testProbPredictions)
    testResult
    
    # Getting booky rate
    bookyProbs <- getBookyProbs(test)
    
    qualifyPrediction(prediction = testResult, bookyProbs)
    
}

getBookyProbs <- function(data) {
    homeProb <- nrow(data[data$matchResult == 'HomeVictory', ]) / nrow(data)
    visitorsProb <- nrow(data[data$matchResult == 'VisitorsVictory', ]) / nrow(data)
    drawProb <- nrow(data[data$matchResult == 'Draw', ]) / nrow(data)
    bookyProbs <- data.frame(matchId = data$matchId, HomeVictory = rep(homeProb, times = nrow(data)),
                             VisitorsVictory = rep(visitorsProb, times = nrow(data)),
                             Draw = rep(drawProb, times = nrow(data)))
    bookyProbs
}



