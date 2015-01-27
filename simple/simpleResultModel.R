fitSimpleResultModel <- function(simpleMatchFeatures, seed = 1) {
    # Filter all observations with one or more NAs in feature variables
    ergModelData <- simpleMatchFeatures[apply(
        simpleMatchFeatures[, c('homePrice', 'homeForm', 'visitorsPrice', 'visitorsForm')],
        1,function(x)!any(is.na(x))), ]
    ergModelData$priceDiff <- ergModelData$homePrice - ergModelData$visitorsPrice
    ergModelData$logPriceFraction <- log(ergModelData$homePrice / ergModelData$visitorsPrice)
    ergModelData$formDiff <- ergModelData$homeForm - ergModelData$visitorsForm
    
    require(caret)
    
    repCVControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 3, allowParallel = TRUE)
    cvControl <- trainControl(method = 'cv', number = 5, allowParallel = TRUE)
    
    # Split in two datasets. One for the preceding goaldiff model 
    # and one for the combined model, which calculates
    # the probabilities of the match outcomes
    set.seed(1)
    goalDiffTrainIndex <- createDataPartition(ergModelData$goalDiff, p = 1/3,
                                     list = FALSE,
                                     times = 1)
    goalDiffTrainset <- ergModelData[ goalDiffTrainIndex,]
    tmpset <- ergModelData[ -goalDiffTrainIndex,]
    
    matchResultTrainIndex <- createDataPartition(tmpset$goalDiff, p = 0.5,
                                              list = FALSE,
                                              times = 1)
    matchResultTrainset <- tmpset[matchResultTrainIndex,]
    
    evaluationset <- tmpset[ -matchResultTrainIndex,]
    
    ### Predicting goal diff ###
    
    # Linear Model
    lmFit <- train(goalDiff ~ priceDiff + logPriceFraction + formDiff, method = 'lm',
                   data = goalDiffTrainset, trControl = repCVControl)
    # Random Forest
    rfFit <- train(goalDiff ~ priceDiff + logPriceFraction + formDiff, method = 'rf',
                   data = goalDiffTrainset, trControl = cvControl, verbose = FALSE)
    
    nnetFit <- train(goalDiff ~ priceDiff + logPriceFraction + formDiff, method = 'nnet',
                     data = goalDiffTrainset, trControl = cvControl, verbose = FALSE, trace = FALSE)
    
    # Preparation for combined model
    allModels <- list(lm = lmFit, rf = rfFit, nnet = nnetFit)
    
    #predict on matchResultDataset
    predictedGoalDiffs <- as.data.frame(predict(allModels, newdata = matchResultTrainset))
    
    nzv <- nearZeroVar(predictedGoalDiffs, saveMetrics = TRUE)
    print(nzv)
    featureCor <- cor(predictedGoalDiffs)
    print(featureCor)
    highlyCorFeatures <- findCorrelation(featureCor, cutoff = .9)
    print(highlyCorFeatures)
    
    predictedGoalDiffs$matchResult <- matchResultTrainset$matchResult
    
    ######### Combined Model ###########
    rfCombFit <- train(matchResult ~ ., method = 'rf', data = predictedGoalDiffs, 
                       trControl = cvControl)
    print(rfCombFit)
    
    ldaCombFit <- train(matchResult ~ ., method = 'lda', data = predictedGoalDiffs, 
                        trControl = cvControl, verbose = FALSE)
    print(ldaCombFit)
    
    # Ordered Logistic Regression
    polrCombFit <- train(matchResult ~ ., method = 'polr', data = predictedGoalDiffs, 
                         trControl = cvControl)
    print(polrCombFit)
    
    ######### Evaluate Testset #############
    allCombModels <- list(polr = polrCombFit)
    evaluateData <- evaluationset
    for(mod in allCombModels) {
        #print(mod)
        evaluateModel <- mod
        evaluationGoalDiffPredictions <- as.data.frame(predict(allModels, newdata = evaluateData))
        evaluationGoalDiffPredictions$matchResult <- evaluateData$matchResult
        evaluationResults <- predict(evaluateModel, newdata = evaluationGoalDiffPredictions, type = 'prob')
        evaluationResults$matchId <- evaluateData$matchId
        evaluationResults$matchResult <- evaluateData$matchResult
        
        eval <- evaluatePrediction(prediction = evaluationResults)
        printEvaluation(eval)
    }
    
    # Predict over all observations
    print('All Matches:')
    evaluationGoalDiffPredictions <- as.data.frame(predict(allModels, newdata = ergModelData))
    evaluationGoalDiffPredictions$matchResult <- ergModelData$matchResult
    evaluationGoalDiffPredictions$matchId <- ergModelData$matchId
    evaluationResults <- predict(polrCombFit, newdata = evaluationGoalDiffPredictions, type = 'prob')
    evaluationResults$matchId <- ergModelData$matchId
    evaluationResults$matchResult <- ergModelData$matchResult
    
    relevantPredictions <- evaluatePrediction(prediction = evaluationResults)
    printEvaluation(relevantPredictions)
    
    # Save for Gui Input
    # save(evaluationResults, file = 'C:/BetLab/SoccerLab/RegInput/predictionsForGui.RData')
}