tuneModelWrapped <- function(modelInput, trControl,
                             tuneGrid, preProcess = NULL, algo = 'xgboost', seed, featuresToInclude = 'all') {
  loginfo('Start tuning xgBoost model')
  if(algo == 'xgboost') {
    #if(!is.null(preProcess) & preProcess == 'log') {
    #  
    #}
    tuneResult <- tuneXGBoost(data = modelInput, trControl = trControl, tuneGrid = tuneGrid, 
                              preProcess = preProcess, seed = seed, featuresToInclude = featuresToInclude)
  } else {
    stop()
  }
  loginfo('End tuning xgBoost model')
  return(tuneResult)
}

tuneXGBoost <- function(data, trControl, tuneGrid, preProcess = NULL, seed,
                        probRatioToBet = 1.1, stake = 1, featuresToInclude) {
  #### Splitting Data ####
  # CV
  set.seed(seed)
  folds <- createFolds(data$matchResult, k = trControl$number,
                       list = FALSE)
  
  configResults <- data.frame()
  # Iterate Over Configurations
  for(confIndex in 1:nrow(tuneGrid)) {
    actGrid <- tuneGrid[confIndex, ]
    loginfo(paste(paste(names(actGrid), actGrid, sep = ' = '), collapse = ' | '))
    
    allResults <- data.frame()
    # Iterate Over Folds
    for(index in 1:trControl$number) {
      testInd <- folds == index
      trainInd <- folds != index
      test <- data[testInd, ]
      train <- data[trainInd, ]
      
      # Select Model Input
      features <- selectFeatures(train, featuresToInclude = featuresToInclude)
      labels <- as.numeric(features$matchResult) - 1
      xgbData <- xgb.DMatrix(data.matrix(features[, -1]), label=labels)
      #### Fit fold model ####
      set.seed(seed)
      xgbModel <- xgboost(data = xgbData, 
                       eta = actGrid$eta, 
                       gamma = actGrid$gamma, 
                       max_depth = actGrid$max_depth, 
                       min_child_weight = actGrid$min_child_weight, 
                       nrounds = actGrid$nrounds,
                       colsample_bytree = actGrid$colsample_bytree,
                       subsample = actGrid$subsample,
                       scale_pos_weight = actGrid$scale_pos_weight,
                       lambda = actGrid$lambda,
                       alpha = actGrid$alpha,
                       # Other
                       eval_metric = "merror",
                       objective = "multi:softprob",
                       num_class = 3,
                       silent = 1,
                       verbose = FALSE)
      
      # Predict Testset
      testFeatures <- selectFeatures(test, featuresToInclude)
      pred <- predict(xgbModel, data.matrix(testFeatures[, -1]))
      probs <- matrix(pred, ncol=3, byrow = T)
      
      predictFrame <- data.frame(probs)
      colnames(predictFrame) <- levels(features$matchResult)
      foldResult <- cbind('matchId' = test$matchId, 'matchResult' = test$matchResult, 'VisitorsOdd' = test$VisitorsOdd, 
                  'DrawOdd' = test$DrawOdd, 'HomeOdd' = test$HomeOdd, 
                  select(predictFrame, VisitorsPred = VisitorsVictory,
                         DrawPred = Draw, HomePred = HomeVictory))
      # merge data
      if(nrow(allResults) == 0) {
        allResults <- foldResult
      } else {
        allResults <- rbind(allResults, foldResult)
      }
      
    }
    
    #### Training Performance ####
    allFeatures <- selectFeatures(data, featuresToInclude)
    allLabels <- as.numeric(allFeatures$matchResult) - 1
    allXgbData <- xgb.DMatrix(data.matrix(allFeatures[, -1]), label=allLabels)
    # Fit model
    set.seed(seed)
    allXgbModel <- xgboost(data = allXgbData, 
                        eta = actGrid$eta, 
                        gamma = actGrid$gamma, 
                        max_depth = actGrid$max_depth, 
                        min_child_weight = actGrid$min_child_weight, 
                        nrounds = actGrid$nrounds,
                        colsample_bytree = actGrid$colsample_bytree,
                        subsample = actGrid$subsample,
                        scale_pos_weight = actGrid$scale_pos_weight,
                        lambda = actGrid$lambda,
                        alpha = actGrid$alpha,
                        # Other
                        eval_metric = "merror",
                        objective = "multi:softprob",
                        num_class = 3,
                        silent = 1,
                        verbose = FALSE)
    
    # Predict
    allPred <- predict(allXgbModel, data.matrix(allFeatures[, -1]))
    allProbs <- matrix(allPred, ncol=3, byrow = T)
    
    allPredictFrame <- data.frame(allProbs)
    colnames(allPredictFrame) <- levels(allFeatures$matchResult)
    allTrain <- cbind('matchId' = data$matchId, 'matchResult' = data$matchResult,
                        'VisitorsOdd' = data$VisitorsOdd, 
                        'DrawOdd' = data$DrawOdd, 'HomeOdd' = data$HomeOdd, 
                        select(allPredictFrame, VisitorsPred = VisitorsVictory,
                               DrawPred = Draw, HomePred = HomeVictory))
    # Train Bet Metrics
    trainMetrics <- betMetricsSummary(allTrain, probRatioToBet = probRatioToBet, stake = stake)
    loginfo('Training Performance:')
    loginfo(paste(paste(colnames(trainMetrics), round(trainMetrics, 3), sep = ' = '), collapse = ' | '))
    
    #### CV Performance ####
    testMetrics <- betMetricsSummary(allResults, probRatioToBet = probRatioToBet, stake = stake)
    loginfo('CV Performance:')
    loginfo(paste(paste(colnames(testMetrics), round(testMetrics, 3), sep = ' = '), collapse = ' | '))
    
    # Collect Metrics
    configResult <- cbind(select(trainMetrics, trainProfit = profitPerc, trainValueDiff = valueDiffPerc, 
                           trainAccuracy = accuracy, trainKappa = kappa),
                    testMetrics, actGrid)
    # merge data
    if(nrow(configResults) == 0) {
      configResults <- configResult
    } else {
      configResults <- rbind(configResults, configResult)
    }
    
  }
  
  return(configResults)
}

selectFeatures <- function(featuredMatches, featuresToInclude) {
  if(featuresToInclude == 'all') {
    modelInput <- select(featuredMatches, matchResult,
                         grep('Price', colnames(featuredMatches)),
                         grep('Form', colnames(featuredMatches)),
                         -heimFormation, -auswFormation)
  } else if(featuresToInclude == 'form') {
    modelInput <- select(featuredMatches, matchResult,
                         grep('Form', colnames(featuredMatches)),
                         -heimFormation, -auswFormation)
  } else if(featuresToInclude == 'price') {
    modelInput <- select(featuredMatches, matchResult,
                         grep('Price', colnames(featuredMatches)),
                         -heimFormation, -auswFormation)
  } else {
    stop()
  }
  
  
  return(modelInput)
}

reduceFeatures <- function(featuredMatches) {
  modelInput <- featuredMatches[, !grepl('_sum_', colnames(featuredMatches))]
  modelInput <- modelInput[, !grepl('_avg_Price_Bench', colnames(modelInput))]
  modelInput <- modelInput[, !grepl('_avg_Form_Bench', colnames(modelInput))]
  return(modelInput)
}

addPred <- function(probData) {
  enrichedData <- mutate(probData, pred = ifelse(HomeVictory >= VisitorsVictory & HomeVictory >= Draw, 'HomeVictory',
                                                 ifelse(VisitorsVictory > HomeVictory & VisitorsVictory > Draw, 'VisitorsVictory', 'Draw')))
  enrichedData <- mutate(enrichedData, pred = 
                           factor(pred, levels = c('VisitorsVictory', 'Draw', 'HomeVictory'), 
                                  labels = c('VisitorsVictory', 'Draw', 'HomeVictory'), 
                                  ordered = TRUE))
  return(enrichedData)
}

plotTuningProfile <- function(tuneResults) {
  xgbParas <- select(tuneResults, nrounds, eta, max_depth, gamma, colsample_bytree, min_child_weight,
                     lambda, alpha, subsample, scale_pos_weight)
  
  used <- apply(xgbParas, 2, FUN = function(x) {
    return(length(unique(x)) > 1)
  })
  
  usedParaName <- names(used[used])
  
  # cv bet metrics
  res <- gather(select(tuneResults, valueDiffPerc, profitPerc, matches(names(used[used]), ignore.case = TRUE)), 
                metric, value, valueDiffPerc, profitPerc)
  betMetricPlot <- ggplot(res, aes(x = res[, usedParaName],
                                   y = value,
                                   color = metric)) + geom_line() + xlab(usedParaName)
  
  # accuracy
  accRes <- gather(select(tuneResults, accuracy, trainAccuracy, bookyAccuracy, matches(names(used[used]), ignore.case = TRUE)), 
                   metric, value, accuracy, trainAccuracy, bookyAccuracy)
  accPlot <- ggplot(accRes, aes(x = accRes[, usedParaName],
                                y = value,
                                color = metric)) + geom_line() + xlab(usedParaName)
  
  return(grid.arrange(betMetricPlot, accPlot, ncol = 1))
}

