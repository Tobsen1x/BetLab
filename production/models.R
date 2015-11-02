# Generic function for fitting
trainModel <- function(resultFormula, train, meth, prePr = NULL,
                       tControl, seed, tGrid = NULL) {
    if(meth == 'rf') {
        set.seed(seed)
        model <- caret:::train(form = resultFormula, data = train, method = meth,
                               preProcess = prePr, trControl = tControl, 
                               ntree = 1000, importance = TRUE, tuneGrid = tGrid)
    } else {
        set.seed(seed)
        model <- caret:::train(form = resultFormula, data = train, method = meth,
                               preProcess = prePr,
                               trControl = tControl)
    }
    
    return(model)
}

iterativelyPredict <- function(splits, folds, resultFormula, meth, 
                               prePr = NULL, tControl, seed, tGrid = NULL) {
    allPredictions <- data.frame()
    for(i in 1:folds) {
        actTrain <- splits[[i]]$train
        actTest <- splits[[i]]$test
        
        
        model <- trainModel(resultFormula, train = actTrain, meth = meth,
                            prePr = prePr,
                            tControl = tControl,
                            seed = seed, tGrid = tGrid)
        
        preds <- predict(model, actTest, type = 'prob')
        preds <- cbind('matchId' = actTest$matchId,
                       'matchResult' = actTest$matchResult,
                       preds)
        
        if(nrow(allPredictions) == 0) {
            allPredictions <- preds
        } else {
            allPredictions <- rbind(allPredictions, preds)
        }
    }
    
    return(allPredictions)
}


simulate <- function(normalMatches, predAufstellungMatches,
                     trainingConf, testingConf, folds, seed, resultFormula,
                     tcontr = trainControl(method = 'none')) {
    trainingSetRest <- data.frame()
    if('normal' %in% trainingConf) {
        matchesToSplit <- normalMatches
        if('predAufstellung' %in% trainingConf) {
            trainingSetRest <- predAufstellungMatches  
        }
    } else if('predAufstellung' %in% trainingConf) {
        matchesToSplit <- predAufstellungMatches
    } else {
        stop(' Wrong motherfuckaaa.')
    }
    
    if(testingConf == 'normal') {
        testingMatches <- normalMatches
    } else if(testingConf == 'predAufstellung') {
        testingMatches <- predAufstellungMatches
    } else {
        stop(' Warong!')
    }
    
    splits <- splitMatches(matchesToSplit, trainingSetRest, testingMatches, 
                           folds = folds, seed = seed)   
    allPredictions <- data.frame()
    
    for(i in 1:folds) {
        resultTrain <- splits[[i]]$train
        resultTest <- splits[[i]]$test
        
        resultModel <- fitModels(resultTrain, resultFormula,
                                        seed = seed, tcontr = tcontr)
        
        preds <- predict(resultModel, newdata = resultTest, type = 'prob')
        testsetPredictions <- cbind(matchId = resultTest$matchId, 
                                    matchResult = resultTest$matchResult, preds)
        
        if(nrow(allPredictions) == 0) {
            allPredictions <- testsetPredictions
        } else {
            allPredictions <- rbind(allPredictions, testsetPredictions)
        }
    }
    return(allPredictions)
}

fitRegressionModels <- function(trainset, resultFormula, modelNamePrefix,
                     tcontr = trainControl(method = 'none')) {  
    enetGrid <- expand.grid(.fraction = 1, .lambda = 0.0004)
    enetModel <- train(resultFormula, data = trainset, method = 'enet', preProcess = c('center', 'scale'),
                     trControl = tcontr, tuneGrid = enetGrid)
    
    rfGrid <- expand.grid(.mtry = c(2))
    rfModel <- train(resultFormula, data = trainset, method = 'rf', trControl = tcontr, tuneGrid = rfGrid)
    
    #bstTreeModel <- train(resultFormula, data = trainset, method = 'bstTree', trControl = tcontr)
    
    #gbmModel <- train(resultFormula, data = trainset, method = 'gbm', trControl = tcontr, 
    #                  verbose = FALSE)
    
    #knn5Grid <- expand.grid(.k = c(5))
    #knn5Model <- train(resultFormula, data = trainset, method = 'knn', 
    #                   trControl = tcontr, tuneGrid = knn5Grid, preProcess = c('center', 'scale'))
    
    knn20Grid <- expand.grid(.k = c(20))
    knn20Model <- train(resultFormula, data = trainset, method = 'knn', 
                        trControl = tcontr, tuneGrid = knn20Grid, preProcess = c('center', 'scale'))
    
    #knn50Grid <- expand.grid(.k = c(50))
    #knn50Model <- train(resultFormula, data = trainset, method = 'knn', 
    #                    trControl = tcontr, tuneGrid = knn50Grid, preProcess = c('center', 'scale'))
    
    #knn100Grid <- expand.grid(.k = c(100))
    #knn100Model <- train(resultFormula, data = trainset, method = 'knn', 
    #                     trControl = tcontr, tuneGrid = knn100Grid, preProcess = c('center', 'scale'))
    
    ### Choose relevant models
    modelNames <- c(paste(modelNamePrefix, 'enet', sep = '_'),
                    paste(modelNamePrefix, 'rf', sep = '_'),
                    paste(modelNamePrefix, 'knn20', sep = '_'))
    relModels <- list(enetModel, rfModel, knn20Model)
    names(relModels) <- modelNames
    
    return(relModels)
    
    
        ###     Simple polr model       ###
    #resultTrain <- dplyr:::select(featuredMatches, -c(matchId, goalDiff))
    #set.seed(seed)
    #install.packages('e1071')
    #library(e1071)
    #polrModel <- train(resultFormula, data = resultTrain, method = 'polr',
    #                   preProcess = c('center', 'scale'),
    #                   trControl = tcontr)
    #
    #rfTuneGrid <- expand.grid(.mtry = seq(2, 26, by = 1))
    #rfModel <- train(resultFormula, data = resultTrain, method = 'rf', tuneGrid = rfTuneGrid,
    #                 trControl = tcontr, ntrees = 1000)
    #modelList <- list('polr' = polrModel, 'rf' = rfModel)
    #return(modelList)
    #
    #return(polrModel)
}



# Split the matches into different folds. Each fold serving as testset.
# Remaining data is split into two different training sets.
splitMatches <- function(matchesToSplit, trainingSetRest = NA, 
                         testingMatches, folds, seed = 1234) {
    require(caret)
    require(dplyr)
    
    if(is.na(trainingSetRest)) {
        trainingSetRest <- data.frame()
    }
    
    # Split data
    set.seed(seed)
    testFold <- createFolds(matchesToSplit$matchResult, k = folds,
                            list = FALSE)
    matchesToSplit <- cbind(matchesToSplit, testFold)
    
    allSets <- list()
    for(i in 1:folds) {
        train <- filter(matchesToSplit, testFold != i)
        train <- dplyr:::select(train, -c(testFold))
        trainingMatchIds <- unique(train$matchId)
        
        # Konsolidating trainset and testset
        restMatches <- filter(trainingSetRest, matchId %in% trainingMatchIds)
        train <- rbind(train, restMatches)
        
        #for(j in 1:nrow(train)) {
        #    aktMatch <- train[j, ]
        #    restMatch <- filter(trainingSetRest, matchId == aktMatch$matchId)
        #    train <- rbind(train, restMatch)
        #}
        
        test <- filter(testingMatches, !(matchId %in% trainingMatchIds))
        
        splitData <- list('train' = train, 'test' = test)
        allSets <- append(allSets, list(splitData))
    }
    
    return(allSets)
}