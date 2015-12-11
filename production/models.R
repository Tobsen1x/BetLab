createModelConfigEntry <- function(name, tGrid, meth, formula, preProc = NULL, ntree = NULL) {
    result <- list('modelName' = name, 'tuneGrid' = tGrid, 'method' = meth, 
                   'formula' = formula,
                   'preProcess' = preProc, 'ntree' = ntree)
    return(result)
}

initHomeVisitorConfig <- function(name, tGrid, meth, homeFormula, 
                                  visitorsFormula, preProc = NULL, ntree = NULL) {
    homeConf <- createModelConfigEntry(name, tGrid, meth, homeFormula, 
                                       preProc, ntree)
    visitorsConf <- createModelConfigEntry(name, tGrid, meth, visitorsFormula, 
                                           preProc, ntree)
    return(list(homeConf, visitorsConf))
}

createRfModelConfigs <- function(mtrys, ntrees, hFormula, vFormula) {
    configs <- list()
    # TEST
    #aktMtry <- 2
    #aktNtree <- 250
    #hFormula <- homeGoalFormula
    #vFormula <- visitorsGoalFormula
    for(aktMtry in mtrys) {
        for(aktNtree in ntrees) {
            aktGrid <- expand.grid(.mtry = aktMtry)
            hvConfigs <- initHomeVisitorConfig(NA, aktGrid, meth = 'rf', homeFormula = hFormula,
                                             visitorsFormula = vFormula, ntree = aktNtree)
            
            hvConfigs[[1]]$modelName <- paste('home_rf_', aktMtry, '_', aktNtree)
            hvConfigs[[2]]$modelName <- paste('visitors_rf_', aktMtry, '_', aktNtree)
            
            configs <- append(configs, hvConfigs)
        }
    }
    return(configs)
}

createKnnModelConfigs <- function(ks, hFormula, vFormula) {
    configs <- list()
    for(aktK in ks) {
        aktGrid <- expand.grid(.k = aktK)
        hvConfigs <- initHomeVisitorConfig(NA, aktGrid, meth = 'knn', homeFormula = hFormula,
                                           visitorsFormula = vFormula)
        hvConfigs[[1]]$modelName <- paste('home_knn_', aktK)
        hvConfigs[[2]]$modelName <- paste('visitors_knn_', aktK)
        
        configs <- append(configs, hvConfigs)
    }
    return(configs)
}

# Generic function for fitting
trainModel <- function(formula, train, meth, prePr = NULL,
                       tControl, seed, tGrid = NULL, ntree = NULL,
                       distr = 'multinomial', metr = 'Accuracy') {
    if(meth == 'rf') {
        set.seed(seed)
        model <- caret:::train(form = formula, data = train, method = meth,
                               preProcess = prePr, trControl = tControl, 
                               ntree = ntree, importance = TRUE, tuneGrid = tGrid,
                               metric = metr)
    } else if(meth == 'gbm') {
        set.seed(seed)
        model <- caret:::train(form = formula, data = train, method = meth,
                               preProcess = prePr, trControl = tControl, 
                               verbose = FALSE, tuneGrid = tGrid, distribution = distr,
                               metric = metr)
    } else {
        set.seed(seed)
        model <- caret:::train(form = formula, data = train, method = meth,
                               preProcess = prePr,
                               trControl = tControl, tuneGrid = tGrid,
                               metric = metr)
    }
    
    return(model)
}

# TEST
#modelConfigList <- allModelConfigs
#trainset <- filteredNormalMatches
#tContr <- noneContr
#aktConfig <- allModelConfigs[[1]]
trainModels <- function(modelConfigList, trainset, tContr, seed) {   
    models <- list()
    for(aktConfig in modelConfigList) {
        aktModel <- trainModel(aktConfig$formula, trainset, aktConfig$method, aktConfig$preProcess,
                               tControl = tContr, seed = seed, tGrid = aktConfig$tuneGrid,
                               ntree = aktConfig$ntree)
        aktList <- list(aktModel)
        names(aktList) <- aktConfig$modelName
        models <- append(models, aktList)
    }
    
    return(models)
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


# Split the matches into different folds. Each fold serving as testset.
# Remaining data is split into two different training sets.
splitMatches <- function(matchesToSplit, splitBy, trainingSetRest = NA, 
                         testingMatches, folds, seed = 1234) {
    require(caret)
    require(dplyr)
    
    if(is.na(trainingSetRest)) {
        trainingSetRest <- data.frame()
    }
    
    # Split data
    set.seed(seed)
    testFold <- createFolds(splitBy, k = folds,
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