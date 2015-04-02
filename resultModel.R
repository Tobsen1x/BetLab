fitResultModel <- function(goalDiffTrainset, resultTrainset, testset, seed) {
    require(caret)
    require(magrittr)
    
    repCVControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
    
    # Goal difference model
    set.seed(seed)
    lmGoalDiffFit <- train(goalDiff ~ priceDiff + logPriceRate + formMeanfDiff + 
                               expChancesDiff, method = 'lm',
                           data = goalDiffTrainset, trControl = repCVControl)
    
    resultModelInput <- resultTrainset %>% 
        mutate(goalDiffPred = predict(lmGoalDiffFit, resultTrainset))
    
    # Result model
    set.seed(seed)
    polrFit <- train(matchResult ~ goalDiffPred, method = 'polr', 
                     data = resultModelInput, trControl = repCVControl)
    
    # Application on testset
    testset <- testset %>%
        mutate(goalDiffPred = predict(lmGoalDiffFit, testset))
    testResult <- predict(polrFit, testset, type = 'prob')
    testResult$matchResult <- testset$matchResult
    testResult$matchId <- testset$matchId
    
    return(testResult)
}

# Split the matches into different folds. Each fold serving as testset.
# Remaining data is split into two different training sets.
splitMatches <- function(featuredMatches, folds, seed) {
    require(caret)
    require(dplyr)
    
    # Split data
    set.seed(seed)
    testFold <- createFolds(featuredMatches$goalDiff, k = folds,
                            list = FALSE)
    featuredMatches <- cbind(featuredMatches, testFold)
    
    allSets <- list()
    for(i in 1:folds) {
        testset <- filter(featuredMatches, testFold == i)
        others <- filter(featuredMatches, testFold != i)
        
        # Split remaining observations in two trainsets
        set.seed(seed)
        goalDiffTrainIndex <- createDataPartition(others$goalDiff, p = 0.5,
                                                  list = FALSE, times = 1)
        goalDiffTrain <- others[ goalDiffTrainIndex, ]
        resultTrain <- others[ -goalDiffTrainIndex, ]
        splitData <- list(testset = testset, goalDiffTrainset = goalDiffTrain,
                          resultTrainset =resultTrain)
        allSets <- append(allSets, list(splitData))
    }
    
    return(allSets)
}