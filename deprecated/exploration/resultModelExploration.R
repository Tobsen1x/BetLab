load('C:/Users/Tobsen1X/RStudioWorkspace/BetLab/data/basisData.RData')

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/positionFeatureExtraction.R', echo = FALSE, encoding = 'UTF-8')

positions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 'mid', 'off', 'off', 'off', 'off', 'off')
featuredMatches <- extractMatchResultFeatures(playerStats = data$stats,
                                              matches = data$matches,
                                              priceAssignedPositions = positions,
                                              functs = c('min', 'max', 'avg', 'sum'))

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/resultModel.R', echo = FALSE, encoding = 'UTF-8')
featuredMatches <- filterFeaturedMatches(featuredMatches)

## Preparation

seed <- 1234
trainControl <- trainControl(method = 'cv', number = 5)
featuredMatches <- select(featuredMatches, -tw_Price_Home_min, -tw_Price_Home_max,
                          -tw_Price_Home_sum, -tw_Price_Visitors_min, -tw_Price_Visitors_max,
                          -tw_Price_Visitors_sum)
x <- featuredMatches[, 4:29]
logX <- log(x + 1)
y <- featuredMatches$matchResult

set.seed(seed)
polrFit <- train(x = x, y = y, method = 'polr', 
                 trControl = trainControl)
head(predict(polrFit, x, type = 'prob'))

data <- cbind(y, x)
formula <- as.formula('y ~ .')
vglmFit <- vglm(formula, family=cumulative(parallel=TRUE),
                data = data)
summary(vglmFit)

vglmAdjaFit <- vglm(formula, family=acat(reverse=TRUE, parallel=TRUE),
                     data = data)
summary(vglmAdjaFit)

vglmRevFit <- vglm(formula, family=cratio(reverse=TRUE, parallel=TRUE),
                    data = data)

vglmProbitFit <- vglm(formula, family=cumulative(link = probit, parallel = TRUE),
                    data = data)

vglmCloglogFit <- vglm(formula, family=cumulative(link = cloglog, parallel = TRUE),
                    data = data)

vgamFit <- vgam(formula, family=cumulative(parallel=TRUE),
                data = data)
summary(vgamFit)

polrFit <- polr(formula, data, method = c('loglog'))
?polr.predict

preds <- predict(polrFit, x, type = 'p')
head(preds)


























load('C:/Users/Tobsen1X/RStudioWorkspace/BetLab/data/basisData.RData')

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/resultModel.R', echo = FALSE, encoding = 'UTF-8')
folds <- 10
seed <- 1234
relevantFeatureMatches <- filterFeaturedMatches(featuredMatches)

# Splitting data
testFolds <- createFolds(featuredMatches$goalDiff, k = folds,
                        list = FALSE)
relevantFeatureMatches <- cbind(relevantFeatureMatches, testFold)

allSets <- list()
for(i in 1:folds) {
    testset <- filter(relevantFeatureMatches, testFold == i)
    resultTrain <- filter(relevantFeatureMatches, testFold != i)
    
    splitData <- list(testset = testset, resultTrainset = resultTrain)
    allSets <- append(allSets, list(splitData))
}

resultFormula <- as.formula('matchResult ~ .')

allPredictionsList <- list()

for(i in 1:folds) {
    testset <- allSets[[i]]$testset
    resultTrain <- allSets[[i]]$resultTrainset

    resultModelInput <- cbind('matchResult' = resultTrain$matchResult, goalDiffPreds)
    
    resultModels <- fitResultModels(resultModelInput, resultFormula,
                                    seed = seed, trainControl = cv)
    
    testsetPredictions <- applyModels(goalDiffModels = goalDiffModels,
                                      resultModels = resultModels,
                                      data = testset)
    
    if(length(allPredictionsList) == 0) {
        allPredictionsList <- testsetPredictions
    } else {
        for(actName in names(allPredictionsList)) {
            allPredictionsList[[actName]] <- rbind(allPredictionsList[[actName]], 
                                                   testsetPredictions[[actName]])
        }
    }
}
