rdsRoot <- 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/rds/'

baseDataFilename <- paste(rdsRoot, '2005-2015_1_20150829.rds', sep = '')
trainingData <- readRDS(baseDataFilename)
filteredNormalMatches <- readRDS(paste(rdsRoot, 'normalFeatured.rds', sep = ''))
filteredPredAufstellungMatches <- readRDS(paste(rdsRoot, 'predAufstellungFeatured.rds', sep = ''))


featuredMatches <- filteredNormalMatches
goalDiffInput <- dplyr:::select(featuredMatches, -c(matchId, matchResult))

### Construct Formulas
allFormula <- paste('goalDiff ~ ', paste(colnames(goalDiffInput[, -1]), '', sep = '', collapse=' + '), sep = '')
allFormula <- as.Formula(allFormula)

justDiffColumns <- colnames(select(goalDiffInput, grep(pattern = 'Diff', colnames(goalDiffInput))))
justDiffColumns <- justDiffColumns[! justDiffColumns %in% c('goalDiff')]
justDiffFormula <- paste('goalDiff ~ ', paste(justDiffColumns, '', sep = '', collapse=' + '), sep = '')
justDiffFormula <- as.Formula(justDiffFormula)

justRatioColumns <- colnames(select(goalDiffInput, grep(pattern = 'Ratio', colnames(goalDiffInput))))
justRatioColumns <- justRatioColumns[! justRatioColumns %in% c('goalDiff')]
paste(paste('log(', justRatioColumns, ')', sep = ''), '', sep = '', collapse = ' + ')
justRatioFormula <- paste('goalDiff ~ ', paste(justRatioColumns, '', sep = '', collapse=' + '), sep = '')
justRatioFormula <- as.Formula(justRatioFormula)

describe(select(goalDiffInput, grep(pattern = 'Ratio', colnames(goalDiffInput))))

library(caret)
cv5Contr <- trainControl(method = 'cv', number = 5)
cv2Contr <- trainControl(method = 'cv', number = 2)
noneContr <- trainControl(method = 'none')

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/production/models.R', 
       echo = FALSE, encoding = 'UTF-8')


allModels <- fitRegressionModels(trainset = featuredMatches, resultFormula = allFormula, 
                                 modelNamePrefix = 'all', tcontr = noneContr)
saveRDS(allModels, paste(rdsRoot, 'allModels.rds', sep = ''))

justDiffModels <- fitRegressionModels(trainset = featuredMatches, resultFormula = justDiffFormula, 
                                      modelNamePrefix = 'diff', tcontr = noneContr)
saveRDS(justDiffModels, paste(rdsRoot, 'justDiffModels.rds', sep = ''))

justRatioModels <- fitRegressionModels(trainset = featuredMatches, resultFormula = justRatioFormula, 
                                       modelNamePrefix = 'ratio', tcontr = noneContr)
saveRDS(justRatioModels, paste(rdsRoot, 'justRatioModels.rds', sep = ''))

allModels <- readRDS(paste(rdsRoot, 'allModels.rds', sep = ''))
justDiffModels <- readRDS(paste(rdsRoot, 'justDiffModels.rds', sep = ''))
justRatioModels <- readRDS(paste(rdsRoot, 'justRatioModels.rds', sep = ''))

allResamples <- resamples(allModels)
justDiffResamples <- resamples(justDiffModels)
justRatioResamples <- resamples(justRatioModels)
summary(allResamples)
summary(justDiffResamples)
summary(justRatioResamples)

predsList <- predict(justDiffModels, newdata = goalDiffInput)
preds <- as.data.frame(do.call(cbind, predsList))
preds <- cbind('goalDiff' = goalDiffInput$goalDiff, preds)
testCorMatrix <- cor(preds)
testCorMatrix

###     Testset Predictions Exploration         ###
folds <- 10
splits <- splitMatches(matchesToSplit = featuredMatches, 
                       testingMatches = featuredMatches, folds = folds)

allPredictions <- data.frame()
for(i in 1:folds) {
    goalDiffTrain <- splits[[i]]$train
    goalDiffTest <- splits[[i]]$test
    
    goalDiffModels <- fitModels(goalDiffTrain, goalDiffFormula,
                             tcontr = cvContr)
    
    predsList <- predict(goalDiffModels, newdata = goalDiffTest)
    preds <- as.data.frame(do.call(cbind, predsList))
    preds <- cbind(preds, 'matchId' = goalDiffTest$matchId,
                   'goalDiff' = goalDiffTest$goalDiff,
                   'matchResult' = goalDiffTest$matchResult)
    
    if(nrow(allPredictions) == 0) {
        allPredictions <- preds
    } else {
        allPredictions <- rbind(allPredictions, preds)
    }
}

testCorMatrix <- cor(select(allPredictions, -c(matchId, matchResult) ))
testCorMatrix


### Result Model exploration ###
repCvContr <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
resultFormula <- as.Formula('matchResult ~ lm + rf + gbm + knn5 + knn20 + knn100')
polrModel <- train(resultFormula, data = allPredictions, method = 'polr',
                                    # preProcess = c('center', 'scale'),
                                     trControl = repCvContr)
polrModel





### POLR Overfitting exploration ###
x_y <- dplyr:::select(featuredMatches, -c(matchId, goalDiff))
x <- dplyr:::select(x_y, -c(matchResult))
y <- x_y$matchResult

polrContr <- trainControl(method = 'cv', number = 5, classProbs = TRUE)
polrModel <- train(x, y, method = 'polr',
                   preProcess = c('center', 'scale'),
                   trControl = polrContr)
print(paste('Test Acc.:', polrModel$results$Accuracy))
preds <- predict(polrModel, x)
confMatrix <- confusionMatrix(data = preds, y)
print(paste('Train Acc.:', confMatrix$overall['Accuracy']))