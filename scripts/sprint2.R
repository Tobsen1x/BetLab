source('./rawData/loadData.R', 
       echo = FALSE, encoding = 'UTF-8')
toMatchday <- 34
seasons <- c('2005-2006', '2006-2007', '2007-2008', '2008-2009', '2009-2010',
             '2010-2011', '2011-2012', '2012-2013', '2013-2014', '2014-2015')
leagues <- c('BL1')
trainingRaw <- loadTrainingData(toMatchday = toMatchday, seasons = seasons, leagues = leagues)
matches <- trainingRaw$matches
odds <- trainingRaw$odds
stats <- trainingRaw$stats

describe(select(matches, goalsHome, goalsVisitors, matchResult))
describe(select(odds, HomeVictory, VisitorsVictory, Draw))
describe(select(stats, fitPrice, grade, position, playerAssignment, formation))

### Feature Engineering ###

source('./production/positionFeatureExtraction.R', 
       echo = FALSE, encoding = 'UTF-8')
### Preparation
#[1] "Torwart"               "Innenverteidiger"      "Linker Verteidiger"    "Rechter Verteidiger"   "Defensives Mittelfeld"
#[6] "Zentrales Mittelfeld"  "Linkes Mittelfeld"     "Rechtes Mittelfeld"    "Offensives Mittelfeld" "Haengende Spitze"     
#[11] "Mittelstuermer"        "Linksaussen"           "Rechtsaussen"
positions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 'mid', 'off', 'off', 'off', 'off', 'off')
lineupAssignments <- c('DURCHGESPIELT', 'AUSGEWECHSELT', 'EINGEWECHSELT')
#benchFuncts = c('max', 'avg')
featuredMatches <- extractMatchResultFeatures(playerStats = stats,
                                            matches = matches,
                                            priceAssignedPositions = positions,
                                            functs = c('min', 'max', 'avg'), #, 'sum'), 
                                            lineupAssignments)

filteredFeatureMatches <- filterFeaturedMatches(featuredMatches)

explMatches <- dplyr:::select(filteredFeatureMatches, -matchId, -matchResult, -goalsHome, - goalsVisitors)
# Features:
colnames(explMatches)

### Explore performance without sums
source(file = './production/models.R', 
       echo = FALSE, encoding = 'UTF-8')
seed <- 16450
customCvContr <- trainControl(method = 'cv', number = 5, classProbs = TRUE, 
                              summaryFunction = betMetricsSummary)

resultFormula <- as.formula('matchResult ~ . -matchId -goalsHome -goalsVisitors')

set.seed(seed)
polrModel <- train(form = resultFormula, data = filteredFeatureMatches, method = 'polr',
                   preProcess = c('center', 'scale'), trControl = customCvContr)
polrModel
confusionMatrix(polrModel)

# POLR Resampling exploration
vioplot(polrModel$resample$GainPerc, names = 'Gain [%]', col = 'green')
title('Violin Plot of Gain Percentage in resamples')
summary(polrModel$resample$GainPerc)

# Training Performance without resampling
testPred <- predict(polrModel, filteredFeatureMatches)
confMatrix <- confusionMatrix(testPred, reference = filteredFeatureMatches$matchResult)
confMatrix$overall[1:2]

### Extreme Gradient Boosting
extrBoostGrid <- expand.grid(nrounds = (1:10)*100,
                             eta = c(.05, .075, .1),
                             max_depth = 1:4)
set.seed(seed)
extrBoostModel <- train(form = resultFormula, data = filteredFeatureMatches, method = 'xgbTree',
                        trControl = customCvContr, tuneGrid = extrBoostGrid, metric = 'GainPerc',
                        objective = 'multi:softprob', num_class = 3,
                        colsample_bytree = 1, min_child_weight = 1)
trellis.par.set(caretTheme())
plot(extrBoostModel)

extrBoostModel$results[as.integer(rownames(extrBoostModel$results)) == as.integer(rownames(extrBoostModel$bestTune)), ]

# Non-resampled training performance
testPred <- predict(extrBoostModel, filteredFeatureMatches)
confMatrix <- confusionMatrix(testPred, reference = filteredFeatureMatches$matchResult)
confMatrix$overall[1:2]


# Model Fitting

seed <- 16450
cvContr = trainControl(method = 'cv', number = 5, classProbs = TRUE, 
                       summaryFunction = multiClassSummary)

resultFormula <- as.formula('matchResult ~ . -matchId -goalsHome -goalsVisitors')

#### Simple POLR model ####
set.seed(seed)
polrModel <- train(form = resultFormula, data = filteredFeatureMatches, method = 'polr',
                   preProcess = c('center', 'scale'),
                   trControl = cvContr)

# Resampled Training Performance
dplyr:::select(polrModel$results, Accuracy, Kappa, Sensitivity, Specificity, ROC, logLoss)
testPred <- predict(polrModel, filteredFeatureMatches)
confMatrix <- confusionMatrix(testPred, reference = filteredFeatureMatches$matchResult)
# Training Performance without resampling
confMatrix$overall[1:2]


### gbm ###
#library(devtools)
#install_github("harrysouthworth/gbm")
#library(gbm)
sourceDir <- function(path, trace = TRUE, ...) {
    for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
        if(trace) cat(nm,":")
        source(file.path(path, nm), ...)
        if(trace) cat("\n")
    }
}
sourceDir('../gbm/R/', trace = FALSE,
          echo = FALSE, encoding = 'UTF-8')

# in problems where there are a low percentage of samples in one class, 
# using metric = "Kappa" can improve quality of the final model
metricString <- 'Accuracy'

gbmGrid <- expand.grid(.interaction.depth = c(1, 5, 9),
                       .n.trees = (1:10)*100, .shrinkage = c(.05, .1), .n.minobsinnode = c(15, 20))
set.seed(seed)
gbmModel <- train(form = resultFormula, data = filteredFeatureMatches, method = 'gbm',
                  trControl = cvContr, verbose = FALSE, 
                  tuneGrid = gbmGrid, distribution = 'multinomial',
                  metric = metricString)
bestGbm <- dplyr:::select(gbmModel$results, shrinkage, interaction.depth, n.minobsinnode, 
                          n.trees, Accuracy, Kappa, Sensitivity, Specificity, ROC, logLoss)
bestGbm <- dplyr:::filter(bestGbm, shrinkage == gbmModel$bestTune[1, 'shrinkage'],
                          interaction.depth == gbmModel$bestTune[1, 'interaction.depth'],
                          n.minobsinnode == gbmModel$bestTune[1, 'n.minobsinnode'],
                          n.trees == gbmModel$bestTune[1, 'n.trees'])
# Best model performance
bestGbm

# Plotting the resampling profile
trellis.par.set(caretTheme())
plot(gbmModel)

## Fine tuning

gbmGrid2 <- expand.grid(.interaction.depth = c(1:3),
                       .n.trees = (1:5)*25, .shrinkage = c(0.025, .05, .075), .n.minobsinnode = c(15, 20))
set.seed(seed)
gbmModel2 <- train(form = resultFormula, data = filteredFeatureMatches, method = 'gbm',
                  trControl = cvContr, verbose = FALSE, 
                  tuneGrid = gbmGrid2, distribution = 'multinomial',
                  metric = metricString)
bestGbm2 <- dplyr:::select(gbmModel2$results, shrinkage, interaction.depth, n.minobsinnode, 
                          n.trees, Accuracy, Kappa, Sensitivity, Specificity, ROC, logLoss)
bestGbm2 <- dplyr:::filter(bestGbm2, shrinkage == gbmModel2$bestTune[1, 'shrinkage'],
                          interaction.depth == gbmModel2$bestTune[1, 'interaction.depth'],
                          n.minobsinnode == gbmModel2$bestTune[1, 'n.minobsinnode'],
                          n.trees == gbmModel2$bestTune[1, 'n.trees'])
# Best model performance
bestGbm2

# Plotting the resampling profile
trellis.par.set(caretTheme())
plot(gbmModel2)

# Plotting the resampling profile
trellis.par.set(caretTheme())
plot(gbmModel)
plot(gbmModel, metric = 'Kappa')

### choosing final gbm fit ###
bestGbmConfig <- gbmModel$bestTune

#bestGbm <- tolerance(gbmModel$results, metric = metricString,
#                         tol = 1, maximize = TRUE)
#bestGbmResult <- gbmModel$results[bestGbm,1:6]
#bestGbmConfig <- expand.grid(.interaction.depth = bestGbmResult$interaction.depth,
#                             .n.trees = bestGbmResult$n.trees,
#                             .shrinkage = bestGbmResult$shrinkage,
#                             .n.minobsinnode = bestGbmResult$n.minobsinnode)

set.seed(seed)
bestGbmModel <- train(form = resultFormula, data = filteredFeatureMatches, method = 'gbm',
                  trControl = cvContr, verbose = FALSE, 
                  tuneGrid = bestGbmConfig, distribution = 'multinomial',
                  metric = metricString)
bestGbmModel
getTrainPerf(bestGbmModel)

### eXtreme Gradient Boosting ###
library(xgboost)
extrBoostGrid <- expand.grid(nrounds = (1:15)*20,
                          eta = c(.025, .05, .075, .1, .15),
                          max_depth = c(1, 2, 3))
set.seed(seed)
extrBoostModel <- train(form = resultFormula, data = filteredFeatureMatches, method = 'xgbTree',
                     trControl = cvContr, tuneGrid = extrBoostGrid, metric = 'Accuracy',
                     objective = 'multi:softprob', num_class = 3, 
                     colsample_bytree = 1, min_child_weight = 1)

bestExtrBoost <- dplyr:::select(extrBoostModel$results, nrounds, max_depth, eta, 
                                Accuracy, Kappa, Sensitivity, Specificity, ROC, logLoss)
bestExtrBoost <- dplyr:::filter(bestExtrBoost, nrounds == extrBoostModel$bestTune[1, 'nrounds'],
                                max_depth == extrBoostModel$bestTune[1, 'max_depth'],
                                eta == extrBoostModel$bestTune[1, 'eta'])
bestExtrBoost 

# Training Performance without resampling
testPred <- predict(extrBoostModel, filteredFeatureMatches)
confMatrix <- confusionMatrix(testPred, reference = filteredFeatureMatches$matchResult)
confMatrix$overall[1:2]

# Plotting the resampling profile
trellis.par.set(caretTheme())
plot(extrBoostModel)

bestExtrBoostConfig <- extrBoostModel$bestTune

### Integrate custom metric 'percentage profit' into caret train

cvContr = trainControl(method = 'cv', number = 5, classProbs = TRUE, 
                       summaryFunction = multiClassSummary)
set.seed(7382)
polrModel <- train(form = resultFormula, data = filteredFeatureMatches, method = 'polr',
                   preProcess = c('center', 'scale'),
                   trControl = cvContr)
polrModel

customGrid <- trainControl(method = 'cv', number = 5, classProbs = TRUE,
                           summaryFunction = betMetricsSummary)
set.seed(seed)
testModel <- train(form = resultFormula, data = filteredFeatureMatches, method = 'polr',
                        trControl = customGrid)
confusionMatrix(testModel)

#### Exploring Performance Distributions within Resamples ####

resamps <- resamples(list(POLR = polrModel,
                          GBM = gbmModel,
                          EXTRBOOST = extrBoostModel))

trellis.par.set(caretTheme())
bwplot(resamps, layout = c(3, 1))

splom(resamps)

difValues <- diff(resamps)
summary(difValues)

trellis.par.set(caretTheme())
dotplot(difValues)


#### Comparison to Booky Odds ####
source(file = './evaluatePrediction.R', 
       echo = FALSE, encoding = 'UTF-8')
bookySummary <- getBookyPerformance(odds = odds, matches = matches)
bookySummary

folds <- 10
noneContr <- trainControl(method = 'none', classProbs = TRUE)

# Split Data
splits <- splitMatches(matchesToSplit = filteredFeatureMatches, splitBy = filteredFeatureMatches$matchResult,
                       testingMatches = filteredFeatureMatches, folds = folds, seed = seed)

# Resampling
allPredictions <- data.frame()
for(i in 1:folds) {
    actTrain <- splits[[i]]$train
    actTest <- splits[[i]]$test
    
    set.seed(seed)
    actPolrFit <- caret:::train(form = resultFormula, data = actTrain, 
                                method = 'polr', preProcess = c('center', 'scale'),
                                trControl = cvContr)
    set.seed(seed)
    actGbmFit <- caret:::train(form = resultFormula, data = actTrain, 
                               method = 'gbm', trControl = noneContr,
                               verbose = FALSE, distribution = 'multinomial',
                               tuneGrid = bestGbmConfig)
    
    actExtrBoostFit <- caret:::train(form = resultFormula, data = actTrain, method = 'xgbTree',
                            trControl = noneContr, tuneGrid = bestExtrBoostConfig,
                            objective = 'multi:softprob', num_class = 3, 
                            colsample_bytree = 1, min_child_weight = 1)
    
    models <- list('POLR' = actPolrFit, 'GBM' = actGbmFit, 'EXTRBOOST' = actExtrBoostFit)
    
    preds <- predict(models, actTest, type = 'prob')
    preds <- do.call(cbind.data.frame, preds)
    preds <- cbind('matchId' = actTest$matchId,
                   'matchResult' = actTest$matchResult, 
                   preds)
    
    if(nrow(allPredictions) == 0) {
        allPredictions <- preds
    } else {
        allPredictions <- rbind(allPredictions, preds)
    }
}

source(file = './evaluatePrediction.R', 
       echo = FALSE, encoding = 'UTF-8')
allPredictions <- arrange(allPredictions, matchId)
polrPreds <- dplyr:::select(allPredictions, matchId, matchResult, 
                            'HomeVictory' = POLR.HomeVictory, 
                            'VisitorsVictory' = POLR.VisitorsVictory,
                            'Draw' = POLR.Draw)
polrEvals <- evaluatePrediction(prediction = polrPreds, 
                                comparison = odds, 
                                probRatioToBet = 1.1, stake = 1)
printEvaluation(polrEvals)

gbmPreds <- dplyr:::select(allPredictions, matchId, matchResult, 
                           'HomeVictory' = GBM.HomeVictory, 
                           'VisitorsVictory' = GBM.VisitorsVictory,
                           'Draw' = GBM.Draw)
gbmEvals <- evaluatePrediction(prediction = gbmPreds, 
                               comparison = odds, 
                               probRatioToBet = 1.1, stake = 1)
printEvaluation(gbmEvals)

extrBoostPreds <- dplyr:::select(allPredictions, matchId, matchResult, 
                          'HomeVictory' = EXTRBOOST.HomeVictory, 
                          'VisitorsVictory' = EXTRBOOST.VisitorsVictory,
                          'Draw' = EXTRBOOST.Draw)
extrBoostEvals <- evaluatePrediction(prediction = extrBoostPreds, 
                              comparison = odds, 
                              probRatioToBet = 1.1, stake = 1)
printEvaluation(extrBoostEvals)





















#### rf ####

rfNtree <- 1000
rfGrid <- expand.grid(.mtry = seq(2, ncol(filteredFeatureMatches) - 4, by = 3))
set.seed(seed)
rfModel <- train(form = resultFormula, data = filteredFeatureMatches, method = 'rf',
                 trControl = cvContr, ntree = rfNtree, importance = TRUE, tuneGrid = rfGrid,
                 metric = 'Accuracy')
bestRf <- dplyr:::select(rfModel$results, mtry, Accuracy, Kappa, Sensitivity, Specificity, ROC, logLoss)
bestRf <- dplyr:::filter(bestRf, mtry == rfModel$bestTune[1, 1])
bestRf

testPred <- predict(rfModel, filteredFeatureMatches)
confMatrix <- confusionMatrix(testPred, reference = filteredFeatureMatches$matchResult)
# Training Performance without resampling
confMatrix$overall[1:2]



trellis.par.set(caretTheme())
plot(rfModel)

## Choosing final rf fit
bestRfConfig <- rfModel$bestTune
set.seed(seed)
bestRfModel <- train(form = resultFormula, data = filteredFeatureMatches, method = 'rf',
                 trControl = cvContr, ntree = rfNtree, importance = TRUE, tuneGrid = bestRfConfig,
                 metric = metricString)
bestRfModel


#bestRf <- tolerance(rfModel$results, metric = metricString,
#                         tol = 1, maximize = TRUE)
#bestRfResult <- rfModel$results[bestRf,1:3]
#bestRfConfig <- expand.grid(.mtry = bestRfResult$mtry)
#bestRfConfig









######### Initialize stacked Modelling ###########

source(file = './production/models.R', 
       echo = FALSE, encoding = 'UTF-8')
noneContr = trainControl(method = 'none')
folds <- 10
seed <- 1257

### First level goal model selection ###

cv5FoldContr <- trainControl(method = 'cv', number = 5)
homeGoalFormula <- as.formula('goalsHome ~ . -matchId -matchResult -goalsVisitors')
visitorsGoalFormula <- as.formula('goalsVisitors ~ . -matchId -matchResult -goalsHome')

### random forest ###

rfGrid <- expand.grid(.mtry = seq(from = 2, to = 44, by = 4))
rf500Model <- trainModel(formula = formula, train = featuredData, 
                         meth = 'rf', tControl = cv5FoldContr, 
                         seed = seed, tGrid = rfGrid, ntree = 500)
rf500Model

rf1000Model <- trainModel(formula = formula, train = featuredData, 
                          meth = 'rf', tControl = cv5FoldContr, 
                          seed = seed, tGrid = rfGrid, ntree = 1000)
rf1000Model

### gbm ###

gbmGrid <- expand.grid(.interaction.depth = (1:5) * 2,
                       .n.trees = (1:10)*25, .shrinkage = c(.05, .1, .15), .n.minobsinnode = c(5, 10, 15))
gbmModel <- trainModel(formula = formula, train = featuredData, 
                       meth = 'gbm', tControl = cv5FoldContr, 
                       seed = seed, tGrid = gbmGrid)
gbmModel

### knn ###

knnGrid <- expand.grid(.k = c(1, 2, 3, 5, 10, 20, 40, 80, 120, 300))
knnModel <- trainModel(formula = formula, train = featuredData, 
                       meth = 'knn', tControl = cv5FoldContr, 
                       seed = seed, tGrid = knnGrid)
knnModel


# Configure models to integrate

rfConfs <- createRfModelConfigs(
    mtrys = seq(from = 2, to = 6, by = 2), ntrees = c(500, 1000), 
    hFormula = homeGoalFormula, vFormula = visitorsGoalFormula)
knnConfs <- createKnnModelConfigs(
    k = 2 ^ c(1:8), hFormula = homeGoalFormula, vFormula = visitorsGoalFormula)

allModelConfigs <- list()
allModelConfigs <- append(allModelConfigs, rfConfs)
allModelConfigs <- append(allModelConfigs, knnConfs)

# Split Data
splits <- splitMatches(matchesToSplit = filteredFeatureMatches, splitBy = filteredFeatureMatches$matchResult,
                       testingMatches = filteredFeatureMatches, folds = folds, seed = seed)

# Predict goals with configured models
firstLevelPredictions <- data.frame()
for(i in 1:folds) {
    actTrain <- splits[[i]]$train
    actTest <- splits[[i]]$test
    

    models <- trainModels(modelConfigList = allModelConfigs, trainset = actTrain, 
                              tContr = noneContr, seed = seed) 

    preds <- predict(models, actTest)
    preds <- do.call(cbind.data.frame, preds)
    preds <- cbind('matchId' = actTest$matchId,
                   'matchResult' = actTest$matchResult, 
                   'goalsHome' = actTest$goalsHome,
                   'goalsVisitors' = actTest$goalsVisitors,
                   preds)
    
    if(nrow(firstLevelPredictions) == 0) {
        firstLevelPredictions <- preds
    } else {
        firstLevelPredictions <- rbind(firstLevelPredictions, preds)
    }
}

# Inspect correlation of first level predictions
library(RColorBrewer)
library(gplots)
explMatches <- dplyr:::select(firstLevelPredictions, -matchId, -matchResult)
featureCorrelations <- cor(explMatches)

# creates a own color palette from red to green
my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)

# (optional) defines the color breaks manually for a "skewed" color transition
col_breaks = c(seq(-1,0,length=100),  # for red
               seq(0,0.8,length=100),              # for yellow
               seq(0.8,1,length=100))              # for green

heatmap.2(featureCorrelations,
          key=T, 
          keysize=1.5,
          density.info="none",
          trace="none",
          margins =c(12,9),
          col=my_palette,       # use on color palette defined earlier 
          #breaks=col_breaks,    # enable color transition at specified limits
          cexCol=0.9, 
          cexRow = 0.9)

corMatrix <- cor(select(firstLevelPredictions, -c(matchId, matchResult) ))
corMatrix

########################## Second Level ########################

############ POLR ###########
cv5FoldContr <- trainControl(method = 'cv', number = 5, classProbs = TRUE)
resultFormula <- as.formula('matchResult ~ . -matchId -goalsHome -goalsVisitors')

polrModel <- trainModel(formula = resultFormula, train = firstLevelPredictions, 
                        meth = 'polr', prePr = c('center', 'scale'), 
                        tControl = cv5FoldContr, seed = seed)
polrModel

### gbm ###

gbmGrid <- expand.grid(.interaction.depth = (1:5) * 2,
                       .n.trees = (1:10)*25, .shrinkage = c(.05, .1, .15), .n.minobsinnode = c(5, 10, 15))
gbmModel <- trainModel(formula = resultFormula, train = firstLevelPredictions, 
                       meth = 'gbm', tControl = cv5FoldContr, 
                       seed = seed, tGrid = gbmGrid)
gbmModel

### knn ###

knnGrid <- expand.grid(.k = c(1, 2, 3, 5, 10, 20, 40, 80, 120, 300))
knnModel <- trainModel(formula = resultFormula, train = firstLevelPredictions, 
                       meth = 'knn', tControl = cv5FoldContr, 
                       seed = seed, tGrid = knnGrid)
knnModel

### random forest ###
### NOT SUITABLE ###
rfGrid <- expand.grid(.mtry = seq(from = 2, to = 6, by = 2))
rf500Model <- trainModel(formula = resultFormula, train = firstLevelPredictions, 
                         meth = 'rf', tControl = cv5FoldContr, 
                         seed = seed, tGrid = rfGrid, ntree = 500)
rf500Model

rf1000Model <- trainModel(formula = resultFormula, train = firstLevelPredictions, 
                          meth = 'rf', tControl = cv5FoldContr, 
                          seed = seed, tGrid = rfGrid, ntree = 1000)
rf1000Model


### Booky Comparison ###

allSecLevModelConfigs <- createModelConfigEntry(
    'polr', tGrid = NULL, meth = 'polr', 
    formula = resultFormula)
allSecLevModelConfigs <- append(list(allSecLevModelConfigs), list(createModelConfigEntry(
    'knn_10', tGrid = expand.grid(.k = 10), meth = 'knn', formula = resultFormula)))
allSecLevModelConfigs <- append(allSecLevModelConfigs, list(createModelConfigEntry(
    'gbm_1', tGrid = expand.grid(.interaction.depth = 2,
                                 .n.trees = 75, .shrinkage = .1, .n.minobsinnode = 15), 
    meth = 'gbm', formula = resultFormula)))


secondLevelSplits <- splitMatches(matchesToSplit = firstLevelPredictions, splitBy = firstLevelPredictions$matchResult,
                                  testingMatches = firstLevelPredictions, folds = folds, seed = seed)

# Predict goalDiff with configured models
secondLevelPredictions <- data.frame()
# TEST
#i <- 1
for(i in 1:folds) {
    actTrain <- secondLevelSplits[[i]]$train
    actTest <- secondLevelSplits[[i]]$test
    
    
    models <- trainModels(modelConfigList = allSecLevModelConfigs, trainset = actTrain, 
                          tContr = noneContr, seed = seed) 
    
    preds <- predict(models, actTest, type = 'prob')
    preds <- do.call(cbind.data.frame, preds)
    preds <- cbind('matchId' = actTest$matchId,
                   'matchResult' = actTest$matchResult, 
                   preds)
    
    if(nrow(secondLevelPredictions) == 0) {
        secondLevelPredictions <- preds
    } else {
        secondLevelPredictions <- rbind(secondLevelPredictions, preds)
    }
}

source(file = './evaluatePrediction.R', 
       echo = FALSE, encoding = 'UTF-8')

# Seperate Second Level Predictions
# TEST
#aktConfig <- allSecLevModelConfigs[[1]]
for(aktConfig in allSecLevModelConfigs) {
    evalSet <- dplyr:::select(secondLevelPredictions, matchId, matchResult)
    
    predsSet <- secondLevelPredictions[, grepl(pattern = aktConfig$modelName, 
                                                             x = colnames(secondLevelPredictions))]
    evalSet <- cbind(evalSet, predsSet)
    
    cNames <- substr(colnames(predsSet), nchar(aktConfig$modelName) + 2, nchar(colnames(predsSet)))
    colnames(evalSet) <- c('matchId', 'matchResult', cNames)
    
    evaluations <- evaluatePrediction(prediction = evalSet, 
                                      comparison = odds, 
                                      probRatioToBet = 1.1, stake = 1)
    print(aktConfig)
    printEvaluation(evaluations)
}




