source('./production/loadData.R', 
       echo = FALSE, encoding = 'UTF-8')
toMatchday <- 38
seasons <- c('2005-2006', '2006-2007', '2007-2008', '2008-2009', '2009-2010',
             '2010-2011', '2011-2012', '2012-2013', '2013-2014', '2014-2015')
leagues <- c('BL1')
trainingRaw <- loadTrainingData(toMatchday = 38, seasons = seasons, leagues = leagues)
matches <- trainingRaw$matches
odds <- trainingRaw$odds
stats <- trainingRaw$stats

### Feature Engineering ###

source('./production/positionFeatureExtraction.R', 
       echo = FALSE, encoding = 'UTF-8')
### Preparation
#[1] "Torwart"               "Innenverteidiger"      "Linker Verteidiger"    "Rechter Verteidiger"   "Defensives Mittelfeld"
#[6] "Zentrales Mittelfeld"  "Linkes Mittelfeld"     "Rechtes Mittelfeld"    "Offensives Mittelfeld" "Haengende Spitze"     
#[11] "Mittelstuermer"        "Linksaussen"           "Rechtsaussen"
positions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 'mid', 'off', 'off', 'off', 'off', 'off')
lineupAssignments <- c('DURCHGESPIELT', 'AUSGEWECHSELT')
benchFuncts = c('max', 'avg')
featuredMatches <- extractMatchResultFeatures(playerStats = stats,
                                            matches = matches,
                                            priceAssignedPositions = positions,
                                            functs = c('min', 'max', 'avg', 'sum'), 
                                            lineupAssignments, benchFuncts)

filteredFeatureMatches <- filterFeaturedMatches(featuredMatches)

#### Simple modeling ####

source(file = './production/models.R', 
       echo = FALSE, encoding = 'UTF-8')
cvContr = trainControl(method = 'cv', number = 5, classProbs = TRUE, 
                       summaryFunction = defaultSummary)
seed <- 12570

noFormationModelInput <- filteredFeatureMatches[, 1:(ncol(filteredFeatureMatches) - 6)]
resultFormula <- as.formula('matchResult ~ . -matchId -goalsHome -goalsVisitors')

polrModel <- trainModel(formula = resultFormula, train = noFormationModelInput, 
                        meth = 'polr', prePr = c('center', 'scale'), 
                        tControl = cvContr, seed = seed)
polrModel

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
#using metric = "Kappa" can improve quality of the final model

gbmGrid <- expand.grid(.interaction.depth = 1:5,
                       .n.trees = (1:10)*15, .shrinkage = c(.025, .05, .075), .n.minobsinnode = c(10, 15, 20))
gbmModel <- trainModel(formula = resultFormula, train = noFormationModelInput, 
                       meth = 'gbm', tControl = cvContr, 
                       seed = seed, tGrid = gbmGrid, metr = 'Kappa')
gbmModel

# Plotting the resampling profile
trellis.par.set(caretTheme())
plot(gbmModel)
plot(gbmModel, metric = 'Kappa')

# choosing final gbm fit
bestKappaGbm <- tolerance(gbmModel$results, metric = "Kappa",
                         tol = 1, maximize = TRUE)
bestGbmResult <- gbmModel$results[bestKappaGbm,1:6]
bestGbmConfig <- expand.grid(.interaction.depth = bestGbmResult$interaction.depth,
                             .n.trees = bestGbmResult$n.trees,
                             .shrinkage = bestGbmResult$shrinkage,
                             .n.minobsinnode = bestGbmResult$n.minobsinnode)

### rf ###

rfGrid <- expand.grid(.mtry = c(2, 3, 4, 11, 12, 13))
rfModel <- trainModel(formula = resultFormula, train = noFormationModelInput, 
                          meth = 'rf', tControl = cvContr, 
                          seed = seed, tGrid = rfGrid, ntree = 750, metr = 'Kappa')
rfModel

# Choosing final rf fit
bestKappaRf <- tolerance(rfModel$results, metric = "Kappa",
                         tol = 1, maximize = TRUE)
bestRfResult <- rfModel$results[bestKappaRf,1:3]
bestRfConfig <- expand.grid(.mtry = bestRfResult$mtry)
bestRfConfig

### Fitting final models to compare ###
cv10Contr = trainControl(method = 'cv', number = 10, classProbs = TRUE, 
                       summaryFunction = defaultSummary)
polrFinalModel <- trainModel(formula = resultFormula, train = noFormationModelInput, 
                             meth = 'polr', prePr = c('center', 'scale'), 
                             tControl = cv10Contr, seed = seed)
gbmFinalModel <- trainModel(formula = resultFormula, train = noFormationModelInput, 
                       meth = 'gbm', tControl = cv10Contr, 
                       seed = seed, tGrid = bestGbmConfig)
rfFinalModel <- trainModel(formula = resultFormula, train = noFormationModelInput, 
                           meth = 'rf', tControl = cv10Contr, 
                           seed = seed, tGrid = bestRfConfig, ntree = 750)

#### Exploring Performance Distributions within Resamples ####

resamps <- resamples(list(GBM = gbmFinalModel,
                          RF = rfFinalModel,
                          POLR = polrFinalModel))

trellis.par.set(caretTheme())
bwplot(resamps, layout = c(3, 1))

splom(resamps)

difValues <- diff(resamps)
summary(difValues)

trellis.par.set(caretTheme())
dotplot(difValues)


#### Comparison to Booky Odds ####
noneContr = trainControl(method = 'none', classProbs = TRUE)
folds <- 10
seed <- 12574

# Split Data
splits <- splitMatches(matchesToSplit = noFormationModelInput, splitBy = noFormationModelInput$matchResult,
                       testingMatches = noFormationModelInput, folds = folds, seed = seed)

# Predict goals with configured models
allPredictions <- data.frame()
for(i in 1:folds) {
    actTrain <- splits[[i]]$train
    actTest <- splits[[i]]$test
    
    actPolrFit <- trainModel(formula = resultFormula, train = actTrain, 
                                 meth = 'polr', prePr = c('center', 'scale'), 
                                 tControl = noneContr, seed = seed)
    actGbmFit <- trainModel(formula = resultFormula, train = actTrain, 
                                meth = 'gbm', tControl = noneContr, 
                                seed = seed, tGrid = bestGbmConfig)
    actRfFit <- trainModel(formula = resultFormula, train = actTrain, 
                               meth = 'rf', tControl = noneContr, 
                               seed = seed, tGrid = bestRfConfig, ntree = 750)
    
    models <- list('POLR' = actPolrFit, 'GBM' = actGbmFit, 'RF' = actRfFit)
    
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

rfPreds <- dplyr:::select(allPredictions, matchId, matchResult, 
                           'HomeVictory' = RF.HomeVictory, 
                           'VisitorsVictory' = RF.VisitorsVictory,
                           'Draw' = RF.Draw)
rfEvals <- evaluatePrediction(prediction = rfPreds, 
                               comparison = odds, 
                               probRatioToBet = 1.1, stake = 1)
printEvaluation(rfEvals)








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




