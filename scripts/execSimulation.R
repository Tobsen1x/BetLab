rdsRoot <- 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/rds/'

filteredNormalMatches <- readRDS(paste(rdsRoot, 'normalFeatured.rds', sep = ''))
filteredPredAufstellungMatches <- readRDS(paste(rdsRoot, 'predAufstellungFeatured.rds', sep = ''))

featuredMatches <- filteredNormalMatches
goalDiffInput <- dplyr:::select(featuredMatches, -c(matchId, matchResult))

### Construct Formulas
#allFormula <- paste('goalDiff ~ ', paste(colnames(goalDiffInput[, -1]), '', sep = '', collapse=' + '), sep = '')
#allFormula <- as.Formula(allFormula)

justDiffColumns <- colnames(select(goalDiffInput, grep(pattern = 'Diff', colnames(goalDiffInput))))
justDiffColumns <- justDiffColumns[! justDiffColumns %in% c('goalDiff')]
justDiffFormula <- paste('goalDiff ~ ', paste(justDiffColumns, '', sep = '', collapse=' + '), sep = '')
justDiffFormula <- as.Formula(justDiffFormula)

justRatioColumns <- colnames(select(goalDiffInput, grep(pattern = 'Ratio', colnames(goalDiffInput))))
justRatioColumns <- justRatioColumns[! justRatioColumns %in% c('goalDiff')]
justRatioFormula <- paste('goalDiff ~ ', paste(justRatioColumns, '', sep = '', collapse=' + '), sep = '')
justRatioFormula <- as.Formula(justRatioFormula)

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/production/models.R', 
       echo = FALSE, encoding = 'UTF-8')

noneContr <- trainControl(method = 'none')
folds <- 5
seed <- 1234
splits <- splitMatches(matchesToSplit = featuredMatches, 
                       testingMatches = featuredMatches, folds = folds, seed = seed)

allPredictions <- data.frame()
for(i in 1:folds) {
    goalDiffTrain <- splits[[i]]$train
    goalDiffTest <- splits[[i]]$test
    
    diffModels <- fitRegressionModels(goalDiffTrain, justDiffFormula,
                                          modelNamePrefix = 'diff', tcontr = noneContr)
    diffPredsList <- predict(diffModels, newdata = goalDiffTest)
    diffPreds <- as.data.frame(do.call(cbind, diffPredsList))
    
    ratioModels <- fitRegressionModels(goalDiffTrain, justRatioFormula,
                                       modelNamePrefix = 'ratio', tcontr = noneContr)
    ratioPredsList <- predict(ratioModels, newdata = goalDiffTest)
    ratioPreds <- as.data.frame(do.call(cbind, ratioPredsList))
    
    preds <- cbind(diffPreds, ratioPreds)
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














baseDataFilename <- paste(rdsRoot, '2005-2015_1_20150829.rds', sep = '')
trainingData <- readRDS(baseDataFilename)

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/production/positionFeatureExtraction.R', 
       echo = FALSE, encoding = 'UTF-8')

BL1matches <- filter(trainingData$matches, league == 'BL1')
PLmatches <- filter(trainingData$matches, league == 'PL')

BL1stats <- filter(trainingData$stats, league == 'BL1')
PLstats <- filter(trainingData$stats, league == 'PL')

relStats <- rbind(BL1stats, PLstats)
relMatches <- rbind(BL1matches, PLmatches)

### Preparation
#[1] "Torwart"               "Innenverteidiger"      "Linker Verteidiger"    "Rechter Verteidiger"   "Defensives Mittelfeld"
#[6] "Zentrales Mittelfeld"  "Linkes Mittelfeld"     "Rechtes Mittelfeld"    "Offensives Mittelfeld" "Haengende Spitze"     
#[11] "Mittelstuermer"        "Linksaussen"           "Rechtsaussen"
positions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 'mid', 'off', 'off', 'off', 'off', 'off')
colNames <- levels(relStats$position)
groupedPositions <- as.data.frame(t(positions))
colnames(groupedPositions) <- colNames
relStats$groupedStaticPosition <- sapply(relStats$staticPosition, FUN = function(x) groupedPositions[1, x])

relNormalAssignments <- c('DURCHGESPIELT', 'EINGEWECHSELT', 'AUSGEWECHSELT')
# Nur Startelf
#   relAssignments <- c('DURCHGESPIELT', 'AUSGEWECHSELT')



normalMatches <- extractMatchResultFeatures(playerStats = relStats,
                                            matches = relMatches,
                                            priceAssignedPositions = positions,
                                            functs = c('min', 'max', 'avg', 'sum'), 
                                            'normal', relNormalAssignments)
predAufstellungMatches <- extractMatchResultFeatures(playerStats = relStats,
                                            matches = relMatches,
                                            priceAssignedPositions = positions,
                                            functs = c('min', 'max', 'avg', 'sum'), 
                                            'predAufstellung')

filteredNormalMatches <- filterFeaturedMatches(normalMatches)
filteredPredAufstellungMatches <- filterFeaturedMatches(predAufstellungMatches)

saveRDS(filteredNormalMatches, paste(rdsRoot, 'normalFeatured.rds', sep = ''))
saveRDS(filteredPredAufstellungMatches, paste(rdsRoot, 'predAufstellungFeatured.rds', sep = ''))

filteredNormalMatches <- readRDS(paste(rdsRoot, 'normalFeatured.rds', sep = ''))
filteredPredAufstellungMatches <- readRDS(paste(rdsRoot, 'predAufstellungFeatured.rds', sep = ''))

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/production/models.R', 
       echo = FALSE, encoding = 'UTF-8')
### Choosing Training Data
trainingConf <- c('normal', 'predAufstellung')
#trainingConf <- c('normal')
testingConf <- c('predAufstellung')

folds <- 20
seed <- 1234
resultFormula <- as.Formula('matchResult ~ .')
preds <- simulate(filteredNormalMatches, filteredPredAufstellungMatches,
                  trainingConf, testingConf, folds, seed, resultFormula)

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/evaluatePrediction.R', echo = FALSE, encoding = 'UTF-8')
ratiosToBet <- c(1.1)
for(ratio in ratiosToBet) {
    print(ratio)
    actEvaluations <- evaluatePrediction(prediction = preds, comparison = trainingData$odds, probRatioToBet = ratio)
    printEvaluation(actEvaluations)
}