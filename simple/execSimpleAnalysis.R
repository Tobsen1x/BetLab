# Prepare Data

### Load Data
source(file = 'loadData.R', echo = FALSE, encoding = 'UTF-8')
data <- loadData('BL1')
stats <- data$playerStats
odds <- data$odds

### Enrich with adjGrades
source(file = 'adjGradeModel.R', echo = FALSE, encoding = 'UTF-8')
adjGradeStats <- enrichAdjGrade(stats)
# Merges adjusted grade in stats
mergedStats <- merge(stats, subset(adjGradeStats, select = c(matchId, playerId, adjGrade)),
                     by = c('matchId', 'playerId'),  all.x = TRUE)

source(file = 'formModel.R', echo = FALSE, encoding = 'UTF-8')
source(file = 'simpleResultFeatureExtraction.R', echo = FALSE, encoding = 'UTF-8')
source(file = 'simpleResultModel.R', echo = FALSE, encoding = 'UTF-8')
source(file = 'evaluatePrediction.R', echo = FALSE, encoding = 'UTF-8')

# Costruct Parameter input for the following procession
params <- data.frame(formAlgorithm = c('arima', 'arima', 'arima'), minMatchdays = c(4,4,5), maxMatchdays = c(NA, NA, NA), 
                     imputeBenchBy = c(0, -0.1, -0.05), imputeNotPlayedBy = c(-0.1, -0.2, -0.1))

evaluationList <- list()

# Execute further processing for each param constallation
for(row in seq_len(nrow(params))) {
    aktParas <- params[row, ]
    
    ### Enrich with form data
    formEnrichedPlayerStats <- enrichForm(mergedStats, formAlgorithm = aktParas$formAlgorithm,  
                                          minMatchdays = aktParas$minMatchdays,
                                          maxMatchdays = aktParas$maxMatchdays,
                                          imputeBenchBy = aktParas$imputeBenchBy,
                                          imputeNotPlayedBy = aktParas$imputeNotPlayedBy)
    
    ### Simple Feature Extraction for matches  
    simpleMatchFeatures <- simpleMatchFeatureExtract(formEnrichedPlayerStats)
    
    ### Result model fit
    allResults <- fitSimpleResultModel(simpleMatchFeatures)
    
    ### Prediction evaluation
    evaluation <- evaluatePrediction(allResults)
    printEvaluation(evaluation)
    evaluationList <- c(evaluationList, row = evaluation)
}

# TEST
matchId <- 2069
simpleMatchFeatures[simpleMatchFeatures$matchId == matchId,]
evaluationGoalDiffPredictions[evaluationGoalDiffPredictions$matchId == matchId, ]

train <- ergModelData[ergModelData$matchId != matchId, ]
test <- ergModelData[ergModelData$matchId == matchId, ]

prep <- as.data.frame(predict(allModels, newdata = ergModelData))
evaluationGoalDiffPredictions$matchResult <- ergModelData$matchResult
evaluationGoalDiffPredictions$matchId <- ergModelData$matchId
evaluationResults <- predict(polrCombFit, newdata = evaluationGoalDiffPredictions, type = 'prob')
evaluationResults$matchId <- ergModelData$matchId
evaluationResults$matchResult <- ergModelData$matchResult

relevantPredictions <- evaluatePrediction(prediction = evaluationResults)
printEvaluation(relevantPredictions)