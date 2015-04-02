### Load Data
source(file = 'loadData.R', echo = FALSE, encoding = 'UTF-8')
data <- loadData('BL1')
stats <- data$playerStats
matches <- data$matches
odds <- data$odds
# --> loadedData.RData

### Enrich with adjGrades
source(file = 'adjGradeModel.R', echo = FALSE, encoding = 'UTF-8')
adjGradeData <- extractFeaturesForAdjGradeModel(stats)
enrichedStats <- enrichAdjGrade(adjGradeData)
# Merges adjusted grade in stats
mergedStats <- merge(stats, select(
    enrichedStats, matchId, playerId, adjGrade),
    by = c('matchId', 'playerId'),  all.x = TRUE)
summary(mergedStats$adjGrade)
# --> adjGradeEnrichedData.RData

### Enrich diagonal inflated bivariate poisson expected goals
load('data/bivPois.RData')
source(file = 'bivpois/estimateGoals.R', echo = FALSE, encoding = 'UTF-8')
enrMatches <- enrichBivPoisExpGoals(matches, minMatchdays = 5)
describe(select(enrMatches, goalsHome, goalsVisitors, 
                homeExpGoals, visitorsExpGoals))

### Enrich double poisson expected chances
source(file = 'bivpois/estimateChances.R', echo = FALSE, encoding = 'UTF-8')
chancesEnrMatches <- enrichDoublePoissonExpChances(enrMatches, minMatchdays = 5)
describe(select(chancesEnrMatches, homeChances, visitorsChances, 
                homeExpChances, visitorsExpChances))
# --> matchStatsEnrichedData.RData

### Enrich player form
source(file = 'formModel.R', echo = FALSE, encoding = 'UTF-8')
formEnrichedPlayerStats <- enrichForm(mergedStats, matches, minMatchdays = 5,
                                      imputeBenchBy = -0.05, imputeNotPlayedBy = -0.1)
# --> formEnrichedData_staticImpute.RData


### Feature extraction
source(file = 'featureExtraction.R', echo = FALSE, encoding = 'UTF-8')
featuredMatches <- extractFeatures(formEnrichedPlayerStats, chancesEnrMatches)
describe(select(featuredMatches, goalDiff, priceDiff, logPriceRate, formMeanfDiff, 
                formSesOptimalDiff, formSesSimpleDiff, expGoalDiff, expChancesDiff))

# Filter observations for model fit
featuredMatches <- filter(featuredMatches, !is.na(goalDiff), !is.na(expGoalDiff), 
       !is.na(expChancesDiff), !is.na(priceDiff), !is.na(formMeanfDiff))

library(psych)
library(dplyr)
source(file = 'resultModel.R', echo = FALSE, encoding = 'UTF-8')
splits <- splitMatches(featuredMatches, folds = 10, seed = 1234)

allPredictions <- NA
for(i in 1:10) {
    testset <- splits[[i]]$testset
    goalDiffTrain <- splits[[i]]$goalDiffTrainset
    resultTrain <- splits[[i]]$resultTrainset
    
    testsetPredictions <- fitResultModel(goalDiffTrainset = goalDiffTrain,
                                         resultTrainset = resultTrain,
                                         testset = testset, seed = 1234)
    if(is.na(allPredictions)) {
        allPredictions <- testsetPredictions
    } else {
        allPredictions <- rbind(allPredictions, testsetPredictions)
    }
}

source(file = 'evaluatePrediction.R', echo = FALSE, encoding = 'UTF-8')
allEvaluations <- evaluatePrediction(prediction = allPredictions, comparison = odds)
printEvaluation(allEvaluations)
