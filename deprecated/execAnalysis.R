### Load Data
source(file = 'loadData.R', echo = FALSE, encoding = 'UTF-8')
data <- loadData('BL1')
stats <- data$playerStats
matches <- data$matches
odds <- data$odds
players <- data$player
prices <- data$prices

load('data/basisData.RData')

describe(select(stats, playerAssignment))
head(filter(stats, playerAssignment == 'EINGEWECHSELT'))

### PositionFeatureExtraction
source(file = 'positionFeatureExtraction.R', echo = FALSE, encoding = 'UTF-8')

#colNames <- c('Torwart', 'Innenverteidiger', 'Linker Verteidiger', 'Rechter Verteidiger',
#              'Defensives Mittelfeld', 'Zentrales Mittelfeld', 'Linkes Mittelfeld',
#              'Rechtes Mittelfeld', 'Offensives Mittelfeld', 'Haengende Spitze', 
#              'Mittelstuermer', 'Linksaussen', 'Rechtsaussen')
#priceAssignedPositions <- c('tw', 'iv', 'lv', 'rv', 'dm', 'mit', 'lm', 'rm', 'om', 'om', 'st', 'ls', 'rs')
priceAssignedPositions <- c('def', 'def', 'def', 'def', 'mid', 'mid', 'mid', 'mid', 'off', 'off', 'off', 'off', 'off')
featuredMatches <- extractMatchResultFeatures(stats, matches, 
                                              priceAssignedPositions = priceAssignedPositions)

############### Predictions with just price feature set ##################
goalDiffTrain <- relevantFeatureMatches
formula <- as.formula('goalDiff ~ .')
goalDiffTrain <- dplyr::select(goalDiffTrain, -c(matchId, matchResult))
trainControl <- trainControl(method = 'cv', number = 5)
    
##########################################

source(file = 'resultModel.R', echo = FALSE, encoding = 'UTF-8')
folds <- 10
seed <- 1234
relevantFeatureMatches <- filterFeaturedMatches(featuredMatches)

allPredictionsList <- executeWholePrediction(featuredMatches = relevantFeatureMatches,
                                             folds = folds, seed = seed)

source(file = 'evaluatePrediction.R', echo = FALSE, encoding = 'UTF-8')
for(actName in names(allPredictionsList)) {
    actPredictions <- allPredictionsList[[actName]]
    actEvaluations <- evaluatePrediction(prediction = actPredictions, comparison = odds)
    print(paste('Result ', actName, ':', sep = ''))
    printEvaluation(actEvaluations)
    print('')
}

saveRDS(allPredictionsList, 'test.rds')
allPreds <- readRDS('test.rds')
###########################################################################
###########################################################################
################ Age Price Adjustment #################

### Age adjusted price model feature enrichment ###
source(file = 'adjustedPrice/enrichAdjustedPrice.R', echo = FALSE, encoding = 'UTF-8')
adjStatsData <- extractAdjStatsModelInput(player = players,
                                          playerStats = stats)
filteredAdjStatsData <- filter(adjStatsData, !is.na(kickerGrade))

### AgeAdjFitPrice Calculation ###
source(file = 'adjustedPrice/adjPriceModels.R', echo = FALSE, encoding = 'UTF-8')
refAge <- floor(mean(filteredAdjStatsData$age))
seed <- 1234
ageAdjStats <- enrichByAgeAdjFitPrice(filteredAdjStatsData, adjStatsData, 
                                      refAge, seed)

############### Form Price Adjustment #####################

source(file = 'adjustedPrice/enrichAdjustedPrice.R', echo = FALSE, encoding = 'UTF-8')
formModelInput <- extractAdjPriceModelInput(prices = prices, playerStats = stats, player = players)
filteredFormModelInput <- filter(formModelInput, !is.na(avgForm))

describe(select(formModelInput, weeksPast, age, avgForm, gradeCount, statsCount))

source(file = 'adjustedPrice/adjPriceModels.R', echo = FALSE, encoding = 'UTF-8')
formula <- as.formula('endPrice ~ startPrice + avgForm + age')
seed <- 1234
adjPriceModels <- fitModels(formula = formula, train = filteredFormModelInput, seed = seed)

### Exploration
summary(adjPriceModels$lm$finalModel)
exampleData <- createExampleObservations()
predictedExamples <- predictPrice(adjPriceModels, exampleData)
predDataFrame <- do.call(cbind.data.frame, predictedExamples)
exampleData <- cbind(exampleData, predDataFrame)

# Calculate AdjPrice
enrichedStats <- enrichStatsByAgeAndAvgForm(stats = stats, players = players)
describe(select(enrichedStats, avgForm, pastWeeks))
View(filter(enrichedStats, is.na(pastWeeks)))
adjPriceRelStats <- filter(enrichedStats, !is.na(avgForm), !is.na(fitPrice))
adjPriceInput <- data.frame('startPrice' = adjPriceRelStats$fitPrice, 
                            'avgForm' = adjPriceRelStats$avgForm,
                            'age' = adjPriceRelStats$age)
adjPricePrediction <- predict(adjPriceModels, adjPriceInput)
adjPriceRelStats <- cbind(adjPriceRelStats, adjPricePrediction)