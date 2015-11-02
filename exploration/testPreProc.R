data <- readRDS('C:/Users/Tobsen1X/RStudioWorkspace/BetLab/rds/2005-2016_1_20150817.rds')

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/production/positionFeatureExtraction.R', 
       echo = FALSE, encoding = 'UTF-8')

testingMatchId <- 3068

### Preparation
#[1] "Torwart"               "Innenverteidiger"      "Linker Verteidiger"    "Rechter Verteidiger"   "Defensives Mittelfeld"
#[6] "Zentrales Mittelfeld"  "Linkes Mittelfeld"     "Rechtes Mittelfeld"    "Offensives Mittelfeld" "Haengende Spitze"     
#[11] "Mittelstuermer"        "Linksaussen"           "Rechtsaussen"
positions <- c('tw', 'def', 'def', 'def', 'def', 
               'mid', 'mid', 'mid', 'mid', 'off', 
               'off', 'off', 'off')

# testFeatures enthält Daten der wahren Aufstellung
testMatch <- filter(data$matches, matchId == testingMatchId)
testFeatures <- extractMatchResultFeatures(playerStats = data$stats,
                                           matches = testMatch,
                                           priceAssignedPositions = positions,
                                           functs = c('min', 'max', 'avg', 'sum'))
testFeatures <- dplyr::select(testFeatures, matchId, 
                              grep(pattern = 'Price', colnames(testFeatures)))
testFeatures <- dplyr:::select(testFeatures, -tw_Price_Home_min, -tw_Price_Home_max,
                               -tw_Price_Home_sum, -tw_Price_Visitors_min, -tw_Price_Visitors_max,
                               -tw_Price_Visitors_sum)

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/production/loadData.R', 
       echo = FALSE, encoding = 'UTF-8')
# progTestFeatures enthält Informationen der prognostizierten Aufstellung
predData <- loadPredictionData(matchId = 3068)
progTestFeatures <- extractMatchResultFeatures(playerStats = predData$stats,
                                               matches = predData$matches,
                                               priceAssignedPositions = positions,
                                               functs = c('min', 'max', 'avg', 'sum'))
progTestFeatures <- dplyr::select(progTestFeatures, matchId, 
                                  grep(pattern = 'Price', colnames(progTestFeatures)))
progTestFeatures <- dplyr:::select(progTestFeatures, -tw_Price_Home_min, -tw_Price_Home_max,
                                   -tw_Price_Home_sum, -tw_Price_Visitors_min, -tw_Price_Visitors_max,
                                   -tw_Price_Visitors_sum)

# compare player stats with prog player stats











### model fit ###

modelInputMatches <- filter(data$matches, matchId != testingMatchId)
modelInputFeatures <- extractMatchResultFeatures(playerStats = data$stats,
                                                 matches = modelInputMatches,
                                                 priceAssignedPositions = positions,
                                                 functs = c('min', 'max', 'avg', 'sum'))

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/production/models.R', 
       echo = FALSE, encoding = 'UTF-8')
modelInputFeatures <- filterFeaturedMatches(modelInputFeatures)
modelInputFeatures <- dplyr:::select(modelInputFeatures, -tw_Price_Home_min, -tw_Price_Home_max,
                                     -tw_Price_Home_sum, -tw_Price_Visitors_min, -tw_Price_Visitors_max,
                                     -tw_Price_Visitors_sum)


resultTrain <- dplyr:::select(featuredMatches, -c(matchId, goalDiff))
resultFormula <- as.Formula('matchResult ~ .')
tcontr <- trainControl(method = 'cv', number = 10)

set.seed(1234)
polrPreProcModel <- train(resultFormula, data = resultTrain, method = 'polr',
                   preProcess = c('center', 'scale'),
                   trControl = tcontr)

#polrModel <- train(resultFormula, data = resultTrain, method = 'polr',
#                   trControl = tcontr)


testset <- rbind(testFeatures, testFeatures)
predict(polrPreProcModel, testset, type = 'prob')
predict(polrModel, testset, type = 'prob')


progTestset <- rbind(progTestFeatures, progTestFeatures)
predict(polrPreProcModel, progTestset, type = 'prob')

comparison <- rbind(testFeatures, progTestFeatures)
View(comparison)
