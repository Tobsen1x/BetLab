model <- readRDS('C:/BetLab/SoccerLab/ModelFiles/BL1_2005-2015_1-34.rds')

require(caret)
require(dplyr)
source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/production/loadData.R', 
       echo = FALSE, encoding = 'UTF-8')

predData <- loadPredictionData(matchId = 3061)
View(predData$stats)
View(predData$matches)

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/production/positionFeatureExtraction.R', 
       echo = FALSE, encoding = 'UTF-8')

### Preparation
#[1] "Torwart"               "Innenverteidiger"      "Linker Verteidiger"    "Rechter Verteidiger"   "Defensives Mittelfeld"
#[6] "Zentrales Mittelfeld"  "Linkes Mittelfeld"     "Rechtes Mittelfeld"    "Offensives Mittelfeld" "Haengende Spitze"     
#[11] "Mittelstuermer"        "Linksaussen"           "Rechtsaussen"
positions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 'mid', 'off', 'off', 'off', 'off', 'off')
featuredMatches <- extractMatchResultFeatures(playerStats = predData$stats,
                                              matches = predData$matches,
                                              priceAssignedPositions = positions,
                                              functs = c('min', 'max', 'avg', 'sum'))
featuredMatches <- dplyr:::select(featuredMatches, -tw_Price_Home_min, -tw_Price_Home_max,
                                  -tw_Price_Home_sum, -tw_Price_Visitors_min, -tw_Price_Visitors_max,
                                  -tw_Price_Visitors_sum)
View(featuredMatches)
summary(featuredMatches)

featuredMatches$matchResult <- NA
newData <- rbind(featuredMatches, featuredMatches)

finModel <- model$finalModel
finModel$coefficients
modelInput <- dplyr:::select(featuredMatches, -(1:9))
head(predict(model, newdata = newData, type = 'prob'))

model

##################

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/production/predictMatch.R', 
       echo = FALSE, encoding = 'UTF-8')

preds <- predictMatch(model, 3061)
preds
preds[, 'HomeVictory']
preds[, 'Draw']
preds[, 'VisitorsVictory']
