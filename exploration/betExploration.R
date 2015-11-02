data <- readRDS('C:/Users/Tobsen1X/RStudioWorkspace/BetLab/rds/2005-2016_1_20150817.rds')

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/production/positionFeatureExtraction.R', 
       echo = FALSE, encoding = 'UTF-8')

### Preparation
#[1] "Torwart"               "Innenverteidiger"      "Linker Verteidiger"    "Rechter Verteidiger"   "Defensives Mittelfeld"
#[6] "Zentrales Mittelfeld"  "Linkes Mittelfeld"     "Rechtes Mittelfeld"    "Offensives Mittelfeld" "Haengende Spitze"     
#[11] "Mittelstuermer"        "Linksaussen"           "Rechtsaussen"
positions <- c('tw', 'def', 'def', 'def', 'def', 
               'mid', 'mid', 'mid', 'mid', 'off', 
               'off', 'off', 'off')
featuredMatches <- extractMatchResultFeatures(playerStats = data$stats,
                                              matches = data$matches,
                                              priceAssignedPositions = positions,
                                              functs = c('min', 'max', 'avg', 'sum'))

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/production/models.R', 
       echo = FALSE, encoding = 'UTF-8')
featuredMatches <- filterFeaturedMatches(featuredMatches)
featuredMatches <- dplyr:::select(featuredMatches, -tw_Price_Home_min, -tw_Price_Home_max,
                                  -tw_Price_Home_sum, -tw_Price_Visitors_min, -tw_Price_Visitors_max,
                                  -tw_Price_Visitors_sum)

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/production/models.R', 
       echo = FALSE, encoding = 'UTF-8')

############# SIMULATION #############

folds <- 20
seed <- 1234
resultFormula <- as.formula('matchResult ~ .')
allPreds <- simulate(featuredMatches, resultFormula = resultFormula, 
                     folds = folds, seed = seed)

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/evaluatePrediction.R', 
       echo = FALSE, encoding = 'UTF-8')

evaluations <- evaluatePrediction(prediction = allPreds, 
                                  comparison = data$odds, 
                                  probRatioToBet = 1.1, stake = 1)
printEvaluation(evaluations)

################# EXPLORE BETS ################

bets <- evaluations$placedBets
enrBets <- merge(x = bets, y = data$matches, by.x = 'matchId', by.y = 'matchId') 
filter(enrBets, season == '2015-2016')
library(Hmisc)
describe(bets)

### Explore by Bet event

byBetevent <- group_by(bets, betOnOutcome)
aggrByEvent <- summarise(byBetevent, gainSum = sum(gain))
aggrByEvent

### Explore by season

bySeason <- group_by(enrBets, season)
aggrBySeason <- summarise(bySeason, gainSum = sum(gain))
aggrBySeason

### Explore outlierst
highProbBets <- filter(bets, predProb > 0.9)
sum(highProbBets$gain)

View(filter(featuredMatches, matchId == 1749))




### TEST ###

train <- filter(featuredMatches, matchId != 1749)
testModel <- fitModel(train, resultFormula, seed = 1234)
test <- filter(featuredMatches, matchId == 1749)
test <- rbind(test, test)
predict(testModel, test, type = 'prob')[1]
