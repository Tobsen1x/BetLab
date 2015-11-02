############# Load Raw data #################

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

############ Raw data exploration ##################

library(Hmisc)
library(plyr)
library(dplyr)
describe(select(matches, goalsHome, goalsVisitors, matchResult))
describe(select(odds, HomeVictory, VisitorsVictory, Draw))
describe(select(stats, fitPrice, position, playerAssignment, position, formation))

### TODO Statistic about players ###

############# Feature Engineering ###############

source('./production/positionFeatureExtraction.R', 
       echo = FALSE, encoding = 'UTF-8')
### Preparation
#[1] "Torwart"               "Innenverteidiger"      "Linker Verteidiger"    "Rechter Verteidiger"   "Defensives Mittelfeld"
#[6] "Zentrales Mittelfeld"  "Linkes Mittelfeld"     "Rechtes Mittelfeld"    "Offensives Mittelfeld" "Haengende Spitze"     
#[11] "Mittelstuermer"        "Linksaussen"           "Rechtsaussen"
positions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 'mid', 'off', 'off', 'off', 'off', 'off')
relNormalAssignments <- c('DURCHGESPIELT', 'EINGEWECHSELT', 'AUSGEWECHSELT')
normalMatches <- extractMatchResultFeatures(playerStats = stats,
                                            matches = matches,
                                            priceAssignedPositions = positions,
                                            functs = c('min', 'max', 'avg', 'sum'), 
                                            relNormalAssignments)

filteredNormalMatches <- filterFeaturedMatches(normalMatches)

############ Feature Exploration ################

explMatches <- select(filteredNormalMatches, -matchId, -matchResult, -goalDiff)
colnames(explMatches)

library(magrittr)
library(tidyr)
explGathered <- explMatches %>% gather(feature, value)

str(explGathered)
head(explGathered)
describe(explGathered)

getGroupStr <- function(feature, group) {
    charList <- strsplit(as.character(feature), '_')
    charFrame <- data.frame(do.call(rbind, charList))
    if(group == 'func') {
        return(charFrame[, 4])
    } else if(group == 'pos') {
        return(charFrame[, 1])
    } else {
        return(NA)
    }
}
groupedMatches <- mutate(explGathered, funct = factor(getGroupStr(feature, 'func')),
                         position = factor(getGroupStr(feature, 'pos')))
avgPlot <- ggplot(filter(groupedMatches, funct == 'avg'), aes(x = position, y = value, fill = position)) +
    geom_boxplot() +
    ggtitle('Avg Prices of players by position') +
    coord_cartesian(ylim = c(0, 10000000))
minPlot <- ggplot(filter(groupedMatches, funct == 'min'), aes(x = position, y = value, fill = position)) +
    geom_boxplot() +
    ggtitle('Min Prices of players by position') +
    coord_cartesian(ylim = c(0, 10000000))
maxPlot <- ggplot(filter(groupedMatches, funct == 'max'), aes(x = position, y = value, fill = position)) +
    geom_boxplot() +
    ggtitle('Max Prices of players by position') +
    coord_cartesian(ylim = c(0, 10000000))
sumPlot <- ggplot(filter(groupedMatches, funct == 'sum'), aes(x = position, y = value, fill = position)) +
    geom_boxplot() +
    ggtitle('Sum Prices of players by position') +
    coord_cartesian(ylim = c(0, 45000000))

avgPlot

#install.packages('grid')
#library(grid)
#multiplot(minPlot, maxPlot, avgPlot, sumPlot, cols = 2)

######### Inspect feature and goalDiff correlation ##############
install.packages('gplots')
library(gplots)
library(caret)
featureCorrelations <- cor(explMatches)
heatmap.2(featureCorrelations)
# Correlations with goalDiff
featureCorrelations[-1,1]
corIndex <- findCorrelation(featureCorrelations, cutoff = .95, verbose = FALSE)
# Potentiel columns to exclude
dimnames(featureCorrelations)[[1]][corIndex]

########### Model fitting #############################

library(plyr)
library(dplyr)
seed <- 1834
tcontr = trainControl(method = 'cv', number = 20, classProbs = TRUE)
train <- dplyr:::select(filteredNormalMatches, -matchId, -goalDiff)
resultFormula <- as.Formula('matchResult ~ .')
set.seed(seed)
polrModel <- train(resultFormula, data = train, method = 'polr',
                   preProcess = c('center', 'scale'),
                   trControl = tcontr)
polrModel

############ Iteratively predict training set ###########

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/production/models.R', 
       echo = FALSE, encoding = 'UTF-8')
test <- dplyr:::select(filteredNormalMatches, -goalDiff)
noneContr = trainControl(method = 'none')
folds <- 10
splits <- splitMatches(matchesToSplit = test, 
                       testingMatches = test, folds = folds, seed = seed)

allPredictions <- iterativelyPredict(splits, folds, 
                                     resultFormula, meth = 'polr', 
                                     prePr = c('center', 'scale'), 
                                     tControl = noneContr, seed = seed)

################ Evaluate predictions ##################

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/evaluatePrediction.R', 
       echo = FALSE, encoding = 'UTF-8')

evaluations <- evaluatePrediction(prediction = allPredictions, 
                                  comparison = odds, 
                                  probRatioToBet = 1.1, stake = 1)
printEvaluation(evaluations)

########### Prediction with just the starting lineup ##############

relPredAufstellungAssignments <- c('DURCHGESPIELT', 'AUSGEWECHSELT')
predAufstellungMatches <- extractMatchResultFeatures(playerStats = stats,
                                                     matches = matches,
                                                     priceAssignedPositions = positions,
                                                     functs = c('min', 'max', 'avg', 'sum'), 
                                                     relPredAufstellungAssignments)
filteredPredAufstellungMatches <- filterFeaturedMatches(predAufstellungMatches)

aufstTest <- dplyr:::select(filteredPredAufstellungMatches, -goalDiff)
noneContr = trainControl(method = 'none')
folds <- 10
aufstSplits <- splitMatches(matchesToSplit = aufstTest, 
                       testingMatches = aufstTest, folds = folds, seed = seed)

allPredictions <- iterativelyPredict(aufstSplits, folds, 
                                     resultFormula, meth = 'polr', 
                                     prePr = c('center', 'scale'), 
                                     tControl = noneContr, seed = seed)

aufstEvaluations <- evaluatePrediction(prediction = allPredictions, 
                                  comparison = odds, 
                                  probRatioToBet = 1.1, stake = 1)
printEvaluation(aufstEvaluations)

########### Try random forest ################

resultFormula <- as.formula('matchResult ~ . - matchId')
cv10FoldContr <- trainControl(method = 'cv', number = 10, classProbs = TRUE)
# Tuning parameters

tGrid <- expand.grid(.mtry = seq(from = 2, to = 26, by = 3))
rfModel <- trainModel(resultFormula = resultFormula, train = aufstTest, 
                      meth = 'rf', tControl = cv10FoldContr, 
                      seed = seed, tGrid = tGrid)
rfModel
# mtry = 14 is best

allPredictions <- iterativelyPredict(aufstSplits, folds, 
                                     resultFormula, meth = 'rf', 
                                     tControl = noneContr, seed = seed,
                                     tGrid = expand.grid(.mtry = 14))

aufstEvaluations <- evaluatePrediction(prediction = allPredictions, 
                                       comparison = odds, 
                                       probRatioToBet = 1.1, stake = 1)
printEvaluation(aufstEvaluations)
[1] "Stake: 1588"
[1] "Gain: -32.3"
[1] "Gain [%]: -2.03400503778338"
[1] "Value Diff [%]: -3.60373023721225"
[1] "Accuracy [%]: 48.7254901960784"
[1] "Booky Accuracy [%]: 50.6535947712418"

#################### Try extreme gradient boosting ################

