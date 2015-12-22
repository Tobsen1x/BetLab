                        ### Tuning GBM Model ###

library(ggplot2)
library(vioplot)
library(RMySQL)
library(caret)
library(e1071)
library(pROC)
library(gridExtra)
library(magrittr)
library(Hmisc)
library(MASS)
library(gbm)
library(plyr)
library(dplyr)

print(paste('Memory limit:', memory.limit()))
print(paste('Memory size:', memory.size()))

sink(file = 'models/gbmTuning.txt', append = FALSE)
print('GBM Tuning')
print('----------')
print('')

### Loading Datasets ###

source('./production/loadData.R', 
       echo = FALSE, encoding = 'UTF-8')
toMatchday <- 34
seasons <- c('2005-2006', '2006-2007', '2007-2008', '2008-2009', '2009-2010',
             '2010-2011', '2011-2012', '2012-2013', '2013-2014', '2014-2015')
leagues <- c('BL1')
trainingRaw <- loadTrainingData(toMatchday = toMatchday, seasons = seasons, leagues = leagues)
matches <- trainingRaw$matches
odds <- trainingRaw$odds
stats <- trainingRaw$stats

print(paste('Leagues:', paste(leagues, collapse = ', ')))
print(paste('Seasons:', paste(seasons, collapse = ', ')))

### Feature Engineering ###

source('./production/positionFeatureExtraction.R', 
       echo = FALSE, encoding = 'UTF-8')
### Preparation
#[1] "Torwart"               "Innenverteidiger"      "Linker Verteidiger"    "Rechter Verteidiger"   "Defensives Mittelfeld"
#[6] "Zentrales Mittelfeld"  "Linkes Mittelfeld"     "Rechtes Mittelfeld"    "Offensives Mittelfeld" "Haengende Spitze"     
#[11] "Mittelstuermer"        "Linksaussen"           "Rechtsaussen"
positions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 'mid', 'off', 'off', 'off', 'off', 'off')

# Integrate all participating players. That gives me an
# unrealistic information advantage, but we'll see...
lineupAssignments <- c('DURCHGESPIELT', 'AUSGEWECHSELT', 'EINGEWECHSELT')
featuredMatches <- extractMatchResultFeatures(playerStats = stats,
                                              matches = matches,
                                              priceAssignedPositions = positions,
                                              functs = c('min', 'max', 'avg', 'sum'), 
                                              lineupAssignments)

filteredFeatureMatches <- filterFeaturedMatches(featuredMatches)

print(paste('Lineup Assignments:', paste(lineupAssignments, collapse = ', ')))

### Initiate modelling ###

source(file = './production/models.R', 
       echo = FALSE, encoding = 'UTF-8')
seed <- 16459
customCvContr <- trainControl(method = 'cv', number = 10, classProbs = TRUE, 
                              summaryFunction = betMetricsSummary)
resultFormula <- as.formula('matchResult ~ . -matchId -goalsHome -goalsVisitors')
print(paste('Seed:', seed))
print('')

print('Initial Tuning Parameter:')
interaction.depth <- 1:6
n.trees <- (3:8)*100
shrinkage <- c(.05, .075, .1)
n.minobsinnode <- c(10, 15, 20)
print(paste('interaction.depth:', paste(interaction.depth, collapse = ',')))
print(paste('n.trees:', paste(n.trees, collapse = ',')))
print(paste('shrinkage:', paste(shrinkage, collapse = ',')))
print(paste('n.minobsinnode:', paste(n.minobsinnode, collapse = ',')))

print(paste('Start Tuning:', Sys.time(), collapse = ''))
gbmGrid <- expand.grid(.interaction.depth = interaction.depth,
                       .n.trees = n.trees, .shrinkage = shrinkage, .n.minobsinnode = n.minobsinnode)
set.seed(seed)
gbmModel <- train(form = resultFormula, data = filteredFeatureMatches, method = 'gbm',
                  trControl = customCvContr, verbose = FALSE, 
                  tuneGrid = gbmGrid, distribution = 'multinomial',
                  metric = 'GainPerc')
print(paste('Finished Tunig:', Sys.time(), collapse = ''))
# Best Tune
bestTune <- gbmModel$results[as.integer(rownames(gbmModel$results)) == as.integer(rownames(gbmModel$bestTune)), ]
print('Best Tune:')
print(bestTune)
sink()

# Plotting the resampling profile to file
jpeg("models/gbmTuningProfile.jpg")
plot(gbmModel)
dev.off()
