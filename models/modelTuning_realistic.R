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


### Feature Engineering ###

source('./production/positionFeatureExtraction.R', 
       echo = FALSE, encoding = 'UTF-8')
### Preparation
#[1] "Torwart"               "Innenverteidiger"      "Linker Verteidiger"    "Rechter Verteidiger"   "Defensives Mittelfeld"
#[6] "Zentrales Mittelfeld"  "Linkes Mittelfeld"     "Rechtes Mittelfeld"    "Offensives Mittelfeld" "Haengende Spitze"     
#[11] "Mittelstuermer"        "Linksaussen"           "Rechtsaussen"
positions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 'mid', 'off', 'off', 'off', 'off', 'off')

# Integrate starting lineup and additional features for players on bench
lineupAssignments <- c('DURCHGESPIELT', 'AUSGEWECHSELT')
benchFuncts <- c('max', 'avg')
featuredMatches <- extractMatchResultFeatures(playerStats = stats,
                                              matches = matches,
                                              priceAssignedPositions = positions,
                                              functs = c('min', 'max', 'avg', 'sum'), 
                                              lineupAssignments,
                                              benchFuncts = benchFuncts)

filteredFeatureMatches <- filterFeaturedMatches(featuredMatches)

### Initiate modelling ###

source(file = './production/models.R', 
       echo = FALSE, encoding = 'UTF-8')
seed <- 16450
customCvContr <- trainControl(method = 'cv', number = 5, classProbs = TRUE, 
                              summaryFunction = betMetricsSummary)
resultFormula <- as.formula('matchResult ~ . -matchId -goalsHome -goalsVisitors')

### Simple POLR Model ###

set.seed(seed)
polrModel <- train(form = resultFormula, data = filteredFeatureMatches, method = 'polr',
                   preProcess = c('center', 'scale'), trControl = customCvContr)
polrModel
confusionMatrix(polrModel)

# POLR Resampling exploration
vioplot(polrModel$resample$GainPerc, names = 'Gain [%]', col = 'green')
title('Violin Plot of Gain Percentage in resamples')
summary(polrModel$resample$GainPerc)

### GBM Model ###
gbmGrid <- expand.grid(.interaction.depth = c(1, 2, 3, 4),
                       .n.trees = seq(50, 1000, by = 200), 
                       .shrinkage = c(.05), 
                       .n.minobsinnode = c(10))
set.seed(seed)
gbmModel <- train(form = resultFormula, data = filteredFeatureMatches, method = 'gbm',
                  trControl = customCvContr, verbose = FALSE, 
                  tuneGrid = gbmGrid, distribution = 'multinomial',
                  metric = 'GainPerc')
# Best Tune
gbmModel$results[as.integer(rownames(gbmModel$results)) == as.integer(rownames(gbmModel$bestTune)), ]
# Plotting the resampling profile
trellis.par.set(caretTheme())
plot(gbmModel)

### Extreme Gradient Boosting ###
extrBoostGrid <- expand.grid(nrounds = (1:10)*100,
                             eta = c(.05, .075, .1),
                             max_depth = 1:4)
set.seed(seed)
extrBoostModel <- train(form = resultFormula, data = filteredFeatureMatches, method = 'xgbTree',
                        trControl = customCvContr, tuneGrid = extrBoostGrid, metric = 'GainPerc',
                        objective = 'multi:softprob', num_class = 3,
                        colsample_bytree = 1, min_child_weight = 1)
trellis.par.set(caretTheme())
plot(extrBoostModel)
