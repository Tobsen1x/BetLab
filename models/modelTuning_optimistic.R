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

# Integrate all participating players. That gives me an
# unrealistic information advantage, but we'll see...
lineupAssignments <- c('DURCHGESPIELT', 'AUSGEWECHSELT', 'EINGEWECHSELT')
featuredMatches <- extractMatchResultFeatures(playerStats = stats,
                                              matches = matches,
                                              priceAssignedPositions = positions,
                                              functs = c('min', 'max', 'avg', 'sum'), 
                                              lineupAssignments)

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

# Training Performance without resampling
testPred <- predict(polrModel, filteredFeatureMatches)
confMatrix <- confusionMatrix(testPred, reference = filteredFeatureMatches$matchResult)
confMatrix$overall[1:2]

# TODO Explore correlations of metrics like 
# GainPerc ~ I(Accuracy - BookyAccuracy) + I(Kappa - BookyKappa)
# GainPerc ~ ValueDiffPerc

### GBM Model ###
gbmGrid <- expand.grid(.interaction.depth = c(1, 2, 3),
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
jpeg("models/gbmTuningProfile2.jpg", width = 1300, height = 800)
plot(gbmModel)
dev.off()

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
jpeg("models/extrBoostingTuningProfile3.jpg", width = 1300, height = 800)
plot(extrBoostModel)
dev.off()

# Preprocess
y <- filteredFeatureMatches$matchResult
x <- filteredFeatureMatches[, 5:ncol(filteredFeatureMatches)]
logx <- log(x + 1)

summary(y)
summary(x)
summary(logx)

set.seed(seed)
extrBoostLogModel <- train(y = y, x = logx, method = 'xgbTree',
                        trControl = customCvContr, tuneGrid = extrBoostGrid, metric = 'GainPerc',
                        objective = 'multi:softprob', num_class = 3, 
                        colsample_bytree = 1, min_child_weight = 1)
plot(extrBoostLogModel)


### Support Vector Machines ###
set.seed(seed)
tanSearchModel <- train(form = resultFormula, data = filteredFeatureMatches, method = 'svmLinear2',
                        trControl = customCvContr, tuneLength = 3, metric = 'GainPerc', preProcess = c('center', 'scale'))

### C5.0 Model ###
### IS BAD
c50Grid <- expand.grid(model = c('rules', 'tree'),
                       winnow = c(TRUE, FALSE),
                       trials = 1:4)
set.seed(seed)
c50Model <- train(form = resultFormula, data = filteredFeatureMatches, method = 'C5.0',
                  trControl = customCvContr, tuneGrid = c50Grid, metric = 'GainPerc')
c50Model
