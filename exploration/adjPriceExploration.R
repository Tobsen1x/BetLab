data <- loadTrainingData(toMatchday = 34, seasons = getAllSeasons('2005-2006', '2015-2016'), 
                         leagues = 'BL1', fitPriceImpute = 50000)
odds <- data$odds
stats <- data$stats

assignedPositions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 
                       'mid', 'off', 'off', 'off', 'off', 'off')
groupedPos <- groupPositions(stats$position, assignedPositions)
stats$groupedPosition <- groupedPos
stats$matchtime <- as.Date(stats$matchtime)
stats <- data.table(stats)
setkey(stats, playerId, matchId, matchtime)

matches <- data$matches
matches <- data.table(matches)
setkey(matches, matchId, season, matchday)

prices <- data$prices
prices <- filter(prices, informationDate > as.Date('2005-03-01', format = '%Y-%m-%d'))
prices <- arrange(prices, playerId, informationDate)
prices <- data.table(prices)
setkey(prices, playerId)

priceRangeData <- extractAdjPriceFeatures(prices, stats, matches)

saveRDS(priceRangeData, file = 'C:/RStudioWorkspace/BetLab/data/adjPrice/adjPrice2005-2016.Rds')
priceRangeData <- readRDS(file = 'C:/RStudioWorkspace/BetLab/data/adjPrice/adjPrice2005-2016.Rds')

describe(priceRangeData)
names(which.max(table(stats$groupedPosition)))[1]

#model fitting
cvControl <- trainControl(method = 'cv', number = 5)

gradeData <- filter(priceRangeData, !is.na(meanGrade), !is.na(primaryPosition))
gradeData <- select(gradeData, priceChangePerWeek, startPrice, meanGrade, age, #primaryPosition,
                    completedPerWeek, benchPerWeek, notPlayedPerWeek, swappedInPerWeek, swappedOutPerWeek)
set.seed(16450)
lmModel <- train(x = gradeData[, -1], y = gradeData$priceChangePerWeek,
                 method = 'lm', trControl = cvControl)
summary(lmModel)

rfGrid <- expand.grid(mtry = c(5))
set.seed(16450)
rfModel <- train(x = gradeData[, -1], y = gradeData$priceChangePerWeek,
                 method = 'rf', trControl = cvControl, tuneGrid = rfGrid, 
                 ntree = 1000, importance = FALSE)
rfModel
saveRDS(rfModel, 'C:/RStudioWorkspace/BetLab/data/adjPrice/rfModel2005-2016.Rds')
rfModel <- readRDS('C:/RStudioWorkspace/BetLab/data/adjPrice/rfModel2005-2016.Rds')
summary(rfModel)
varImp(rfModel)
getTrainPerf(rfModel)


numericData <- select(gradeData, -primaryPosition)

gbmGrid <- expand.grid(n.trees = (1:3) * 200, 
                       interaction.depth = c(5,6,7,8), 
                       shrinkage = .05, 
                       n.minobsinnode = c(20))
gbmModel <- train(x = numericData[, -1], y = numericData$priceChangePerWeek,
                  method = 'gbm', trControl = cvControl, tuneGrid = gbmGrid,
                  verbose = FALSE)
gbmModel
getTrainPerf(gbmModel)
plot(gbmModel, metric = 'Rsquared')


### Models without grades

naGradeData <- filter(priceRangeData, is.na(meanGrade))
naGradeData <- select(naGradeData, priceChangePerWeek, startPrice, age,
                      benchPerWeek, notPlayedPerWeek)
describe(naGradeData)
set.seed(16450)
naLmModel <- train(x = naGradeData[, -1], y = naGradeData$priceChangePerWeek,
                 method = 'lm', trControl = cvControl)
summary(naLmModel)

rfGrid <- expand.grid(mtry = c(5))
set.seed(16450)
naRfModel <- train(x = naGradeData[, -1], y = naGradeData$priceChangePerWeek,
                 method = 'rf', trControl = cvControl, tuneGrid = rfGrid, 
                 ntree = 1000, importance = FALSE)
naRfModel
saveRDS(naRfModel, 'C:/RStudioWorkspace/BetLab/data/adjPrice/naRfModel2005-2016.Rds')
naRfModel <- readRDS('C:/RStudioWorkspace/BetLab/data/adjPrice/naRfModel2005-2016.Rds')
summary(naRfModel)

### Predicting ###

byMatchday <- group_by(matches, season, matchday)
matchDates <- summarise(byMatchday, 
                        matchdayTime = as.Date(max(matchtime)))
matchDates <- data.table(matchDates)
setkey(matchDates, matchdayTime)

predData <- data.table('playerId' = stats$playerId, 'matchId' = stats$matchId, 'startDate' = stats$fitPriceDate,
                       'endDate' = as.Date(stats$matchtime), 'startPrice' = stats$fitPrice, 'birthday' = stats$birthday)

result <- data.frame()
for(index in 1:nrow(predData)) {
  actPred <- predData[index,]
  relMatchdays <- filter(matchDates, matchdayTime > actPred$startDate, matchdayTime < actPred$endDate)
  
  playerStats <- stats[playerId == actPred$playerId]
  relPlayerStats <- filter(playerStats, matchtime > actResult$startDate, matchtime < actResult$endDate)
  actResult <- cbind(actResult, data.frame('matchdayCount' = nrow(relMatchdays),
                                         'statsCount' = nrow(relPlayerStats),
                                         'completedMatches' = nrow(relPlayerStats[playerAssignment == 'DURCHGESPIELT']),
                                         'benchMatches' = nrow(relPlayerStats[playerAssignment == 'BENCH']),
                                         'notPlayedMatches' = nrow(relMatchdays) - nrow(relPlayerStats),
                                         'swappedInMatches' = nrow(relPlayerStats[playerAssignment == 'EINGEWECHSELT']),
                                         'swappedOutMatches' = nrow(relPlayerStats[playerAssignment == 'AUSGEWECHSELT']),
                                         'meanGrade' = mean(relPlayerStats$grade, na.rm = TRUE),
                                         'medianGrade' = median(relPlayerStats$grade, na.rm = TRUE)))
  
  actResult <- mutate(actResult, 
                     age = round(as.numeric(difftime(actResult$endDate, actResult$birthday, units = 'weeks'), units='weeks') / 52, digits = 0),
                     weekDiff = ceiling(as.numeric(difftime(actResult$endDate, actResult$startDate, units = 'weeks'), units='weeks')),
                     completedPerWeek = completedMatches / weekDiff,
                     benchPerWeek = benchMatches / weekDiff,
                     notPlayedPerWeek = notPlayedMatches / weekDiff,
                     swappedInPerWeek = swappedInMatches / weekDiff,
                     swappedOutPerWeek = swappedOutMatches / weekDiff,
                     statsPerWeek = statsCount / weekDiff
  )
  result <- rbind(result, actResult)
}

saveRDS(result, 'C:/RStudioWorkspace/BetLab/data/adjPrice/predFeatures2005-2016.Rds')

result <- readRDS('C:/RStudioWorkspace/BetLab/data/adjPrice/predFeatures2005-2016.Rds')
describe(result)

fResult <- filter(result, !is.na(meanGrade))
fResult <- cbind(fResult, 'predPriceChangePerWeek' = predict(rfModel, fResult))
naResult <- filter(result, is.na(meanGrade))
naResult <- cbind(naResult, 'predPriceChangePerWeek' = 0)
allPreds <- rbind(fResult, naResult)
describe(enrStats$priceChange)

enrStats <- merge(stats, select(allPreds, matchId, playerId, predPriceChangePerWeek, weekDiff), by = c('matchId', 'playerId'))
enrStats <- mutate(enrStats, priceChange = predPriceChangePerWeek * weekDiff)
enrStats <- mutate(enrStats, fitPrice = fitPrice + priceChange)
saveRDS(enrStats, 'C:/RStudioWorkspace/BetLab/data/adjPrice/enrichedStats2005-2016.Rds')

## Feature Engineering

source(file = 'featureEngineering/positionFeatureExtraction.R', echo = FALSE, encoding = 'UTF-8')
assignedPositions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 
                       'mid', 'off', 'off', 'off', 'off', 'off')
relNormalAssignments <- c('DURCHGESPIELT', 'AUSGEWECHSELT')
priceFuncts <- c('min', 'max', 'avg', 'sum')
benchPriceFuncts <- c('max', 'avg')
args <- list('featuredMatches.staticPriceImpute' = 50000, 'featuredMatches.staticFormImpute' = 0)
featuredMatches <- extractMatchResultFeatures(as.data.frame(enrStats), as.data.frame(matches), assignedPositions, relNormalAssignments,
                                              priceFuncts = priceFuncts, benchPriceFuncts = benchPriceFuncts, args = args)

#### Model Tuning ####
modelInput <- reduceFeatures(featuredMatches)
# Add booky odds
modelInput <- merge(select(odds, matchId, HomeOdd = HomeVictory, VisitorsOdd = VisitorsVictory, 
                           DrawOdd = Draw), modelInput, by = c('matchId'), all.y = TRUE, sort = FALSE)

customCvContr <- trainControl(method = 'cv' , number = 10)
tuneGrid <- expand.grid(nrounds = 1:5 * 50,
                        max_depth = c(3,4,5),
                        eta = c(0.1, 0.2),
                        gamma = 0,
                        min_child_weight = 1,
                        colsample_bytree = .9,
                        subsample = 1,
                        scale_pos_weight = 1,
                        lambda = 1,
                        alpha = 0)
loginfo(paste(nrow(tuneGrid), 'Parameter constellations to work...', sep = ' '))

beforeTime <- Sys.time()
tuningResult <- tuneModelWrapped(modelInput = modelInput, trControl = customCvContr, 
                                 tuneGrid = tuneGrid, seed = 16450, featuresToInclude = 'price')

afterTime <- Sys.time()
calcTime <- round(difftime(afterTime, beforeTime, units = c('mins')))


