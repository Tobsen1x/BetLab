# Preparation #
profileId <- 21
props <- readInterfaceProperties()
tuningProfile <- findTuningProfile(profileId)
tuneGrid <- extractXGBoostGrid(tuningProfile)
loginfo('Tune Grid:')
loginfo(paste(names(tuneGrid), tuneGrid))
featureConfig <- extractFeatureConfig(tuningProfile)
loginfo('Feature Config:')
loginfo(paste(names(featureConfig), featureConfig))

# Loading rawdata #
allSeasons <- getAllSeasons(firstSeason = props$data.firstSeason, lastSeason = featureConfig$season)
data <- loadTrainingData(toMatchday = featureConfig$matchday, seasons = allSeasons, 
                         leagues = featureConfig$league, fitPriceImpute = props$data.fitPriceImpute)
odds <- data$odds
stats <- data$stats
matches <- data$matches

fileName <- 'C:/RStudioWorkspace/BetLab/data/featuredMatches/HOMEVISITORS_BL12014-2015_34_122_SBI-0.25_SNPI-0.4_PD60.Rds'
featuredMatches <- readRDS(file = fileName)

modelInput <- reduceFeatures(featuredMatches)
# Add booky odds
modelInput <- merge(select(odds, matchId, HomeOdd = HomeVictory, VisitorsOdd = VisitorsVictory, 
                           DrawOdd = Draw), modelInput, by = c('matchId'), all.y = TRUE, sort = FALSE)

loginfo(paste(nrow(tuneGrid), 'Parameter constellations to work...', sep = ' '))

customCvContr <- trainControl(method = 'cv' , number = as.integer(unique(tuningProfile$cvNumber)))

tuneResults <- tuneXGBoost(data = modelInput, trControl = customCvContr, tuneGrid = tuneGrid, seed = 16450)
tuningResults <- tuneResults 
featureId <- featureConfig$id
calcTime <- 30
profileId <- 1
minPercProfit <- -100
cvNumber <- 5
cvRepeats <- 1

insertTuningResults(tuningResults, featureId, calcTime, profileId, minPercProfit, cvNumber, cvRepeats)

# Plot
xgbParas <- select(tuneResults, nrounds, eta, max_depth, gamma, colsample_bytree, min_child_weight,
                   lambda, alpha, subsample, scale_pos_weight)

used <- apply(xgbParas, 2, FUN = function(x) {
  return(length(unique(x)) > 1)
})

usedParaName <- names(used[used])

# cv bet metrics
res <- gather(select(tuneResults, valueDiffPerc, profitPerc, matches(names(used[used]), ignore.case = TRUE)), 
              metric, value, valueDiffPerc, profitPerc)
betMetricPlot <- ggplot(res, aes(x = res[, usedParaName],
                y = value,
                color = metric)) + geom_line() + xlab(usedParaName)

# accuracy
accRes <- gather(select(tuneResults, accuracy, trainAccuracy, bookyAccuracy, matches(names(used[used]), ignore.case = TRUE)), 
                 metric, value, accuracy, trainAccuracy, bookyAccuracy)
accPlot <- ggplot(accRes, aes(x = accRes[, usedParaName],
                y = value,
                color = metric)) + geom_line() + xlab(usedParaName)

grid.arrange(betMetricPlot, accPlot, ncol = 1)

