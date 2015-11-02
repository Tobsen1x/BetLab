### Load Data
source(file = 'loadData.R', echo = FALSE, encoding = 'UTF-8')
data <- loadData('BL1')
stats <- data$playerStats
matches <- data$matches
odds <- data$odds
players <- data$player
prices <- data$prices

source(file = 'adjustedPrice/enrichAdjustedPrice.R', echo = FALSE, encoding = 'UTF-8')
adjPriceData <- extractAdjPriceModelInput(prices = prices, player = players,
                                          playerStats = stats)
describe(adjPriceData)

adjStatsData <- extractAdjStatsModelInput(player = players,
                                          playerStats = stats)
filteredAdjStatsData <- filter(adjStatsData, !is.na(kickerGrade))
describe(filteredAdjStatsData)


### Model Fitting ###
debugSource(file = 'adjustedPrice/adjPriceModels.R', echo = FALSE, encoding = 'UTF-8')

enrichByAgeAdjFitPrice <- function(filteredAdjStatsData, seed) {
    
}

seed <-1234

# fill with age
formula <- 'fitPrice ~ kickerGrade + age'
formula <- as.formula(formula)
adjPriceModels <- fitModels(formula, train = filteredAdjStatsData, seed = seed)

exampleObs <- createStatsExampleObservations()
exampleObs$predPrice <- predict(adjPriceModels$lm, exampleObs)
summary(exampleObs)

gainPerYear <- summary(adjPriceModels$lm$finalModel)$coefficients['age', 'Estimate']
age <- seq(16, 44, by = 1)
priceLambda <- age * gainPerYear
modPrices <- data.frame(age = age, priceLambda = priceLambda)

refAge <- floor(mean(filteredAdjStatsData$age))
adjStatsData$ageAdjFitPrice <- NA
for(i in 1:nrow(adjStatsData)) {
    actStat <- adjStatsData[i,]
    actPredPriceLambda <- modPrices[modPrices$age == actStat$age, ]$priceLambda
    actPredRefPriceLambda <- modPrices[modPrices$age == refAge, 'priceLambda']
    adjStatsData$ageAdjFitPrice[i] <- actStat$fitPrice + actPredRefPriceLambda - actPredPriceLambda
}

describe(select(adjStatsData, fitPrice, ageAdjFitPrice))
adjStatsData$ageAdjFitPrice[adjStatsData$ageAdjFitPrice < 0] <- 0
################

exampleObs <- createExampleObservations()
predList <- predictPrice(adjPriceModels, exampleObs, seed = seed)
predDataFrame <- do.call(cbind.data.frame, predList)
exampleObs <- cbind(exampleObs, predDataFrame)
