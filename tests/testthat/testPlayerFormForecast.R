context("Player form forecast")
library(dplyr)
data <- readRDS(file = paste(Sys.getenv('DATA_PATH'), 'BL1_2005-2015.Rds', sep = ''))
stats <- data$stats
matches <- data$matches

source(file = paste(Sys.getenv('PROJECT_PATH'), 'featureEngineering/playerFormForecast.R', sep = ''), 
       echo = FALSE, encoding = 'UTF-8')

# Test linear decrease past match weights
testData <- c(0, 1, 2, 10, 20, 50, 70)
testWeights <- calculateLinearWeight(x = testData, maxPastDays = 40)
testWeights <- round(testWeights, 2)
test_that("linear weight calculation", {
  expected <- c(0.0, 1.0, 0.97, 0.77, 0.51, 0.0, 0.0)
  expect_equal(testWeights, expected)
})

# Test linear decrease form calculation
forecastData <- data.frame('matchTime' = c(as.POSIXct('2015-06-01 20:00:00'),
                                           as.POSIXct('2015-05-01 20:00:00'), 
                                           as.POSIXct('2015-04-12 15:30:00'), 
                                           as.POSIXct('2015-01-12 15:30:00')),
                           'purgedGrade' = c(0.5, 0.3, 0.0, -0.5))
form <- linearFormDecrease(x = forecastData, actMatchtime = as.POSIXct('2015-05-14 20:00:00'), 
                           maxPastDays = 40)
test_that("linear decrease calculation", {
  expected <- c(0.2314)
  expect_equal(round(form, 4), expected)
})

# Extract data for Ribery 2014-2015 [18]
relPlayerId <- 549
relSeason <- '2014-2015'
stats <- data.table(stats)
setkey(stats, matchId, playerId, season)
relMatchday <- 17
riberyStat <- filter(stats, season == relSeason, playerId == relPlayerId, matchday == relMatchday)

pastStats1 <- pastMatchSelection(relStats = stats, relSeason = riberyStat$season, 
                                relMatchday = riberyStat$matchday, 
                                relPlayerId = riberyStat$playerId, 
                                lastPriceDate = riberyStat$fitPriceDate,
                                version = 1)

pastStats2 <- pastMatchSelection(relStats = stats, relSeason = riberyStat$season, 
                                 relMatchday = riberyStat$matchday, 
                                 relPlayerId = riberyStat$playerId, 
                                 lastPriceDate = riberyStat$fitPriceDate,
                                 version = 2, weeksBeforeLastPriceDate = 1)

test_that("pastMatchSelection", {
  expect_equal(nrow(pastStats1), 10)
  expect_equal(nrow(pastStats2), 10)
})

impStats1 <- gradeImputation(pastStats = pastStats1, matches = matches, statToCalcFor = riberyStat, version = 1, args = list())
args <- list('staticBenchImpute' = -0.1, 'staticNotPlayedImpute' = -0.2, 'weeksBeforeLastPriceDate' = 1)
impStats2 <- gradeImputation(pastStats = pastStats1, matches = matches, statToCalcFor = riberyStat, version = 2, args = args)
playerForm1 <- applyForecastMethod(forecastData = impStats1, forecastMethod = linearFormDecrease, 
                                   forecastMethodArgs = 
                                     list('x' = impStats1, 'actMatchtime' = riberyStat$matchtime, 'maxPastDays' = 60))
playerForm2 <- applyForecastMethod(forecastData = impStats2, forecastMethod = linearFormDecrease, 
                                  forecastMethodArgs = 
                                    list('x' = impStats2, 'actMatchtime' = riberyStat$matchtime, 'maxPastDays' = 60))


riberyForm1 <- calculatePlayerForm(stats = stats, matches = matches, statToCalcFor = riberyStat,
                                  versions = c(1, 1, 1), args = list('weeksBeforeLastPriceDate' = 1))
args <- append(args, list('pastDays' = 60))
riberyForm2 <- calculatePlayerForm(stats = stats, matches = matches, statToCalcFor = riberyStat,
                                  versions = c(2, 2, 2), args = args)


test_that("Form forecast is valid", {
  expect_equal(round(riberyForm1, 2), 0.25)
  expect_equal(round(playerForm2, 3), round(riberyForm2, 3))
  expect_equal(round(playerForm2, 3), 0.151)
})