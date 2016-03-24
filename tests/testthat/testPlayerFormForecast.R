context("Player form forecast")
library(dplyr)
data <- readRDS(file = paste(Sys.getenv('DATA_PATH'), 'BL1_2005-2015.Rds', sep = ''))
stats <- data$stats
matches <- data$matches

source(file = paste(Sys.getenv('PROJECT_PATH'), 'featureEngineering/playerFormForecast.R', sep = ''), 
       echo = FALSE, encoding = 'UTF-8')
# Extract data for Ribery 2014-2015 [18]
relPlayerId <- 549
relSeason <- '2014-2015'
relStats <- filter(stats, season == relSeason, playerId == relPlayerId)
relMatchday <- 18
riberyForm <- calculatePlayerForm(stats = relStats, season = relSeason, 
                            matchday = relMatchday, playerId = relPlayerId)

test_that("Form forecast is valid", {
  expect_equal(riberyForm, 2.75)
})