context("Form Feature Exctraction")
data <- readRDS(file = paste(Sys.getenv('DATA_PATH'), 'BL1_2005-2015.Rds', sep = ''))
formEnrichedStats <- readRDS(file = paste(Sys.getenv('DATA_PATH'), 'formEnriched/122_SBI-0.25_SNPI-0.4_PD60.Rds', sep = ''))

# Engineer features
assignedPositions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 
                       'mid', 'off', 'off', 'off', 'off', 'off')
relNormalAssignments <- c('DURCHGESPIELT', 'AUSGEWECHSELT')
priceFuncts <- c('min', 'max', 'avg', 'sum')
formFuncts <- c('min', 'max', 'avg')
benchPriceFuncts <- c('max', 'avg')
benchFormFuncts <- c('min', 'avg')

relId <- 1671
relMatch <- filter(data$matches, matchId == relId)
relStats <- filter(formEnrichedStats, season == relMatch$season)

source(file = paste(Sys.getenv('PROJECT_PATH'), 'featureEngineering/positionFeatureExtraction.R', sep = ''), 
            echo = FALSE, encoding = 'UTF-8')
featureCols <- extractFeatureNames(assignedPositions, priceFuncts, formFuncts, 
                                   benchPriceFuncts, benchFormFuncts)
test_that('extractFeatureNames column count', {
  expColCount <- 4 * 2 * (length(priceFuncts) + length(formFuncts)) + 3 * 2 * (length(benchPriceFuncts) + length(benchFormFuncts))
  expect_equal(length(featureCols), expColCount)
})

featuredMatches <- extractMatchResultFeatures(relStats, relMatch, assignedPositions, relNormalAssignments,
                                              priceFuncts, formFuncts, benchPriceFuncts, benchFormFuncts)

priceFeatures <- featuredMatches[, grep('_Price', colnames(featuredMatches))]
formFeatures <- featuredMatches[, grep('_Form', colnames(featuredMatches))]

test_that("Correct feature values", {
  expect_equal(sum(is.na(featuredMatches)), 0)
  expect_true(sum(priceFeatures < 0) == 0)
  expect_true(sum(formFeatures < -6.0) == 0)
  expect_true(sum(formFeatures > 6.0) == 0)
})

# TODO Test negative correlation between form and price features
