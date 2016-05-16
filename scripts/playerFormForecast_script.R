### Script executes model tuning with parameters from tuning.properties
### Saves results as plots and txt

cat("Loading necessary packages...\n")
suppressMessages(library(ggplot2))
suppressMessages(library(vioplot))
suppressMessages(library(corrplot))
suppressMessages(library(polycor))
suppressMessages(library(RMySQL))
suppressMessages(library(caret))
suppressMessages(library(tidyr))
suppressMessages(library(properties))
suppressMessages(library(e1071))
suppressMessages(library(pROC))
suppressMessages(library(gridExtra))
suppressMessages(library(magrittr))
suppressMessages(library(MASS))
suppressMessages(library(gbm))
suppressMessages(library(bnclassify))
suppressMessages(library(C50))
suppressMessages(library(kernlab))
suppressMessages(library(xgboost))
suppressMessages(library(testthat))
suppressMessages(library(Hmisc))
suppressMessages(library(data.table))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
cat("Packages loaded.\n\n")

# Logging
suppressMessages(library(logging))
basicConfig()
source(file = 'C:/RStudioWorkspace/BetLab/scripts/scriptUtils.R', echo = FALSE, encoding = 'UTF-8')

data <- readRDS(file="C:/RStudioWorkspace/BetLab/data/BL1_2005-2015.Rds")
stats <- data$stats
matches <- data$matches

### Extract config
props <- read.properties('C:/RStudioWorkspace/BetLab/scripts/formForecastConfig.properties', fields = NULL, encoding = "UTF-8")
props <- typeProperties(props)
# Print Properties #
t <- lapply(names(props), FUN = function(x, props){
  print(paste(x, '=', props[[x]]))
}, props = props)

loginfo('Calculate Form Forecast...')

### Execute form forecast ###
source(file = 'C:/RStudioWorkspace/BetLab/featureEngineering/playerFormForecast.R', echo = FALSE, encoding = 'UTF-8')
formEnrichedStats <- fillAllPlayerForm(stats = stats, matches =  matches, args = props)
formEnrichedFileName <- extractFormFileName(props)
formFileName <- paste('C:/RStudioWorkspace/BetLab/data/formEnriched/', formEnrichedFileName, 'Rds', sep = '')
saveRDS(formEnrichedStats, file = formFileName)
loginfo('Saved Form Enriched File:', formFileName)

### Execute feature engineering ###
#source(file = 'C:/RStudioWorkspace/BetLab/featureEngineering/positionFeatureExtraction.R', echo = FALSE, encoding = 'UTF-8')
#assignedPositions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 
#                       'mid', 'off', 'off', 'off', 'off', 'off')
#relNormalAssignments <- c('DURCHGESPIELT', 'AUSGEWECHSELT')
#priceFuncts <- c('min', 'max', 'avg', 'sum')
#benchPriceFuncts <- c('max', 'avg')
#formFuncts <- c('min', 'max', 'avg')
#benchFormFuncts <- c('max', 'avg')
#loginfo('Engineere features...')
#featuredMatches <- extractMatchResultFeatures(formEnrichedStats, matches, assignedPositions, relNormalAssignments,
#                                              priceFuncts = priceFuncts, formFuncts = formFuncts, 
#                                              benchPriceFuncts = benchPriceFuncts, benchFormFuncts = benchFormFuncts)
#if(props$features == 'interactions') {
#  featuredMatches <- extractInteractionFeatures(featuredMatches)
#  featureFileName <- paste('interact_', formEnrichedFileName, sep = '')
#} else if(props$features == 'homeVisitors') {
#  featureFileName <- paste('homeVis_', formEnrichedFileName, sep = '')
#}

#featureFileName <- paste('C:/RStudioWorkspace/BetLab/data/featuredMatches/', featureFileName, sep = '')
#saveRDS(featuredMatches, file = featureFileName)
#loginfo('Saved Feature File:', featureFileName)
