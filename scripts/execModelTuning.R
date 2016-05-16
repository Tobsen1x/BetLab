### Script executes model tuning with parameters from tuning.properties
### Saves results as plots and txt
source(file = 'C:/RStudioWorkspace/BetLab/scripts/scriptUtils.R', echo = FALSE, encoding = 'UTF-8')
initScripting()

envArgs = commandArgs(trailingOnly=TRUE)
if(length(envArgs) == 0) {
  propsFileName <- 'C:/RStudioWorkspace/BetLab/models/tuning.properties'
} else {
  propsFileName <- paste('C:/RStudioWorkspace/BetLab/models/', envArgs[1], sep = '')
}
# getting parameters from properties file
props <- read.properties(propsFileName, fields = NULL, encoding = "UTF-8")
tuneGrids <- extractGridsFromProperties(props)
props <- typeProperties(props)

# Loading rawdata #
data <- readRDS(file = 'C:/RStudioWorkspace/BetLab/data/BL1_2005-2015.Rds')
odds <- data$odds

# Loading featured matches
featuredMatchesFileName <- extractFeaturedMatchesFileName(props)
fileName <- paste('C:/RStudioWorkspace/BetLab/data/featuredMatches/',  featuredMatchesFileName, '.Rds', sep = '')
if(file.exists(fileName)) {
  featuredMatches <- readRDS(file = fileName)
} else { 
  loginfo(paste(fileName, 'not existing. Will be created...'))
  stats <- data$stats
  matches <- data$matches
  # Featured Matches have to be engineered
  source(file = 'C:/RStudioWorkspace/BetLab/featureEngineering/playerFormForecast.R', echo = FALSE, encoding = 'UTF-8')
  formEnrichedStats <- fillAllPlayerForm(stats = stats, matches =  matches, version = props$version, args = props)
  formEnrichedFileName <- extractFormFileName(props)
  formFileName <- paste('C:/RStudioWorkspace/BetLab/data/formEnriched/', formEnrichedFileName, '.Rds', sep = '')
  saveRDS(formEnrichedStats, file = formFileName)
  loginfo('Saved Form Enriched File:', formFileName)
  
  ### Execute feature engineering ###
  source(file = 'C:/RStudioWorkspace/BetLab/featureEngineering/positionFeatureExtraction.R', echo = FALSE, encoding = 'UTF-8')
  assignedPositions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 
                         'mid', 'off', 'off', 'off', 'off', 'off')
  relNormalAssignments <- c('DURCHGESPIELT', 'AUSGEWECHSELT')
  priceFuncts <- c('min', 'max', 'avg', 'sum')
  benchPriceFuncts <- c('max', 'avg')
  formFuncts <- c('min', 'max', 'avg')
  benchFormFuncts <- c('max', 'avg')
  loginfo('Engineere features...')
  featuredMatches <- extractMatchResultFeatures(formEnrichedStats, matches, assignedPositions, relNormalAssignments,
                                                priceFuncts = priceFuncts, formFuncts = formFuncts, 
                                                benchPriceFuncts = benchPriceFuncts, benchFormFuncts = benchFormFuncts)
  featureFileName <- paste('homeVis_', formEnrichedFileName, '.Rds', sep = '')
  saveRDS(featuredMatches, file = paste('C:/RStudioWorkspace/BetLab/data/featuredMatches/', 
                                        featureFileName, sep = ''))
  loginfo('Saved Feature File:', featureFileName)
  
  interactFeatureFileName <- paste('inter_', formEnrichedFileName, '.Rds', sep = '')
  interFeaturedMatches <- extractInteractionFeatures(featuredMatches)
  saveRDS(interFeaturedMatches, file = paste('C:/RStudioWorkspace/BetLab/data/featuredMatches/', 
                                             interactFeatureFileName, sep = ''))
  loginfo('Saved interaction Feature File:', interactFeatureFileName)
  
  
  if(props$features == 'inter') {
    featuredMatches <- interFeaturedMatches
  } else if(props$features == 'homeVis') {
    featuredMatches <- featuredMatches
  } else {
    stop()
  }
}

#### Model Tuning ####
source(file = 'C:/RStudioWorkspace/BetLab/models/models.R', echo = FALSE, encoding = 'UTF-8')
customCvContr <- trainControl(method = 'cv', number = as.integer(props$cv), classProbs = TRUE, 
                              summaryFunction = betMetricsSummary)
resultFormula <- as.formula('matchResult ~ . -matchId -goalsHome -goalsVisitors -goalDiff')

modelInput <- selectModelInput(featuredMatches)
modelInput <- reduceFeatures(modelInput)
idMapping <- data.frame(row = rownames(modelInput), matchId = modelInput$matchId)
writeLines(paste(length(tuneGrids), 'tuning steps to take...', sep = ' '))

for(tuneStep in 1:length(tuneGrids)) {
  actGrid <- tuneGrids[[tuneStep]]
  tuneInfo <- apply(actGrid, 2, unique)
  lapply(tuneInfo, FUN = function(x) {
    paste(x, collapse = ', ')
  })

  writeLines(paste(nrow(actGrid), 'Parameter constellations to work with...', sep = ' '))

  model <- tuneModelWrapped(modelInput = modelInput, odds = odds, idMapping = idMapping,
                            resultFormula = resultFormula, trControl = customCvContr, 
                            tuneGrid = actGrid, seed = props$seed)

  ## Metrics of best model ##
  bestConfig <- model$results[as.integer(rownames(model$results)) == 
                              as.integer(rownames(model$bestTune)), ]
  bestConfig
  now <- Sys.time()
  now <- format(now, '%Y-%m-%d')
  txtFileName <- paste(props$name, '_', now, '_', tuneStep, '.txt', sep = '')
  jpegFileName <- paste(props$name, '_', now, '_', tuneStep, '.jpeg', sep = '')
  txtFile <- paste('C:/RStudioWorkspace/BetLab/models/tuningResults/', txtFileName, sep = '')
  jpegFile <- paste('C:/RStudioWorkspace/BetLab/models/tuningResults/', jpegFileName, sep = '')
  writeLines(paste('Create', jpegFile, sep = ' '))
  
  sink(txtFile)
  writeLines(now)
  cat('\n')
  writeLines('Tuning Parameters:')
  print(tuneInfo)
  writeLines('Best Profile:')
  print(bestConfig)
  sink()
  
  ggplot <- plot.train(model, output = 'ggplot')
  png(file = jpegFile, type = "windows")
  plot(ggplot)
  dev.off()
}
