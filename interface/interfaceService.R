executeTuning <- function(profileId, seed = 16450) {
  logfilename <- paste('tuning_', profileId, '.log', sep = '')
  addHandler(writeToFile, file = paste('C:/Betlab_Java/soccer.gui/logs/', logfilename, sep = ''))
  source(file = 'C:/RStudioWorkspace/BetLab/dataProvider/tuningProfileDao.R', 
         echo = FALSE, encoding = 'UTF-8')
  source(file = 'C:/RStudioWorkspace/BetLab/scripts/scriptUtils.R', 
         echo = FALSE, encoding = 'UTF-8')
  tuningProfile <- findTuningProfile(profileId)
  tuneGrid <- extractXGBoostGrid(tuningProfile)
  
  # Loading rawdata #
  data <- readRDS(file = paste('C:/RStudioWorkspace/BetLab/data/', unique(tuningProfile$rawDataFileName) , '.Rds', sep = ''))
  odds <- data$odds
  stats <- data$stats
  matches <- data$matches
  
  featureConfig <- extractFeatureConfig(tuningProfile)
  loginfo('Feature Config:')
  loginfo(paste(names(featureConfig), featureConfig))
  beforeTime <- Sys.time()
  # Loading featured matches
  source(file = 'C:/RStudioWorkspace/BetLab/scripts/scriptUtils.R', 
         echo = FALSE, encoding = 'UTF-8')
  featuredMatchesFileName <- extractFeaturedMatchesFileName(featureConfig)
  fileName <- paste('C:/RStudioWorkspace/BetLab/data/featuredMatches/',  featuredMatchesFileName, '.Rds', sep = '')
  if(file.exists(fileName)) {
    loginfo(paste(fileName, 'exists. Loading it...'))
    featuredMatches <- readRDS(file = fileName)
  } else { 
    loginfo(paste(fileName, 'not existing. Will be created...'))
    
    # Load Form Enriched stats #
    formEnrichedFileName <- extractFormFileName(featureConfig)
    formFileName <- paste('C:/RStudioWorkspace/BetLab/data/formEnriched/', formEnrichedFileName, '.Rds', sep = '')
    if(file.exists(formFileName)) {
      loginfo(paste(formFileName, 'will be loaded...'))
      formEnrichedStats <- readRDS(file = formFileName)
    } else {
      loginfo(paste(formFileName, 'does not exist. Will be created...'))
      # Featured Matches have to be engineered
      source(file = 'C:/RStudioWorkspace/BetLab/featureEngineering/playerFormForecast.R', 
             echo = FALSE, encoding = 'UTF-8')
      formEnrichedStats <- fillAllPlayerForm(stats = stats, matches =  matches, args = featureConfig)
      saveRDS(formEnrichedStats, file = formFileName)
      loginfo('Saved Form Enriched File:', formFileName)
    }
    
    ### Execute feature engineering ###
    source(file = 'C:/RStudioWorkspace/BetLab/featureEngineering/positionFeatureExtraction.R', 
           echo = FALSE, encoding = 'UTF-8')
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
    
    if(featureConfig$featureSelection == 'INTERACTIONS') {
      featuredMatches <- extractInteractionFeatures(featuredMatches)
    }
    saveRDS(featuredMatches, file = fileName)
    loginfo('Saved Feature File:', fileName)
  }
  
  #### Model Tuning ####
  source(file = 'C:/RStudioWorkspace/BetLab/models/models.R', echo = FALSE, encoding = 'UTF-8')
  
  modelInput <- selectModelInput(featuredMatches)
  modelInput <- reduceFeatures(modelInput)
  idMapping <- data.frame(row = rownames(modelInput), matchId = modelInput$matchId)
  
  loginfo(paste(nrow(tuneGrid), 'Parameter constellations to work...', sep = ' '))
  loginfo(paste(names(tuneGrid), tuneGrid))
  
  customCvContr <- trainControl(method = 'cv', number = as.integer(unique(tuningProfile$cv)), classProbs = TRUE, 
                                summaryFunction = betMetricsSummary)
  resultFormula <- as.formula('matchResult ~ . -matchId -goalsHome -goalsVisitors -goalDiff')
  
  model <- tuneModelWrapped(modelInput = modelInput, odds = odds, idMapping = idMapping,
                            resultFormula = resultFormula, trControl = customCvContr, 
                            tuneGrid = tuneGrid, seed = seed)
  
  ## Metrics and Tune of best model ##
  bestConfig <- model$results[as.integer(rownames(model$results)) == 
                                as.integer(rownames(model$bestTune)), ]
  
  afterTime <- Sys.time()
  calcTime <- round(difftime(afterTime, beforeTime, units = c('mins')))
  
  # Saving Results #
  tuneInstanceId <- insertTuningResults(bestConfig, calcTime, profileId)
  loginfo(paste('Tuningprofile', profileId, 'updated.'))
  
  jpegFileName <- extractTuningProfileFileName(featureConfig, profileId)
  jpegFile <- paste('C:/RStudioWorkspace/BetLab/models/tuningResults/', jpegFileName, '.jpg', sep = '')
  
  ggplot <- plot.train(model, output = 'ggplot')
  png(file = jpegFile, type = "windows")
  plot(ggplot)
  dev.off()
  loginfo(paste(jpegFileName, 'created.'))
  loginfo(paste('Tuning finisished for TuningProfile', profileId))
}