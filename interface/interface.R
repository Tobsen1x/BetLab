executeTuning <- function(profileId) {
  loginfo(paste('Tune Models for ProfileId:', profileId))
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
  
  beforeTime <- Sys.time()
  loginfo(paste('Data loaded. Start calculation:', beforeTime))
  # Loading featured matches
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
      loginfo('Fill adj Grade...')
      stats <- fillAdjGrade(stats, seed = props$seed, cvNumber = props$adjGrade.cv)
      
      #### Enrich with form data ####
      loginfo('Fill player Form...')
      formArgs <- append(featureConfig, list('weeksBeforeLastPriceDate' = props$form.weeksBeforeLastPriceDate))
      formEnrichedStats <- fillAllPlayerForm(stats = stats, matches =  matches, args = formArgs)
      saveRDS(formEnrichedStats, file = formFileName)
      loginfo('Saved Form Enriched File:', formFileName)
    }
    
    #### Execute feature engineering ####
    assignedPositions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 
                           'mid', 'off', 'off', 'off', 'off', 'off')
    relNormalAssignments <- c('DURCHGESPIELT', 'AUSGEWECHSELT')
    priceFuncts <- c('min', 'max', 'avg', 'sum')
    benchPriceFuncts <- c('max', 'avg')
    formFuncts <- c('min', 'max', 'avg')
    benchFormFuncts <- c('max', 'avg')
    loginfo('Engineere features...')
    featureArgs <- list('featuredMatches.staticPriceImpute' = props$featuredMatches.staticPriceImpute,
                        'featuredMatches.staticFormImpute' = props$featuredMatches.staticFormImpute)
    featuredMatches <- extractMatchResultFeatures(formEnrichedStats, matches, assignedPositions, relNormalAssignments,
                                                  priceFuncts = priceFuncts, formFuncts = formFuncts, 
                                                  benchPriceFuncts = benchPriceFuncts, benchFormFuncts = benchFormFuncts,
                                                  args = featureArgs)
    
    if(featureConfig$featureSelection == 'INTERACTIONS') {
      featuredMatches <- extractInteractionFeatures(featuredMatches, priceImpute = props$featuredInteract.priceImpute)
    }
    saveRDS(featuredMatches, file = fileName)
    loginfo(paste('Saved Feature File:', fileName))
  }
  
  #### Model Tuning ####
  modelInput <- reduceFeatures(featuredMatches)
  # Add booky odds
  modelInput <- merge(select(odds, matchId, HomeOdd = HomeVictory, VisitorsOdd = VisitorsVictory, 
                             DrawOdd = Draw), modelInput, by = c('matchId'), all.y = TRUE, sort = FALSE)
  
  loginfo(paste(nrow(tuneGrid), 'Parameter constellations to work...', sep = ' '))
  
  customCvContr <- trainControl(method = 'cv' , number = as.integer(unique(tuningProfile$cvNumber)))
  
  tuningResult <- tuneModelWrapped(modelInput = modelInput, trControl = customCvContr, 
                            tuneGrid = tuneGrid, seed = props$seed)
  
  afterTime <- Sys.time()
  calcTime <- round(difftime(afterTime, beforeTime, units = c('mins')))
  
  #### Saving Results ####
  tuneInstanceId <- insertTuningResults(tuningResults = tuningResult, featureId = featureConfig$id, 
                                        calcTime = as.integer(calcTime), profileId = profileId, 
                                        minPercProfit = props$tuning.minPercProfitToPersist,
                                        cvNumber = as.integer(unique(tuningProfile$cvNumber)),
                                        cvRepeats = as.integer(unique(tuningProfile$cvRepeats)))
  
  #loginfo(paste('Tuningprofile', profileId, 'updated.'))
  
  jpegFileName <- extractTuningProfileFileName(featureConfig, profileId)
  jpegFile <- paste('C:/RStudioWorkspace/BetLab/models/tuningResults/', jpegFileName, '.jpg', sep = '')
  
  if(shouldBePlotted(tuningResult)) {
    png(file = jpegFile, type = "windows")
      plotTuningProfile(tuningResult)
    dev.off()
  }
  
  loginfo(paste(jpegFileName, 'created.'))
  loginfo(paste('Tuning finisished for TuningProfile', profileId))
  loginfo('')
  loginfo('----------------------')
  loginfo('')
}

shouldBePlotted <- function(tuningResult) {
  xgbParas <- select(tuningResult, nrounds, eta, max_depth, gamma, colsample_bytree, min_child_weight,
                     lambda, alpha, subsample, scale_pos_weight)
  
  used <- apply(xgbParas, 2, FUN = function(x) {
    return(length(unique(x)) > 1)
  })
  
  if(sum(used) > 1) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}