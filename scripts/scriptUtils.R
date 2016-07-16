readInterfaceProperties <- function() {
  props <- read.properties('C:/RStudioWorkspace/BetLab/interface/interfaceConfig.properties', 
                           fields = NULL, encoding = "UTF-8")
  props$tuning.minPercProfitToPersist <- as.numeric(props$tuning.minPercProfitToPersist)
  props$data.fitPriceImpute <- as.numeric(props$data.fitPriceImpute)
  props$seed <- as.integer(props$seed)
  props$adjGrade.cv <- as.integer(props$adjGrade.cv)
  props$form.weeksBeforeLastPriceDate <- as.integer(props$form.weeksBeforeLastPriceDate)
  props$featuredMatches.staticPriceImpute <- as.integer(props$featuredMatches.staticPriceImpute)
  props$featuredMatches.staticFormImpute <- as.numeric(props$featuredMatches.staticFormImpute)
  props$featuredInteract.priceImpute <- as.integer(props$featuredInteract.priceImpute)
  return(props)
}

getAllSeasons <- function(firstSeason, lastSeason) {
  allSeasons <- vector(mode = 'character')
  aktSeason <- firstSeason
  repeat {
    allSeasons <- append(allSeasons, aktSeason)
    # Increase season
    from <- as.integer(substr(aktSeason,1,4))
    to <- as.integer(substr(aktSeason,6,9))
    aktSeason <- paste(from+1, to+1, sep = '-')
    
    if(aktSeason == lastSeason) {
      allSeasons <- append(allSeasons, aktSeason)
      break
    }
  }
  return(allSeasons)
}

extractFeatureConfig <- function(grid) {
  config <- list('id' = unique(grid$featureId), 'version' = unique(grid$version), 'staticBenchImpute' = unique(grid$staticBenchImpute),
                 'staticNotPlayedImpute' = unique(grid$staticNotPlayedImpute), 'pastDays' = unique(grid$pastDays),
                 'lastFormImpute' = unique(grid$lastFormImpute), 'featureSelection' = unique(grid$featureSelection),
                 'league' = unique(grid$league), 'season' = unique(grid$season), 'matchday' = unique(grid$matchday))
  return(config)
}

extractXGBoostGrid <- function(profileData) {
  grid <- expand.grid(nrounds = unique(as.integer(profileData$nrounds)),
                      max_depth = unique(as.integer(profileData$maxDepth)),
                      eta = unique(as.numeric(profileData$eta)),
                      gamma = unique(as.numeric(profileData$gamma)),
                      colsample_bytree = unique(as.numeric(profileData$colsample)),
                      min_child_weight = unique(as.integer(profileData$minChildWeight)),
                      lambda = unique(as.numeric(profileData$lambda)),
                      alpha = unique(as.numeric(profileData$alpha)),
                      subsample = unique(as.numeric(profileData$subsample)),
                      scale_pos_weight = unique(as.numeric(profileData$scalePosWeight)))
  return(grid)
}

extractTuningProfileFileName <- function(props, profileId) {
  filename <- extractFeaturedMatchesFileName(props)
  filename <- paste(profileId, filename, sep = '_')
  return(filename)
}

extractFeaturedMatchesFileName <- function(props) {
  formName <- extractFormFileName(props)
  fileName <- paste(props$featureSelection, formName, sep = '_')
  return(fileName)
}

extractFormFileName <- function(props) {
  fileName <- as.character(props$league)
  fileName <- paste(fileName, as.character(props$season), sep = '')
  fileName <- paste(fileName, as.character(props$matchday), sep = '_')
  fileName <- paste(fileName, as.character(props$version), sep = '_')
  if(length(props$staticBenchImpute) == 1) {
    fileName <- paste(fileName, '_SBI', props$staticBenchImpute, sep = '')
  }
  if(length(props$staticNotPlayedImpute) == 1) {
    fileName <- paste(fileName, '_SNPI', props$staticNotPlayedImpute, sep = '')
  }
  if(length(props$pastDays) == 1) {
    fileName <- paste(fileName, '_PD', props$pastDays, sep = '')
  }
  if(length(props$lastFormImpute) == 1) {
    fileName <- paste(fileName, '_LFI', props$lastFormImpute, sep = '')
  }
  return(fileName)
}

extractGridsFromProperties <- function(props) {
  nroundLoops <- as.vector(strsplit(props$nround, '|', fixed = TRUE)[[1]], mode = 'character')
  maxDepthLoops <- as.vector(strsplit(props$max_depth, '|', fixed = TRUE)[[1]], mode = 'character')
  etaLoops <- as.vector(strsplit(props$eta, '|', fixed = TRUE)[[1]], mode = 'character')
  gammaLoops <- as.vector(strsplit(props$gamma, '|', fixed = TRUE)[[1]], mode = 'character')
  colsampleLoops <- as.vector(strsplit(props$colsample_bytree, '|', fixed = TRUE)[[1]], mode = 'character')
  minChildWeightLoops <- as.vector(strsplit(props$min_child_weight, '|', fixed = TRUE)[[1]], mode = 'character')
  
  allConfigs <- c()
  for(i in 1:length(nroundLoops)) {
    actNround <- nroundLoops[i]
    actMaxDepth <- maxDepthLoops[i]
    actEta <- etaLoops[i]
    actGamma <- gammaLoops[i]
    actColsample <- colsampleLoops[i]
    actMinChildWeight <- minChildWeightLoops[i]
    
    extrBoostGrid <- expand.grid(nrounds = as.vector(strsplit(actNround, ',')[[1]], mode = 'numeric'),
                                 max_depth = as.vector(strsplit(actMaxDepth, ',')[[1]], mode = 'numeric'),
                                 eta = as.vector(strsplit(actEta, ',')[[1]], mode = 'numeric'),
                                 gamma = as.vector(strsplit(actGamma, ',')[[1]], mode = 'numeric'),
                                 colsample_bytree = as.vector(strsplit(actColsample, ',')[[1]], mode = 'numeric'),
                                 min_child_weight = as.vector(strsplit(actMinChildWeight, ',')[[1]], mode = 'numeric'))
    allConfigs <- append(allConfigs, list(extrBoostGrid))
  }
  
  return(allConfigs)
}