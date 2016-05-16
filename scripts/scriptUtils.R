extractFeatureConfig <- function(grid) {
  config <- list('version' = unique(grid$version), 'staticBenchImpute' = unique(grid$staticBenchImpute),
                 'staticNotPlayedImpute' = unique(grid$staticNotPlayedImpute), 'pastDays' = unique(grid$pastDays),
                 'lastFormImpute' = unique(grid$lastFormImpute), 'featureSelection' = unique(grid$featureSelection))
  return(config)
}

extractXGBoostGrid <- function(profileData) {
  grid <- expand.grid(nrounds = unique(as.integer(profileData$nrounds)),
                      max_depth = unique(as.integer(profileData$maxDepth)),
                      eta = unique(as.numeric(profileData$eta)),
                      gamma = unique(as.numeric(profileData$gamma)),
                      colsample_bytree = unique(as.numeric(profileData$colsample)),
                      min_child_weight = unique(as.integer(profileData$minChildWeight)))
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
  fileName <- as.character(props$version)
  if(length(props$staticBenchImpute) == 1) {
    fileName <- paste(fileName, '_SBI', props$staticBenchImpute, sep = '')
  }
  if(length(props$staticNotPlayedImpute) == 1) {
    fileName <- paste(fileName, '_SNPI', props$staticNotPlayedImpute, sep = '')
  }
  if(length(props$pastDays) == 1) {
    fileName <- paste(fileName, '_PD', props$pastDays, sep = '')
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

typeProperties <- function(props) {
  if(length(props$seed) > 0) {
    props$seed <- as.integer(props$seed)
  }
  if(length(props$staticBenchImpute) > 0) {
    props$staticBenchImpute <- as.numeric(props$staticBenchImpute)
  }
  if(length(props$staticNotPlayedImpute) > 0) {
    props$staticNotPlayedImpute <- as.numeric(props$staticNotPlayedImpute)
  }
  if(length(props$pastDays) > 0) {
    props$pastDays <- as.integer(props$pastDays)
  }
  if(length(props$lastFormImpute) > 0) {
    props$lastFormImpute <- as.numeric(props$lastFormImpute)
  }
  if(length(props$cv) > 0) {
    props$cv <- as.integer(props$cv)
  }
  return(props)
}

initScripting <- function() {
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
  loginfo('Logging initialized.')
}