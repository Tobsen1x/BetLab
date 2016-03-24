### Script executes model tuning with parameters from tuning.properties
### Saves results as plots and txt

cat("Loading necessary packages...\n")
library(ggplot2)
library(vioplot)
library(corrplot)
library(polycor)
library(RMySQL)
library(caret)
library(tidyr)
library(properties)
library(e1071)
library(pROC)
library(gridExtra)
library(magrittr)
library(MASS)
library(gbm)
library(bnclassify)
library(C50)
library(kernlab)
library(xgboost)
library(testthat)
library(Hmisc)
library(plyr)
library(dplyr)
cat("Packages loaded.\n")

# Logging
library(logging)
basicConfig()

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

# getting parameters from properties file
props <- read.properties('C:/RStudioWorkspace/BetLab/models/tuning.properties', fields = NULL, encoding = "UTF-8")
tuneGrids <- extractGridsFromProperties(props)

source(file = 'C:/RStudioWorkspace/BetLab/models/models.R', echo = FALSE, encoding = 'UTF-8')
customCvContr <- trainControl(method = 'cv', number = 5, classProbs = TRUE, 
                              summaryFunction = betMetricsSummary)
resultFormula <- as.formula('matchResult ~ . -matchId -goalsHome -goalsVisitors -goalDiff')

# Loading input
# Staticly load just price featured data
featuredMatches <- readRDS(file = 'C:/RStudioWorkspace/BetLab/data/featuredMatches/priceFeatured.Rds')
modelInput <- selectModelInput(featuredMatches)
data <- readRDS(file = 'C:/RStudioWorkspace/BetLab/data/BL1_2005-2015.Rds')
odds <- data$odds
print(paste(length(tuneGrids), 'tuning steps to take...', sep = ' '))

for(tuneStep in 1:length(tuneGrids)) {
  actGrid <- tuneGrids[[tuneStep]]
  tuneInfo <- apply(actGrid, 2, unique)
  tuneInfo <- lapply(tuneInfo, FUN = function(x) {
    paste(x, collapse = ', ')
  })
  print('')
  print(paste(nrow(actGrid), 'Parameter constellations to work with...', sep = ' '))

  set.seed(props$seed)
  model <- train(form = resultFormula, data = modelInput, method = 'xgbTree',
                             trControl = customCvContr, tuneGrid = actGrid, metric = 'GainPerc',
                             objective = 'multi:softprob', num_class = 3, allowParallel = FALSE)

  ## Metrics of best model ##
  bestConfig <- model$results[as.integer(rownames(model$results)) == 
                              as.integer(rownames(model$bestTune)), ]
  
  now <- Sys.time()
  now <- format(now, '%Y-%m-%d_%H-%M')
  txtFileName <- paste(now, '_', tuneStep, '.txt', sep = '')
  pdfFileName <- paste(now, '_', tuneStep, '.jpeg', sep = '')
  txtFile <- paste('C:/RStudioWorkspace/BetLab/models/tuningResults/', txtFileName, sep = '')
  pdfFile <- paste('C:/RStudioWorkspace/BetLab/models/tuningResults/', pdfFileName, sep = '')
  
  sink(txtFile)
  print(now)
  print('')
  print('Tuning Parameters:')
  print(tuneInfo)
  print('')
  print('Best Profile:')
  print(bestConfig)
  sink()
  
  ggplot <- plot.train(model, output = 'ggplot')
  png(file = pdfFile, type = "windows")
  plot(ggplot)
  dev.off()
}
