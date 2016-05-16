data <- readRDS(file = 'data/BL1_2005-2015.Rds')
odds <- data$odds
### Load featured matches ###
#priceFeaturedMatches <- readRDS(file = 'data/featuredMatches/priceFeatured.Rds')
featuredMatches <- readRDS(file = 'data/featuredMatches/interact_222.Rds')

# Parallel processing #
#library(doSNOW)
#cl <- makeCluster(Sys.getenv('CORE_COUNT'))
#registerDoSNOW(cl)
#stopCluster(cl)
##

source(file = 'models/models.R', 
       echo = FALSE, encoding = 'UTF-8')
allModelInput <- selectModelInput(featuredMatches)
modelInput <- reduceFeatures(allModelInput)
idMapping <- data.frame(row = rownames(modelInput), matchId = modelInput$matchId)
seed <- 16450
customCvContr <- trainControl(method = 'cv', number = 10, classProbs = TRUE, 
                              summaryFunction = betMetricsSummary)

resultFormula <- as.formula('matchResult ~ . -matchId -goalsHome -goalsVisitors -goalDiff')

# For short calculations
applicationGrid <- expand.grid(nrounds = (1:3)*100,
                               max_depth = c(3, 4),
                               eta = c(.1, .3, .9),
                               gamma = c(0, .1),
                               colsample_bytree = 1,
                               min_child_weight = 1)

# For longer runs
extrBoostGrid <- expand.grid(nrounds = 50 + (1:3)*50,
                             max_depth = c(3),
                             eta = c(.25),
                             gamma = c(0),
                             colsample_bytree = 1,
                             min_child_weight = 1)

# Further reduce features
colsToReduce <- grepl('_avg_', colnames(modelInput)) & !grepl('tw_', colnames(modelInput))
withoutAvg <- modelInput[, !colsToReduce]
modelInput <- withoutAvg

# Simple model
simpleModel <- tuneModelWrapped(modelInput = modelInput, odds = odds, idMapping = idMapping,
                                      resultFormula = resultFormula, trControl = customCvContr, 
                                      tuneGrid = applicationGrid)
plot(simpleModel)

### Compare with prediction benchmarks ###

inputData <- data.frame(rowIndex = modelInput$matchId, obs = modelInput$matchResult)
rownames(inputData) <- modelInput$matchId
idMapping <- data.frame(row = rownames(inputData), matchId = modelInput$matchId)
homeVictorybenchmarkData <- fillByAllHomeBenchmark(inputData)
level <- c('VisitorsVictory', 'Draw', 'HomeVictory')
model <- 'HomeVictory Benchmark'
homeVictoryBenchmark <- betMetricsSummary(homeVictorybenchmarkData, level, model)
homeVictoryBenchmark

randomBenchmarkData <- fillByAllRandom(inputData)
model <- 'Random Benchmark'
randomBenchmark <- betMetricsSummary(randomBenchmarkData, level, model)
randomBenchmark



# log model Funzt nicht ?! ?! ?!
y <- modelInput$matchResult
x <- modelInput[, grepl('Price', colnames(modelInput))]
id <- modelInput$matchId
xLog <- log(x + 1)
logModelInput <- cbind('matchId' = id, y, xLog)
logFormula <- 'y ~ . -matchId'

logModel <- tuneModelWrapped(modelInput = logModelInput, idMapping = idMapping, odds = odds,
                             resultFormula = logFormula, trControl = customCvContr, 
                             tuneGrid = applicationGrid)

# PCA preprocessed model BAD
pcaModel <- tuneModelWrapped(modelInput = modelInput, idMapping = idMapping, odds = odds,
                             resultFormula = resultFormula, trControl = customCvContr, 
                             tuneGrid = applicationGrid, preProcess = 'pca')
plot(pcaModel)





### Just Price features ###
modelInput <- allModelInput[, !grepl('Form', colnames(allModelInput))]

# reduce further features
modelInput <- allModelInput[, !grepl('_sum_', colnames(allModelInput))]
modelInput <- modelInput[, !grepl('_avg_Price_Bench', colnames(modelInput))]
modelInput <- modelInput[, !grepl('_min_', colnames(modelInput))]
modelInput <- modelInput[, !grepl('_max_', colnames(modelInput))]



loginfo('Start tuning xgBoost model')
set.seed(seed)
priceXGBoostModel <- train(form = resultFormula, data = modelInput, method = 'xgbTree',
                        trControl = customCvContr, tuneGrid = extrBoostGrid, metric = 'GainPerc',
                        objective = 'multi:softprob', num_class = 3, allowParallel = TRUE)
loginfo('End tuning xgBoost model')
## Metrics of best model ##
priceXGBoostModel$results[as.integer(rownames(priceXGBoostModel$results)) == as.integer(rownames(priceXGBoostModel$bestTune)), ]

trellis.par.set(caretTheme())
plot(priceXGBoostModel)
vioplot(priceXGBoostModel$resample$GainPerc, names = 'Profit [%]', col = 'green')
title('Violin Plot of Profit Percentage in resamples')
summary(priceXGBoostModel$resample$GainPerc)

### Just form features ###
modelInput <- allModelInput[, !grepl('Price', colnames(allModelInput))]
set.seed(seed)
formXGBoostModel <- train(form = resultFormula, data = modelInput, method = 'xgbTree',
                        trControl = customCvContr, tuneGrid = extrBoostGrid, metric = 'GainPerc',
                        objective = 'multi:softprob', num_class = 3)
## Metrics of best model ##
formXGBoostModel$results[as.integer(rownames(formXGBoostModel$results)) == as.integer(rownames(formXGBoostModel$bestTune)), ]

trellis.par.set(caretTheme())
plot(formXGBoostModel)
vioplot(formXGBoostModel$resample$GainPerc, names = 'Gain [%]', col = 'green')
title('Violin Plot of Gain Percentage in resamples')
summary(formXGBoostModel$resample$GainPerc)
