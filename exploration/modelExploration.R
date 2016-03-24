data <- readRDS(file = 'data/BL1_2005-2015.Rds')
odds <- data$odds
### Load featured matches ###
featuredMatches <- readRDS(file = 'data/featuredMatches/111.Rds')

# Parallel processing #
#library(doSNOW)
#cl <- makeCluster(Sys.getenv('CORE_COUNT'))
#registerDoSNOW(cl)
#stopCluster(cl)
##

source(file = 'models/models.R', 
       echo = FALSE, encoding = 'UTF-8')
allModelInput <- selectModelInput(featuredMatches)
seed <- 16450
customCvContr <- trainControl(method = 'cv', number = 5, classProbs = TRUE, 
                              summaryFunction = betMetricsSummary)

resultFormula <- as.formula('matchResult ~ . -matchId -goalsHome -goalsVisitors -goalDiff')

# Extreme gradiant boosting config #
extrBoostGrid <- expand.grid(nrounds = (1:3)*300,
                             max_depth = c(2, 5),
                             eta = c(.05, .075),
                             gamma = c(0, 1),
                             colsample_bytree = 1,
                             min_child_weight = 1)

applicationGrid <- expand.grid(nrounds = (1:3)*100,
                               max_depth = c(2, 5),
                               eta = c(.02, .05),
                               gamma = c(0, 1),
                               colsample_bytree = 1,
                               min_child_weight = 1)

### Just Price features ###
modelInput <- allModelInput[, !grepl('Form', colnames(allModelInput))]

loginfo('Start tuning xgBoost model')
set.seed(seed)
priceXGBoostModel <- train(form = resultFormula, data = modelInput, method = 'xgbTree',
                        trControl = customCvContr, tuneGrid = applicationGrid, metric = 'GainPerc',
                        objective = 'multi:softprob', num_class = 3, allowParallel = TRUE)
loginfo('End tuning xgBoost model')
## Metrics of best model ##
priceXGBoostModel$results[as.integer(rownames(priceXGBoostModel$results)) == as.integer(rownames(priceXGBoostModel$bestTune)), ]

trellis.par.set(caretTheme())
plot(priceXGBoostModel)
vioplot(priceXGBoostModel$resample$GainPerc, names = 'Profit [%]', col = 'green')
title('Violin Plot of Profit Percentage in resamples')
summary(priceXGBoostModel$resample$GainPerc)

### Compare with prediction benchmarks ###

inputData <- data.frame(rowIndex = modelInput$matchId, obs = modelInput$matchResult)
homeVictorybenchmarkData <- fillByAllHomeBenchmark(inputData)
level <- c('VisitorsVictory', 'Draw', 'HomeVictory')
model <- 'HomeVictory Benchmark'
homeVictoryBenchmark <- betMetricsSummary(homeVictorybenchmarkData, level, model)

randomBenchmarkData <- fillByAllRandom(inputData)
model <- 'Random Benchmark'
randomBenchmark <- betMetricsSummary(randomBenchmarkData, level, model)

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



### Price and form features ###
set.seed(seed)
xGBoostModel <- train(form = resultFormula, data = modelInput, method = 'xgbTree',
                           trControl = customCvContr, tuneGrid = extrBoostGrid, metric = 'GainPerc',
                           objective = 'multi:softprob', num_class = 3)
## Metrics of best model ##
xGBoostModel$results[as.integer(rownames(xGBoostModel$results)) == as.integer(rownames(xGBoostModel$bestTune)), ]

trellis.par.set(caretTheme())
plot(xGBoostModel)
vioplot(xGBoostModel$resample$GainPerc, names = 'Gain [%]', col = 'green')
title('Violin Plot of Gain Percentage in resamples')
summary(xGBoostModel$resample$GainPerc)

varImp <- varImp(xGBoostModel)
varImp$importance

### TODO Use dimension reduction ###