load('C:/Users/Tobsen1X/RStudioWorkspace/BetLab/data/basisData.RData')

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/positionFeatureExtraction.R', echo = FALSE, encoding = 'UTF-8')

positions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 'mid', 'off', 'off', 'off', 'off', 'off')
featuredMatches <- extractMatchResultFeatures(playerStats = data$stats,
                                              matches = data$matches,
                                              priceAssignedPositions = positions,
                                              functs = c('min', 'max', 'avg', 'sum'))

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/resultModel.R', echo = FALSE, encoding = 'UTF-8')
featuredMatches <- filterFeaturedMatches(featuredMatches)

## Preparation

library(caret)
seed <- 1234
trainControl <- trainControl(method = 'cv', number = 5)
featuredMatches <- select(featuredMatches, -tw_Price_Home_min, -tw_Price_Home_max,
                          -tw_Price_Home_sum, -tw_Price_Visitors_min, -tw_Price_Visitors_max,
                          -tw_Price_Visitors_sum)
x <- featuredMatches[, 4:29]
y <- featuredMatches$goalDiff

######## Trying to find preferably diverse 1. level models 
paste('Start lm:', Sys.time())
set.seed(seed)
lmModel <- train(x = x, y = y, method = 'lm',
                 preProcess = c('center', 'scale'),
                 trControl = trainControl)

paste('Start enet:', Sys.time())
set.seed(seed)
enetModel <- train(x = x, y = y, method = 'enet',
                   preProcess = c('center', 'scale'),
                   trControl = trainControl)

paste('Start pls:', Sys.time())
set.seed(seed)
plsModel <- train(x = x, y = y, method = 'pls',
                  preProcess = c('center', 'scale'),
                  trControl = trainControl)

#paste('Start enpls:', Sys.time())
#set.seed(seed)
#enplsModel <- train(x = x, y = y, method = 'enpls',
#                    preProcess = c('center', 'scale'),
#                    trControl = trainControl)
# -

paste('Start spls:', Sys.time())
set.seed(seed)
splsModel <- train(x = x, y = y, method = 'spls',
                   preProcess = c('center', 'scale'),
                   trControl = trainControl)

paste('Start brnn:', Sys.time())
set.seed(seed)
brnnModel <- train(x = x, y = y, method = 'brnn',
                   preProcess = c('center', 'scale'),
                   trControl = trainControl, verbose = FALSE)

#paste('Start avnnet:', Sys.time())
#set.seed(seed)
#avNNetNeuralNetModel <- train(x = x, y = y, method = 'avNNet',
#                              preProcess = c('center', 'scale'),
#                              trControl = trainControl)
# --

paste('Start svmLinear:', Sys.time())
set.seed(seed)
svmLinearModel <- train(x = x, y = y, method = 'svmLinear',
                        preProcess = c('center', 'scale'),
                        trControl = trainControl)

paste('Start svmPoly:', Sys.time())
set.seed(seed)
svmPolyModel <- train(x = x, y = y, method = 'svmPoly',
                      preProcess = c('center', 'scale'),
                      trControl = trainControl)
# -

paste('Start baggedMars:', Sys.time())
set.seed(seed)
baggedMarsModel <- train(x = x, y = y, method = 'bagEarth',
                         trControl = trainControl)

paste('Start knn:', Sys.time())
knnGrid <- expand.grid(.k = floor(1.5 ^ seq(from = 8, to = 14, by = 1)))
set.seed(seed)
knnModel <- train(x = x, y = y, method = 'knn',
                  preProcess = c('center', 'scale'),
                  tuneGrid = knnGrid, trControl = trainControl)

paste('Start rknn:', Sys.time())
set.seed(seed)
rknnModel <- train(x = x, y = y, method = 'rknn',
                   preProcess = c('center', 'scale'),
                   trControl = trainControl)
# --

### Tree based models ###

paste('Start bstTree:', Sys.time())
set.seed(seed)
bstTreeModel <- train(x = x, y = y, method = 'bstTree',
                      #preProcess = c('center', 'scale'),
                      trControl = trainControl)

paste('Start ctree:', Sys.time())
set.seed(seed)
ctreeModel <- train(x = x, y = y, method = 'ctree',
                    #preProcess = c('center', 'scale'),
                    trControl = trainControl)

paste('Start M5:', Sys.time())
set.seed(seed)
M5Model <- train(x = x, y = y, method = 'M5',
                 #preProcess = c('center', 'scale'),
                 trControl = trainControl)
# --

paste('Start M5rules:', Sys.time())
set.seed(seed)
M5RulesModel <- train(x = x, y = y, method = 'M5Rules',
                      #preProcess = c('center', 'scale'),
                      trControl = trainControl)
# --

paste('Start treebag:', Sys.time())
set.seed(seed)
treebagModel <- train(x = x, y = y, method = 'treebag',
                      #preProcess = c('center', 'scale'),
                      trControl = trainControl)

### Other Models ###

paste('Start rf:', Sys.time())
set.seed(seed)
rfModel <- train(x = x, y = y, method = 'rf',
                     #preProcess = c('center', 'scale'),
                     trControl = trainControl)

paste('Start cubist:', Sys.time())
set.seed(seed)
cubistModel <- train(x = x, y = y, method = 'cubist',
                     #preProcess = c('center', 'scale'),
                     trControl = trainControl)

paste('Start gam:', Sys.time())
set.seed(seed)
gamModel <- train(x = x, y = y, method = 'gam',
                  #preProcess = c('center', 'scale'),
                  trControl = trainControl)

paste('Start gaussPoly:', Sys.time())
set.seed(seed)
gaussprPolyModel <- train(x = x, y = y, method = 'gaussprPoly',
                          #preProcess = c('center', 'scale'),
                          trControl = trainControl)

paste('Start gbm:', Sys.time())
set.seed(seed)
gbmModel <- train(x = x, y = y, method = 'gbm',
                  #preProcess = c('center', 'scale'),
                  trControl = trainControl, verbose = FALSE)

paste('Start glmnet:', Sys.time())
set.seed(seed)
glmnetModel <- train(x = x, y = y, method = 'glmnet',
                     #preProcess = c('center', 'scale'),
                     trControl = trainControl)

paste('Start icr:', Sys.time())
icr.grid <- expand.grid(.n.comp = c(2,3,4,5,6,7))
set.seed(seed)
icrModel <- train(x = x, y = y, method = 'icr',
                  #preProcess = c('center', 'scale'),
                  trControl = trainControl,
                  tuneGrid = icr.grid)

paste('Start lars:', Sys.time())
lars.grid <- expand.grid(.fraction = seq(0.05, 1, by = 0.075))
set.seed(seed)
larsModel <- train(x = x, y = y, method = 'lars',
                   #preProcess = c('center', 'scale'),
                   trControl = trainControl,
                   tuneGrid = lars.grid)

modelList <- list('lm' = lmModel,
                  'enet' = enetModel,
                  'pls' = plsModel,
                  'spls' = splsModel,
                  'brnn' = brnnModel,
                  'svmLinear' = svmLinearModel,
                  'svmPoly' = svmPolyModel,
                  'baggedMars' = baggedMarsModel,
                  'knn' = knnModel,
                  'rknn' = rknnModel,
                  'bstTree' = bstTreeModel,
                  'ctree' = ctreeModel,
                  'm5' = M5Model,
                  'm5Rules' = M5RulesModel,
                  'treebag' = treebagModel,
                  'rf' = rfModel,
                  'cubist' = cubistModel,
                  'gam' = gamModel,
                  'gaussPoly' = gaussprPolyModel,
                  'gbm' = gbmModel,
                  'glmnet' = glmnetModel,
                  'icr' = icrModel,
                  'lars' = larsModel)

saveRDS(modelList, 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/rds/modelList.rds')