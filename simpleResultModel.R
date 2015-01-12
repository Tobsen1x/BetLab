
# Filter all observations with one or more NAs in feature variables
ergModelData <- simpleMatcheFeatures[apply(
    simpleMatcheFeatures[, c('homePrice', 'homeForm', 'visitorsPrice', 'visitorsForm')],
        1,function(x)!any(is.na(x))), ]
ergModelData$priceDiff <- ergModelData$homePrice - ergModelData$visitorsPrice
ergModelData$logPriceFraction <- log(ergModelData$homePrice / ergModelData$visitorsPrice)
ergModelData$formDiff <- ergModelData$homeForm - ergModelData$visitorsForm

require(caret)

repCVControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 3, allowParallel = TRUE)
cvControl <- trainControl(method = 'cv', number = 5, allowParallel = TRUE)

# Split in two datasets. One for the preceding goaldiff model 
# and one for the combined model, which calculates
# the probabilities of the match outcomes
set.seed(1)
combIndex <- createDataPartition(ergModelData$goalDiff, p = .5,
                                  list = FALSE,
                                  times = 1)
goalDiffDataset <- ergModelData[ -combIndex,]
matchResultDataset <- ergModelData[combIndex,]

# Linear Model
lmFit <- train(goalDiff ~ priceDiff + logPriceFraction + formDiff, method = 'lm',
                data = goalDiffDataset, trControl = repCVControl)

# Generalized Additive Model using Splines
#require(gam)
#gamSplineFit <- train(goalDiff ~ priceDiff + logPriceFraction + formDiff, method = 'gamSpline',
#                      data = train, trControl = repCVControl)

# Gaussian Process with Polynomial Kernel
#require(kernlab)
#gaussprPolyFit <- train(goalDiff ~ priceDiff + logPriceFraction + formDiff, method = 'gaussprPoly',
#                       data = train, trControl = cvControl)

# Stochastic Gradient Boosting
#require(gbm)
#gbmFit <- train(goalDiff ~ priceDiff + logPriceFraction + formDiff, method = 'gbm',
#                data = train, trControl = repCVControl, verbose = FALSE)

# Partial Least Squares
#require(pls)
#plsFit <- train(goalDiff ~ priceDiff + logPriceFraction + formDiff, method = 'pls',
#                 data = train, trControl = repCVControl, verbose = FALSE)

# Support Vector Machines with Polynomial Kernel
#svmPolyFit <- train(goalDiff ~ priceDiff + logPriceFraction + formDiff, method = 'svmPoly',
#                data = train, trControl = cvControl, verbose = FALSE)

# Random Forest
rfFit <- train(goalDiff ~ priceDiff + logPriceFraction + formDiff, method = 'rf',
               data = goalDiffDataset, trControl = cvControl, verbose = FALSE)

# Prediction (Preparation for combined model)
#allModels <- list(lm = lmFit, gamSpline = gamSplineFit,
#                  gaussprPoly = gaussprPolyFit, gbm = gbmFit, pls = plsFit,
#                  svmPoly = svmPolyFit, rf = rfFit)
allModels <- list(lm = lmFit, rf = rfFit)

#predict on matchResultDataset
predictedGoalDiffs <- as.data.frame(predict(allModels, newdata = matchResultDataset))

nzv <- nearZeroVar(predictedValues, saveMetrics = TRUE)
nzv
featureCor <- cor(predictedValues)
featureCor
highlyCorFeatures <- findCorrelation(featureCor, cutoff = .8)
highlyCorFeatures

predictedGoalDiffs$matchResult <- matchResultDataset$matchResult

######### Combined Model ###########
rfCombFit <- train(matchResult ~ ., method = 'rf', data = predictedGoalDiffs, 
                   trControl = cvControl)
rfCombFit

ldaCombFit <- train(matchResult ~ ., method = 'lda', data = predictedGoalDiffs, 
                      trControl = cvControl, verbose = FALSE)
ldaCombFit

multinomCombFit <- train(matchResult ~ ., method = 'multinom', data = predictedGoalDiffs, 
                         trControl = cvControl, verbose = FALSE)
multinomCombFit

######### Evaluate over all matches #############
allGoalDiffPredictions <- as.data.frame(predict(allModels, newdata = ergModelData))
allGoalDiffPredictions$matchResult <- ergModelData$matchResult
allResults <- predict(ldaCombFit, newdata = allGoalDiffPredictions, type = 'prob')
allResults$matchId <- ergModelData$matchId
allResults$matchResult <- ergModelData$matchResult

benchmarkResult <- data.frame()

evaluation <- evaluatePrediction(allResults)
View(evaluation$placedBets)
