########### FeatureExtraction   #################

source(file = 'featureExtraction.R', echo = FALSE, encoding = 'UTF-8')
featuredMatches <- extractFeatures(formEnrichedPlayerStats, chancesEnrMatches)
describe(select(featuredMatches, goalDiff, priceDiff, logPriceRate, formMeanfDiff, 
                formSesOptimalDiff, formSesSimpleDiff, expGoalDiff, expChancesDiff))
summary(select(featuredMatches, goalDiff, priceDiff, logPriceRate, formMeanfDiff, 
               formSesOptimalDiff, formSesSimpleDiff, expGoalDiff, expChancesDiff))

library(caret)
library(ggplot2)

# Price and poisson features
p1 <- featuredMatches %>% ggplot(aes(y = goalDiff, x = priceDiff)) +
    geom_smooth()
p2 <- featuredMatches %>% ggplot(aes(y = goalDiff, x = logPriceRate)) +
    geom_smooth()
p3 <- featuredMatches %>% ggplot(aes(y = goalDiff, x = expGoalDiff)) +
    geom_smooth()
p4 <- featuredMatches %>% ggplot(aes(y = goalDiff, x = expChancesDiff)) +
    geom_smooth()
p5 <- featuredMatches %>% ggplot(aes(y = goalDiff, x = expGoalByChancesDiff)) +
    geom_smooth()
library(grid)
library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, ncol = 2, main = "feature vs. goalDiff")

# Form features
f1 <- featuredMatches %>% ggplot(aes(y = goalDiff, x = formMeanfDiff)) +
    geom_smooth()
f2 <- featuredMatches %>% ggplot(aes(y = goalDiff, x = formExpSmoothDiff)) +
    geom_smooth()

grid.arrange(f1, f2, ncol = 2, main = "feature vs. goalDiff")

summary(featuredMatches)
### Data splitting ###
modelInput <- featuredMatches %>% filter(!is.na(priceDiff), !is.na(formMeanfDiff))
set.seed(1)
testIndex <- createDataPartition(featuredMatches$goalDiff, p = 0.2,
                                 list = FALSE,
                                 times = 1)
test <- featuredMatches[ testIndex, ]
train <- featuredMatches[ -testIndex, ]
set.seed(1)
goalDiffTrainIndex <- createDataPartition(train$goalDiff, p = 0.5,
                                          list = FALSE,
                                          times = 1)
goalDiffTrain <- train[ goalDiffTrainIndex, ]
resultTrain <- train[ -goalDiffTrainIndex, ]

### goal diff model exploration ###
library(caret)
set.seed(1234)
?preProcess
formComponents <- preProcess(select(featuredMatches, formMeanfDiff, 
                                    formSesSimpleDiff, formSesOptimalDiff),
                             method = 'pca')

formTrain <- predict(formComponents, select(featuredMatches, formMeanfDiff, 
                                            formSesSimpleDiff, formSesOptimalDiff))

extFeaturedMatches <- cbind(featuredMatches, formTrain)

# goalDiffPCAFormula <- goalDiff ~ priceDiff + logPriceRate +  
#    expChancesDiff + PC1 + PC2
# lmGoalDiffPCAFit <- train(goalDiffPCAFormula, method = 'lm',
#                          data = extFeaturedMatches, trControl = trainControl(method = 'cv'))
# summary(lmGoalDiffPCAFit)

goalDiffFormula <- goalDiff ~ priceDiff + logPriceRate +  
    expChancesDiff + formMeanfDiff # + formSesSimpleDiff + formSesOptimalDiff

lmGoalDiffFit <- train(goalDiffFormula, method = 'lm',
                          data = featuredMatches, trControl = trainControl(method = 'cv'))
summary(lmGoalDiffFit)

# Multivariate adaptive regression splines
marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:38)
set.seed(1)
earthGoalDiffFit <- train(goalDiffFormula, method = 'earth',
                          data = featuredMatches, tuneGrid = marsGrid, 
                          trControl = trainControl(method = 'cv'))
summary(earthGoalDiffFit)

# Neural Network
nnetGrid <- expand.grid(.decay = c(0, 0.01, 0.1),
                        .size = c(1:10))
set.seed(1)
nnetGoalDiffFit <- train(goalDiffFormula, method = 'nnet',
                         tuneGrid = nnetGrid, trControl = trainControl(method = 'cv'),
                         data = featuredMatches,
                         preProc = c('center', 'scale'), linout = TRUE,
                         trace = FALSE, MaxNWts = 10 * (3 + 1) + 10 + 1,
                         maxit = 500)
nnetGoalDiffFit

# Random Forest
rfGoalDiffFit <- train(goalDiffFormula, method = 'rf', 
                       data = featuredMatches, trControl = trainControl(method = 'cv'))
rfGoalDiffFit

# Bagged CART
baggedCartGoalDiffFit <- train(goalDiffFormula, method = 'treebag',
                               data = featuredMatches, trControl = trainControl(method = 'cv'))
baggedCartGoalDiffFit

# Model Comparison
resamples <- resamples(list(lmFit = lmGoalDiffFit, marsFit = earthGoalDiffFit,
                            nnetFit = nnetGoalDiffFit, rfFit = rfGoalDiffFit,
                            baggedCartFit = baggedCartGoalDiffFit))
summary(resamples)

residPlot1 <- featuredMatches %>% ggplot(aes(y = goalDiff, x = predict(lmGoalDiffFit, featuredMatches))) +
    geom_point() +
    geom_smooth() +
    coord_fixed(ratio = 1, xlim = c(-2.5, 3), ylim = c(-2.5, 3)) +
    scale_x_continuous(name = 'Predicted') +
    scale_y_continuous(name = 'Observed') +
    geom_abline(intercept = 0, slope = 1, size = 0.2, linetype = 'dashed')

residPlot2 <- featuredMatches %>% ggplot(aes(y = resid(lmGoalDiffFit), 
                                           x = predict(lmGoalDiffFit, featuredMatches))) +
    geom_point() +
    geom_smooth() +
    scale_x_continuous(name = 'Predicted') +
    scale_y_continuous(name = 'Residual') +
    geom_hline(size = 0.2, linetype = 'dashed')

library(grid)
library(gridExtra)
grid.arrange(residPlot1, residPlot2, ncol = 2, main = "Residual Plots")

###########################################################

set.seed(1234)
goalDiffIndex <- createDataPartition(featuredMatches$goalDiff, p = 0.5,
                                 list = FALSE,
                                 times = 1)
trainGoalDiff <- featuredMatches[ goalDiffIndex, ]
trainResult <- featuredMatches[ -goalDiffIndex, ]

### Goal Diff Models
goalDiffFormula <- goalDiff ~ priceDiff + logPriceRate +  
    expChancesDiff + formMeanfDiff # + formSesSimpleDiff + formSesOptimalDiff

lmGoalDiffFit <- train(goalDiffFormula, method = 'lm',
                       data = trainGoalDiff, trControl = trainControl(method = 'cv'))

# Multivariate adaptive regression splines
marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:38)
earthGoalDiffFit <- train(goalDiffFormula, method = 'earth',
                          data = trainGoalDiff, tuneGrid = marsGrid, 
                          trControl = trainControl(method = 'cv'))

# Random Forest
rfGoalDiffFit <- train(goalDiffFormula, method = 'rf', 
                       data = trainGoalDiff, trControl = trainControl(method = 'cv'))

# Bagged CART
baggedCartGoalDiffFit <- train(goalDiffFormula, method = 'treebag',
                               data = trainGoalDiff, trControl = trainControl(method = 'cv'))

goalDiffModels <- list(lm = lmGoalDiffFit, mars = earthGoalDiffFit, rf = rfGoalDiffFit,
                       baggedCART = baggedCartGoalDiffFit)
goalDiffPreds <- predict(goalDiffModels, newdata = trainResult)
resultModelInput <- do.call(cbind.data.frame, goalDiffPreds)
resultModelInput$expGoalDiff <- trainResult$expGoalDiff
resultModelInput$matchResult <- trainResult$matchResult
resultModelInput$matchId <- trainResult$matchId

# Ensamble Result Model
ensambleFormula <- matchResult ~ lm + mars + rf + baggedCART + expGoalDiff
resultRfFit <- train(ensambleFormula, method = 'rf',
                     data = resultModelInput, trControl = trainControl(method = 'cv'))
resultRfFit

###########################################################################






resultModelInput <- train %>% mutate(goalDiffPred = predict(lmGoalDiffFit, train))
polrFit <- train(matchResult ~ goalDiffPred, method = 'polr', data = resultModelInput, 
                     trControl = repCVControl)
polrFit

testSet <- data.frame(goalDiffPred = predict(lmGoalDiffFit, test))
testSet$matchResult <- test$matchResult
testSet$matchId <- test$matchId
testResult <- predict(polrFit, testSet, type = 'prob')
testResult$matchResult <- testSet$matchResult
testResult$matchId <- testSet$matchId

source(file = 'evaluatePrediction.R', echo = FALSE, encoding = 'UTF-8')
evaluations <- evaluatePrediction(testResult)
printEvaluation(evaluations)

# Not meaningful #
############ Enrich team form ###############
source(file = 'teamForm.R', echo = FALSE, encoding = 'UTF-8')
formMatches <- enrichTeamForm(stats, matches, 3, 5, 5)
formMatches <- formMatches %>% mutate(teamFormDiff = homeTeamForm - visitorsTeamForm)
library(psych)
describe(select(formMatches, homeTeamForm, visitorsTeamForm))
library(caret)
lmFit <- train(goalDiff ~ teamFormDiff, method = 'lm', data = formMatches)
summary(lmFit)

library(ggplot2)
formMatches %>% ggplot(aes(y = goalDiff, x = (homeTeamForm - visitorsTeamForm))) +
    geom_smooth() +
    geom_point()