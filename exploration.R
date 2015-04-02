##########   Exploration of the raw DB data and the fit price assignments    ##########
source(file = 'loadData.R', echo = FALSE, encoding = 'UTF-8')
data <- loadData('BL1')
stats <- data$playerStats
matches <- data$matches
odds <- data$odds

library(dplyr)
summary(select(stats, kickerGrade, transPos, playerAssignment, fitPrice))

############ Explore BivPois Goal Model #########
load('data/bivPois.RData')
source(file = 'bivpois/estimateGoals.R', echo = FALSE, encoding = 'UTF-8')

enrMatches <- enrichBivPoisExpGoals(matches, minMatchdays = 5)

library(psych)
describe(select(enrMatches, goalsHome, goalsVisitors, 
                homeExpGoals, visitorsExpGoals))

enrMatches %>% ggplot(aes(y = goalDiff, x = (homeExpGoals - visitorsExpGoals))) +
    geom_smooth() +
    coord_fixed(ratio = 1, xlim = c(-3, 3), ylim = c(-3, 3)) +
    scale_x_continuous(name = 'Predicted') +
    scale_y_continuous(name = 'Observed') +
    geom_abline(intercept = 0, slope = 1, size = 0.01, linetype = 'dashed')

# Residuals depending on matchday
enrMatches %>% ggplot(aes(y = abs(goalDiff - (homeExpGoals - visitorsExpGoals)), x = matchday)) +
    geom_smooth() +
    scale_x_continuous(name = 'Matchday') +
    scale_y_continuous(name = 'abs(residual)')

############ Enrich adjusted grades #############
source(file = 'adjGradeModel.R', echo = FALSE, encoding = 'UTF-8')

### Explore adjusted grade ###
adjGradeData <- extractFeaturesForAdjGradeModel(stats)
suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
modelData <- adjGradeData %>% select(kickerGrade, fitPrice, 
                                     opponentPrice, home, transPos)

suppressMessages(library(e1071))
# Checking for skewness in numeric predictors
modelData %>% select(fitPrice, opponentPrice) %>% apply(2, skewness)

suppressMessages(library(caret))
# BoxCox Transformation to resolve skewness
preProc <- modelData %>% select(fitPrice, opponentPrice) %>% 
    preProcess(method = c('center', 'scale', "BoxCox"))
preProcPredictors <-  preProc %>% predict(select(modelData, fitPrice, opponentPrice))
preProcPredictors <- preProcPredictors %>% dplyr::rename(preProcFitPrice = fitPrice, 
                                                  preProcOpponentPrice = opponentPrice)
# attach preprocessed predictors
modelData <- modelData %>% cbind(preProcPredictors)

library(ggplot2)
modelData %>% ggplot(aes(y = kickerGrade)) +
    geom_smooth(aes(x = preProcFitPrice, colour = 'preProcFitPrice')) +
    geom_smooth(aes(x = preProcOpponentPrice, 
                    colour = 'preProcOpponentPrice')) +
    scale_x_continuous(name = element_blank()) +
    scale_colour_discrete(name  = element_blank(),
                          breaks=c("preProcFitPrice", "preProcOpponentPrice"),
                          labels=c("Fit Price [preProc]", "Opponent Price [preProc]")) +
    ggtitle(label = 'Preprocessed Fit Price and Opponent Price\nagainst the outcome')

repCVControl <- trainControl(method = 'repeatedcv', number = 10, 
                             repeats = 3)
set.seed(1)
# ordinary linear regression
lmPreProcFit <- train(kickerGrade ~ poly(preProcFitPrice, 3) + 
                          poly(preProcOpponentPrice, 3) +
                          home + transPos, data = modelData,
                      method = 'lm', trControl = repCVControl)
lmPreProcFit

residPlot1 <- modelData %>% ggplot(aes(y = kickerGrade, x = predict(lmPreProcFit, modelData))) +
    geom_smooth() +
    coord_fixed(ratio = 1, xlim = c(2, 5), ylim = c(2, 5)) +
    scale_x_continuous(name = 'Predicted') +
    scale_y_continuous(name = 'Observed') +
    geom_abline(intercept = 0, slope = 1, size = 0.01, linetype = 'dashed')
    
residPlot2 <- modelData %>% ggplot(aes(y = resid(lmPreProcFit), x = predict(lmPreProcFit, modelData))) +
    geom_smooth() +
    scale_x_continuous(name = 'Predicted') +
    scale_y_continuous(name = 'Residual') +
    geom_hline(size = 0.01, linetype = 'dashed')

library(grid)
library(gridExtra)
grid.arrange(residPlot1, residPlot2, ncol = 2, main = "Residual Plots")

### continue enrich adjusted grade ###

enrichedStats <- enrichAdjGrade(adjGradeData)
# Merges adjusted grade in stats
mergedStats <- merge(stats, select(
    enrichedStats, matchId, playerId, adjGrade),
    by = c('matchId', 'playerId'),  all.x = TRUE)
summary(mergedStats$adjGrade)

########## Enrich player form ##########
source(file = 'formModel.R', echo = FALSE, encoding = 'UTF-8')
formEnrichedPlayerStats <- enrichForm(mergedStats, matches, minMatchdays = 5,
                                      imputeBenchBy = -0.05, imputeNotPlayedBy = -0.25)

formExploreData <- formEnrichedPlayerStats %>% filter(
    !is.na(adjGrade), !is.na(sesOptimal))

describe(select(formExploreData, adjGrade, meanf, 
                sesOptimal, sesSimple, formQuality))
summary(select(formExploreData, adjGrade, meanf, 
               sesOptimal, sesSimple, formQuality))



means <- rep(mean(formExploreData$adjGrade), length(formExploreData$adjGrade))

accuracy(means, formExploreData$adjGrade)
accuracy(formExploreData$meanf, formExploreData$adjGrade)
accuracy(formExploreData$sesOptimal, formExploreData$adjGrade)
accuracy(formExploreData$sesSimple, formExploreData$adjGrade)
> accuracy(formExploreData$meanf, formExploreData$adjGrade)
ME      RMSE       MAE      MPE    MAPE
Test set -0.02399178 0.9122064 0.7262043 57.40415 218.396
> accuracy(formExploreData$sesOptimal, formExploreData$adjGrade)
ME      RMSE       MAE      MPE     MAPE
Test set -0.03001043 0.9394085 0.7452072 48.43653 251.5511
> accuracy(formExploreData$sesSimple, formExploreData$adjGrade)
ME      RMSE      MAE      MPE     MAPE
Test set -0.02762759 0.9525993 0.755079 57.16827 257.9617



########### simpleResultFeatureExtraction   #################
library(dplyr)
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
library(grid)
library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 2, main = "feature vs. goalDiff")

# Form features
f1 <- featuredMatches %>% ggplot(aes(y = goalDiff, x = formMeanfDiff)) +
    geom_smooth()
f2 <- featuredMatches %>% ggplot(aes(y = goalDiff, x = formSesOptimalDiff)) +
    geom_smooth()
f3 <- featuredMatches %>% ggplot(aes(y = goalDiff, x = formSesSimpleDiff)) +
    geom_smooth()
grid.arrange(f1, f2, f3, ncol = 2, main = "feature vs. goalDiff")


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
goalDiffFormula <- goalDiff ~ priceDiff + logPriceRate +  
    formMeanfDiff + expChancesDiff
lmGoalDiffFit <- train(goalDiffFormula, method = 'lm',
               data = featuredMatches, trControl = trainControl(method = 'cv'))
lmGoalDiffFit
summary(lmGoalDiffFit)

# Multivariate adaptive regression splines
marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:38)
set.seed(1)
earthGoalDiffFit <- train(goalDiffFormula, method = 'earth',
                          data = goalDiffTrain, tuneGrid = marsGrid, 
                          trControl = trainControl(method = 'cv'))
summary(earthGoalDiffFit)

# Neural Network
nnetGrid <- expand.grid(.decay = c(0, 0.01, 0.1),
                        .size = c(1:10))
set.seed(1)
nnetGoalDiffFit <- train(goalDiff ~ priceDiff + logPriceRate + formDiff, method = 'nnet',
                         tuneGrid = nnetGrid, trControl = trainControl(method = 'cv'),
                         data = goalDiffTrain,
                         preProc = c('center', 'scale'), linout = TRUE,
                         trace = FALSE, MaxNWts = 10 * (3 + 1) + 10 + 1,
                         maxit = 500)
nnetGoalDiffFit

resamples <- resamples(list(lmFit = lmGoalDiffFit, marsFit = earthGoalDiffFit,
                            nnetFit = nnetGoalDiffFit))
summary(resamples)




residPlot1 <- goalDiffTrain %>% ggplot(aes(y = goalDiff, x = predict(lmGoalDiffFit, goalDiffTrain))) +
    geom_point() +
    geom_smooth() +
    coord_fixed(ratio = 1, xlim = c(-2.5, 3), ylim = c(-2.5, 3)) +
    scale_x_continuous(name = 'Predicted') +
    scale_y_continuous(name = 'Observed') +
    geom_abline(intercept = 0, slope = 1, size = 0.2, linetype = 'dashed')

residPlot2 <- goalDiffTrain %>% ggplot(aes(y = resid(lmGoalDiffFit), 
                                           x = predict(lmGoalDiffFit, goalDiffTrain))) +
    geom_point() +
    geom_smooth() +
    scale_x_continuous(name = 'Predicted') +
    scale_y_continuous(name = 'Residual') +
    geom_hline(size = 0.2, linetype = 'dashed')

library(grid)
library(gridExtra)
grid.arrange(residPlot1, residPlot2, ncol = 2, main = "Residual Plots")

### explore result model ###
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