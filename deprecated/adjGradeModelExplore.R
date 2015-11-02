modelData <- adjGradeData %>% select(kickerGrade, fitPrice, 
                                     opponentPrice, home, transPos)

# Checking for skewness in numeric predictors
modelData %>% select(fitPrice, opponentPrice) %>% apply(2, skewness)

# BoxCox Transformation to resolve skewness
preProc <- modelData %>% select(fitPrice, opponentPrice) %>% 
    preProcess(method = c('center', 'scale', 'BoxCox'))
preProcPredictors <-  preProc %>% predict(select(modelData, fitPrice, opponentPrice))
preProcPredictors <- preProcPredictors %>% dplyr::rename(preProcFitPrice = fitPrice, 
                                                         preProcOpponentPrice = opponentPrice)
# attach preprocessed predictors
modelData <- modelData %>% cbind(preProcPredictors)

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
set.seed(1234)
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

grid.arrange(residPlot1, residPlot2, ncol = 2, main = "Residual Plots")