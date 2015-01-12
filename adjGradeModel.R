# Calculates the weighted price of the opponents playing against
getOpponentPrice <- function(playerStats, posPriceWeights, gId, h, transPos) {
    opponentStats <- subset(playerStats, matchId == gId & home != h)
    
    weightSum <- 0
    priceSum <- 0
    for(i in 1:nrow(opponentStats)) {
        row <- opponentStats[i,]
        # Only opponents with transPos are considered
        if(!is.na(transPos) & !is.na(row$transPos)) {
            weight <- posPriceWeights[transPos, row$transPos]
            weightSum <- weightSum + weight
            priceSum <- priceSum + weight * row$fitPrice
        }
    }
    priceSum / weightSum
}

# Calculates the adjusted grade which purges the kicker grade with the aim
# that this adjusted grade just represent the short/middle term form
# of the player.
enrichAdjGrade <- function(playerStats) {
    
    # Remove all player stats without a price, transPos or kickerGrade
    relPlayerStats <- subset(playerStats, !is.na(fitPrice) & !is.na(transPos) &
                                 !is.na(kickerGrade))
    
    #############   FEATURE EXTRACTION      ######################
    
    # Load Position Price Weight Matrix
    posPriceWeight <- read.table('data/opponentPriceWeights.csv', sep = ';', 
                                 quote = '', skip = 1)
    posPriceWeight[, 1] <- NULL
    rownames(posPriceWeight) <- levels(relPlayerStats$transPos)
    colnames(posPriceWeight) <- levels(relPlayerStats$transPos)
    
    print('Table of Trans Positions of stats with fitPrice, transPos and kickerGrade:')
    print(summary(relPlayerStats$transPos))
    
    shrinkedPlayerStats <- subset(relPlayerStats, 
                                  select = c(matchId, playerId, home, 
                                             transPos, fitPrice))
    
    # enrich player stats with opponent price
    relPlayerStats <- cbind(relPlayerStats, opponentPrice = mapply(
        getOpponentPrice, relPlayerStats$matchId, relPlayerStats$home, 
        relPlayerStats$transPos, 
        MoreArgs = list(playerStats = shrinkedPlayerStats, 
                        posPriceWeight = posPriceWeight)))
    
    #relPlayerStats$priceDiff <- relPlayerStats$fitPrice - 
    #    relPlayerStats$opponentPrice
    #relPlayerStats$logPriceFraction <- log(relPlayerStats$fitPrice / 
    #                                           relPlayerStats$opponentPrice)
    
    #x <- subset(relPlayerStats, select = c(fitPrice, opponentPrice, 
                                           #priceDiff, logPriceFraction, 
    #                                       home, transPos))
    #y <- subset(relPlayerStats, select = c(kickerGrade))
    
    #corMatrix <- cor(x[, -c(3, 4)])
    #print('Correlation Matrix:')
    #print(corMatrix)
    
    require(caret)
    #highlyCorrelated <- findCorrelation(corMatrix, cutoff=0.75)
    #print('Predictors which are highly correlated (> 0.75):')
    #print(highlyCorrelated)
    
    repCVControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
    cvControl <- trainControl(method = 'cv', number = 5)
    
    set.seed(1)
    
    ################    LM MODEL    ##################################
    
    lmFit <- train(kickerGrade ~ fitPrice + opponentPrice + home + transPos,
                   data = relPlayerStats, method = 'lm', trControl = repCVControl,
                   preProcess = c('center', 'scale'))
    print(lmFit)
    
    lmImportance <- varImp(lmFit, scale = FALSE)
    print(lmImportance)
    
    ########################    RANDOM FOREST MODEL     ###################################
    
    print(paste('Random Forest Adjusted Grade Model Start Time:', Sys.time()))
    rfFit <- train(kickerGrade ~ fitPrice + opponentPrice + home + transPos
                   , data = relPlayerStats, 
                   method = 'rf', trControl = cvControl, 
                   tuneGrid = data.frame(mtry = 3), 
                   importance = TRUE)
    print(paste('End Time:', Sys.time()))
    print(rfFit)
    
    rfImportance <- varImp(rfFit, scale = FALSE)
    print(rfImportance)
    
    #######################     Stochastic Gradient Boosting    ###########################
    
    # Final model uses n.trees = 250, interaction.depth = 10 and shrinkage = 0.1
    gbmGrid <- expand.grid(.interaction.depth = (1:5) * 2,
                           .n.trees = (1:10)*25, .shrinkage = .1)
    gbmFit <- train(kickerGrade ~ fitPrice + opponentPrice + home + transPos
                    , data = relPlayerStats, 
                    method = 'gbm', trControl = cvControl,verbose = FALSE, tuneGrid = gbmGrid )
    print(gbmFit)
    
    ############################    COMBINED RANDOM FOREST MODEL    ######################
    
    #print(paste('Random Forest adjusted Grade Combined Model Start Time:', 
    #            Sys.time()))
    #combFit <- train(kickerGrade ~ lmPredKickerGrade + rfPredKickerGrade + gbmPredKickerGrade, 
    #                 data = relPlayerStats, method = 'rf', 
    #                 trControl = cvControl,    # tuneGrid = data.frame(mtry = 3), 
    #                 importance = TRUE)
    #print(paste('End Time:', Sys.time()))
    #print(combFit)
    
    ############################    GBM Comb fit Model  ###################################
    
    relPlayerStats$lmPredKickerGrade <- predict(lmFit, newdata = relPlayerStats)
    relPlayerStats$rfPredKickerGrade <- predict(rfFit, newdata = relPlayerStats)
    relPlayerStats$gbmPredKickerGrade <- predict(gbmFit, newdata = relPlayerStats)
    
    # The final values used for the model were n.trees = 150, interaction.depth = 8 and shrinkage = 0.1.
    print(paste('GBM adjusted Grade Combined Model Start Time:', 
                Sys.time()))
    gbmCombFit <- train(kickerGrade ~ lmPredKickerGrade + rfPredKickerGrade + gbmPredKickerGrade, 
                     data = relPlayerStats, method = 'gbm', 
                     trControl = cvControl, verbose = FALSE, tuneGrid = gbmGrid)
    print(paste('End Time:', Sys.time()))
    print(gbmCombFit)
    
    
    combImportance <- varImp(gbmCombFit, scale = FALSE)
    print(combImportance)
    
    relPlayerStats$combPredKickerGrade <- predict(gbmCombFit, 
                                                  newdata = relPlayerStats)
    relPlayerStats$adjGrade <- relPlayerStats$combPredKickerGrade - 
        relPlayerStats$kickerGrade
    
    relPlayerStats
}