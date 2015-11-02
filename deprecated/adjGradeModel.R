# Calculates the weighted price of the opponents playing against
getOpponentPrice <- function(playerStats, posPriceWeights, gId, h, transPos) {
    require(dplyr)
    opponentStats <- filter(playerStats, matchId == gId, home != h)
    
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
    return(priceSum / weightSum)
}

extractFeaturesForAdjGradeModel <- function(playerStats) {
    require(dplyr)
    # Remove all player stats without a price, transPos or kickerGrade
    relPlayerStats <- filter(playerStats, !is.na(fitPrice), !is.na(transPos),
                             !is.na(kickerGrade))
    
    #############   FEATURE EXTRACTION      ######################
    
    # Load Position Price Weight Matrix
    posPriceWeight <- read.table('data/opponentPriceWeights.csv', sep = ';', 
                                 quote = '', skip = 1)
    posPriceWeight[, 1] <- NULL
    rownames(posPriceWeight) <- levels(relPlayerStats$transPos)
    colnames(posPriceWeight) <- levels(relPlayerStats$transPos)
    
    #print('Table of Trans Positions of stats with fitPrice, transPos and kickerGrade:')
    #print(summary(relPlayerStats$transPos))
    
    shrinkedPlayerStats <- select(relPlayerStats, matchId, 
                                  playerId, home, transPos, fitPrice)
    
    # enrich player stats with opponent price
    relPlayerStats <- cbind(relPlayerStats, opponentPrice = mapply(
        getOpponentPrice, relPlayerStats$matchId, relPlayerStats$home, 
        relPlayerStats$transPos, 
        MoreArgs = list(playerStats = shrinkedPlayerStats, 
                        posPriceWeight = posPriceWeight)))
    
    relPlayerStats$opponentPrice[is.na(relPlayerStats$opponentPrice)] <- 0
    
    return(relPlayerStats)
}

# Calculates the adjusted grade which purges the kicker grade with the aim
# that this adjusted grade solely represent the short/middle term form
# of the player.
enrichAdjGrade <- function(relPlayerStats) {
    require(caret)
    require(dplyr)
    require(magrittr)
    modelData <- relPlayerStats %>% select(kickerGrade, fitPrice, 
                                           opponentPrice, home, transPos)
    
    # BoxCox Transformation to resolve skewness
    #    preProc <- modelData %>% select(fitPrice, opponentPrice) %>% 
    #        preProcess(method = c('center', 'scale', 'BoxCox'))
    #    preProcPredictors <-  preProc %>% predict(select(modelData, fitPrice, opponentPrice))
    #    preProcPredictors <- preProcPredictors %>% dplyr:::rename(preProcFitPrice = fitPrice, 
    #                                                      preProcOpponentPrice = opponentPrice)
    # attach preprocessed predictors
    #modelData <- modelData %>% cbind(preProcPredictors)
    
    repCVControl <- trainControl(method = 'repeatedcv', 
                                 number = 10, repeats = 3)
    
    ################    LM MODEL    ##################################
    set.seed(1)
    lmFit <- train(kickerGrade ~ poly(fitPrice, 3) + 
                              poly(opponentPrice, 3) +
                              home + transPos, data = modelData,
                          method = 'lm', trControl = repCVControl)
    
    #    lmPreProcFit <- train(kickerGrade ~ poly(preProcFitPrice, 3) + 
    #                               poly(preProcOpponentPrice, 3) +
    #                               home + transPos, data = modelData,
    #                          method = 'lm', trControl = repCVControl)
    
    # Subtracting the achieved kicker grade from the predicted one to
    # extract the pure player performance
    relPlayerStats$adjGrade <- predict(lmFit, newdata = modelData) - 
        relPlayerStats$kickerGrade
    
    relPlayerStats
}