predictMatch <- function(model, matchId) {
    source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/production/loadData.R', 
           echo = FALSE, encoding = 'UTF-8')
    
    predData <- loadPredictionData(matchId = matchId)
    
    source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/production/positionFeatureExtraction.R', 
           echo = FALSE, encoding = 'UTF-8')
    
    ### Preparation
    #[1] "Torwart"               "Innenverteidiger"      "Linker Verteidiger"    "Rechter Verteidiger"   "Defensives Mittelfeld"
    #[6] "Zentrales Mittelfeld"  "Linkes Mittelfeld"     "Rechtes Mittelfeld"    "Offensives Mittelfeld" "Haengende Spitze"     
    #[11] "Mittelstuermer"        "Linksaussen"           "Rechtsaussen"
    positions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 'mid', 'off', 'off', 'off', 'off', 'off')
    featuredMatches <- extractMatchResultFeatures(playerStats = predData$stats,
                                                  matches = predData$matches,
                                                  priceAssignedPositions = positions,
                                                  functs = c('min', 'max', 'avg', 'sum'))
    featuredMatches <- dplyr:::select(featuredMatches, -tw_Price_Home_min, -tw_Price_Home_max,
                                      -tw_Price_Home_sum, -tw_Price_Visitors_min, -tw_Price_Visitors_max,
                                      -tw_Price_Visitors_sum)
    
    newData <- rbind(featuredMatches, featuredMatches)
    
    preds <- predict(model, newdata = newData, type = 'prob')
    return(preds[1,])
}