qualifyPrediction <- function(prediction, comparison, probRatioToBet = 1.1) {
    #print(prediction)
    #head(prediction)
    #head(comparison)
    
    valueDiffVec <- c()
    possibleMatches <- 0
    einsatz <- 0
    gainVec <- c()
    
    for(i in seq(1:length(prediction$matchId))) {
        aktPred <- prediction[i, ]
        aktComp <- comparison[comparison$matchId == aktPred$matchId, ]
        
        predProb <- as.numeric(aktPred[as.character(aktPred$matchResult)])
        compProb <- as.numeric(aktComp[as.character(aktPred$matchResult)])
        
        valueDiffVec <- c(valueDiffVec, (predProb - compProb))
        
        
        
        # Bet on HomeVictory
        if(as.numeric(aktPred$HomeVictory) / as.numeric(aktComp$HomeVictory) >= 
               probRatioToBet) {
            
        }
        # Bet on VisitorsVictory
        if(as.numeric(aktPred$VisitorsVictory) / as.numeric(aktComp$VisitorsVictory) >= 
               probRatioToBet) {
            
        }
        # Bet on Draw
        if(as.numeric(aktPred$Draw) / as.numeric(aktComp$Draw) >= probRatioToBet) {
            
        }
    }
    valueDiff <- sum(valueDiffVec)
}