# Function calculates statistics for the comparison of the predicted
# probabilites against reference probabilities (bookie probabilities)
evaluatePrediction <- function(prediction, comparison = odds, probRatioToBet = 1.1,
                               stake = 1) {
    valueDiffVec <- c()
    possibleMatches <- 0
    hitVec <- c()
    placedBets <- data.frame()
    for(i in seq(1:length(prediction$matchId))) {
        aktPred <- prediction[i, ]
        
        # Accuracy calculation
        predProb <- as.numeric(aktPred[as.character(aktPred$matchResult)])
        otherProbs <- aktPred[1:3][
            names(aktPred[1:3]) != as.character(aktPred$matchResult)]
        
        if(predProb > as.numeric(otherProbs[1]) &
               predProb > as.numeric(otherProbs[2])) {
            hitVec <- c(hitVec, TRUE)       
        }
        # No hit
        else {
            hitVec <- c(hitVec, FALSE)
        }
        
        aktComp <- comparison[comparison$matchId == aktPred$matchId, ]
        # No Booky odds on this match
        if(nrow(aktComp) == 0) {
            # TODO continue
        }
        
        possibleMatches <- possibleMatches + 1
        
        compProb <- as.numeric(aktComp[as.character(aktPred$matchResult)])
        
        valueDiffVec <- c(valueDiffVec, (predProb - compProb))
        
        # Bet on HomeVictory
        if(as.numeric(aktPred$HomeVictory) / as.numeric(aktComp$HomeVictory) >= 
               probRatioToBet) {
            # Bet won
            if(aktPred$matchResult == 'HomeVictory') {
                matchGain <- aktComp$homeOdd * stake - stake
            } 
            # Bet lost
            else {
                matchGain <- -stake
            }
            placedBets <- rbind(placedBets, 
                                data.frame(matchId = aktPred$matchId,
                                           matchResult = aktPred$matchResult,
                                           betOnOutcome = 'HomeVictory',
                                           predProb = as.numeric(aktPred$HomeVictory),
                                           bookyProb = as.numeric(aktComp$HomeVictory),
                                           stake = stake,
                                           gain = matchGain))
        }
        # Bet on VisitorsVictory
        if(as.numeric(aktPred$VisitorsVictory) / as.numeric(aktComp$VisitorsVictory) >= 
               probRatioToBet) {
            # Bet won
            if(aktPred$matchResult == 'VisitorsVictory') {
                matchGain <- aktComp$visitorsOdd * stake - stake
            } 
            # Bet lost
            else {
                matchGain <- -stake
            }
            placedBets <- rbind(placedBets, 
                                data.frame(matchId = aktPred$matchId,
                                           matchResult = aktPred$matchResult,
                                           betOnOutcome = 'VisitorsVictory',
                                           predProb = as.numeric(aktPred$VisitorsVictory),
                                           bookyProb = as.numeric(aktComp$VisitorsVictory),
                                           stake = stake,
                                           gain = matchGain))
        }
        # Bet on Draw
        if(as.numeric(aktPred$Draw) / as.numeric(aktComp$Draw) >= probRatioToBet) {
            # Bet won
            if(aktPred$matchResult == 'Draw') {
                matchGain <- aktComp$drawOdd * stake - stake
            } 
            # Bet lost
            else {
                matchGain <- -stake
            }
            placedBets <- rbind(placedBets, 
                                data.frame(matchId = aktPred$matchId,
                                           matchResult = aktPred$matchResult,
                                           betOnOutcome = 'Draw',
                                           predProb = as.numeric(aktPred$Draw),
                                           bookyProb = as.numeric(aktComp$Draw),
                                           stake = stake,
                                           gain = matchGain))
        }
    }
    valueDiff <- mean(valueDiffVec)
    accuracy <- mean(hitVec)
    stake <- sum(placedBets$stake)
    gain <- sum(placedBets$gain)
    
    list(valueDiff = valueDiff, accuracy = accuracy, stake = stake, gain = gain,
         placedBets = placedBets)
}

printEvaluation <- function(evalList) {
    print(paste('Stake:', evalList$stake))
    print(paste('Gain:', evalList$gain))
    print(paste('Gain [%]:', evalList$gain / evalList$stake))
    print(paste('Value Diff:', evalList$valueDiff))
    print(paste('Accuracy:', evalList$accuracy))
}