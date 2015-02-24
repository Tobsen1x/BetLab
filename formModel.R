# Enriches player stats with a predicted prematch form qualifier.
# Input data frame has to contain adjGrade.
# FormAlgorithm could be one of:
# - 'beta'
# - 'arima'
enrichForm <- function(playerStats, matchesToBet, formAlgorithm, minMatchdays, maxMatchdays, 
                       imputeBenchBy = NA, imputeNotPlayedBy = NA ) {
    playerStats$playerForm <- NA
    for(i in seq(1:nrow(matchesToBet))) {
        row <- matchesToBet[i, ]
        actPlayers <- filter(playerStats, matchId == row$matchId)
        for(j in seq(1:nrow(actPlayers))) {
            actPlayer <- actPlayers[j, ]
            # Simple form calculation
            if(formAlgorithm == 'beta') {
                form <- calcBetaForm(playerStats, actPlayer$playerId, actPlayer$season, 
                                     actPlayer$matchday, maxMatchdays = maxMatchdays, 
                                     minMatchdays = minMatchdays, 
                                     imputeBenchBy = imputeBenchBy, 
                                     imputeNotPlayedBy = imputeNotPlayedBy)
            } 
            # Time series arima form calculation
            else if(formAlgorithm == 'arima') {
                form <- calcTSForm(playerStats, actPlayer$playerId, actPlayer$season, 
                                   actPlayer$matchday, minMatchdays = minMatchdays, 
                                   imputeBenchBy = imputeBenchBy,
                                   imputeNotPlayedBy = imputeNotPlayedBy)
            } else {
                throw(paste('Illegal formAlgorithm argument:', formAlgorithm))
            }
            
            playerStats$playerForm[playerStats$playerId == actPlayer$playerId &
                                       playerStats$matchId == row$matchId] <- form
        }
    }
    
    #formData <- filter(playerStats, !is.na(playerForm), !is.na(adjGrade))
    
    #mseTrivial <- sqrt(1 / nrow(formData) * 
    #                       sum((formData$adjGrade - 0) ^ 2))
    
    #mseForm <- sqrt(1 / nrow(formData) *                     
    #                     sum((formData$adjGrade - 
    #                              formData$playerForm) ^ 2))
    
    #print(paste('MSE Trivial =', mseTrivial, '| MSE Form =', mseForm))
    playerStats
}

# Simple form calculation over the last few matches
calcBetaForm <- function(allStats, playerId, season, matchday, maxMatchdays, minMatchdays, 
                         imputeBenchBy = NA, imputeNotPlayedBy = NA) {
    matchCount <- maxMatchdays
    if(matchday <= maxMatchdays) {
        matchCount <- matchday - 1
    }
    
    playerMatches <- allStats[allStats$playerId == playerId & allStats$season == season &
                                allStats$matchday < matchday & 
                                allStats$matchday > matchday - (matchCount + 1), ]
    
    if(matchday <= minMatchdays | matchCount < 1) {
        return(NA)
    }
    
    formVector <- c()
    weightVector <- c()
    for(past in seq(1:matchCount)) {
        pastMatch <- playerMatches[playerMatches$matchday == (matchday - past), ]
        
        weight <- 1.5 - punif(q = past, 1, maxMatchdays)
        
        # Player was not deployed
        if(nrow(pastMatch) == 0) {
            form <- imputeNotPlayedBy
        } else {
            # Player didn't get a grade this match
            if(is.na(pastMatch$adjGrade)) {
                form <- imputeBenchBy
            } else {
                form <- pastMatch$adjGrade
            }            
        }
        
        if(is.na(form)) {
            form <- 0
            weight <- 0
        }
        
        # print(paste(paste('gew:', gew), paste('form:', form)))
        formVector <- c(formVector, form)
        weightVector <- c(weightVector, weight)
    }
    
    enumerator <- sum(formVector * weightVector)
    denominator <- sum(weightVector)
    if(denominator == 0) {
        return(NA)
    } 
    enumerator / denominator
}

# Calculates player form with time series analysis over the
# player performances of the past matches
calcTSForm <- function(allStats, playerId, season, matchday, minMatchdays, 
                       imputeBenchBy, imputeNotPlayedBy) {
    if(matchday <= minMatchdays) {
        return(NA)
    }
    
    playerMatches <- allStats[allStats$playerId == playerId & 
                                  allStats$season == season & allStats$matchday < matchday, ]
    
    formVector <- c()
    require(forecast)
    for(past in seq(1:(matchday - 1))) {
        pastMatch <- playerMatches[playerMatches$matchday == (matchday - past), ]
        
        # Player was not deployed
        if(nrow(pastMatch) == 0) {
            form <- imputeNotPlayedBy
        } else {
            # Player didn't get a grade this match
            if(is.na(pastMatch$adjGrade)) {
                form <- imputeBenchBy
            } else {
                form <- pastMatch$adjGrade
            }
        }
        formVector <- c(formVector, form)
    }
    
    # reverse Vector
    formVector <- rev(formVector)
    timeSeries <- ts(formVector)
    #plot(timeSeries)
    # ets needs relatively much data
    #expTsFit <- ets(timeSeries)
    
    autoArimaFit <- try(auto.arima(timeSeries))
    if(class(autoArimaFit) != 'try-error') {
        return(forecast(autoArimaFit, 1)$mean)
    } else {
        print(paste('ARIMA model not suitable for playerId', playerId, ' | season',
                    season, ' | matchday', matchday))
        return (NA)
    }
}