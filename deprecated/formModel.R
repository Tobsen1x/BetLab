# Enriches player stats with a predicted prematch form qualifier.
# Input data frame has to contain adjGrade.
enrichForm <- function(playerStats, matchesToBet, alpha = NULL,
                       restrictTimeline, imputeBenchBy = NA, imputeNotPlayedBy = NA) {
    require(forecast)
    require(dplyr)
    
    # For the new dynamic calculation, only
    playerFormImpute <- read.table('data/playerFormImpute.csv', sep = ';', 
                                   quote = '', header = TRUE)
    
    playerStats$meanf <- NA
    playerStats$expSmoothing <- NA
    playerStats$formMaxMatches <- NA
    playerStats$formPlayedMatches <- NA
    for(i in seq(1:nrow(matchesToBet))) {
        row <- matchesToBet[i, ]
        actPlayers <- playerStats[playerStats$matchId == row$matchId, ]
        for(j in seq(1:nrow(actPlayers))) {
            actPlayer <- actPlayers[j, ]
            
            if(is.na(actPlayer$fitPriceDate)) {
                next
            }
            
            # Time series forecasting
            forecasts <- calcTSForm(playerStats, actPlayer, matchesToBet,
                                    alpha = alpha,
                                    restrictTimeline = restrictTimeline,
                                    playerFormImpute = playerFormImpute,
                                    imputeBenchBy = imputeBenchBy,
                                    imputeNotPlayedBy = imputeNotPlayedBy)
            
            playerStats$meanf[playerStats$playerId == actPlayer$playerId &
                                  playerStats$matchId == row$matchId] <- forecasts$meanf
            playerStats$expSmoothing[playerStats$playerId == actPlayer$playerId &
                                         playerStats$matchId == row$matchId] <- forecasts$expSmoothing
            playerStats$formMaxMatches[playerStats$playerId == actPlayer$playerId &
                                           playerStats$matchId == row$matchId] <- forecasts$formMaxMatches
            playerStats$formPlayedMatches[playerStats$playerId == actPlayer$playerId &
                                              playerStats$matchId == row$matchId] <- forecasts$formPlayedMatches
        }
    }
    
    return(playerStats)
}

# Calculates player form with time series analysis over the
# player performances of the past matches
calcTSForm <- function(allStats, playerStat, matches, alpha, restrictTimeline,
                       playerFormImpute, imputeBenchBy, imputeNotPlayedBy) {
    mday <- playerStat$matchday
    
    if(playerStat$home) {
        team <- playerStat$homeTeamId
    } else {
        team <- playerStat$visitorsTeamId
    }
    
    
    if(restrictTimeline) {
        # Filter all past Playerstats for the regarding player, the actual season,
        # after the date of the fit price
        pastPlayerMatches <- allStats[allStats$playerId == playerStat$playerId & 
                                          allStats$season == playerStat$season &
                                          allStats$matchday < mday &
                                          allStats$matchtime > as.POSIXct(playerStat$fitPriceDate), ]
        
        relMatches <- matches[matches$season == playerStat$season &
                                  matches$matchday < playerStat$matchday &
                                  (matches$homeTeamId == team | matches$visitorsTeamId == team) &
                                  matches$matchtime > as.POSIXct(playerStat$fitPriceDate), ]
    } else {
        pastPlayerMatches <- allStats[allStats$playerId == playerStat$playerId & 
                                          allStats$season == playerStat$season &
                                          allStats$matchday < mday, ]
        
        relMatches <- matches[matches$season == playerStat$season &
                                  matches$matchday < playerStat$matchday &
                                  (matches$homeTeamId == team | matches$visitorsTeamId == team), ]
    }
    
    if(nrow(relMatches) == 0) {
        return(list(meanf = NA,
                    expSmoothing = NA,
                    formMaxMatches = 0,
                    formPlayedMatches = 0))
    }
    
    # Static version
    if(!is.na(imputeNotPlayedBy) & !is.na(imputeBenchBy)) {
        ts <- extractTimeSeriesByStaticImpute(pastPlayerMatches, relMatches, mday, 
                                              imputeNotPlayedBy,
                                              imputeBenchBy)
    } 
    # New version #### TODO adjust to newest changes
    else {
        teammateStats <- allStats[allStats$season == playerStat$season &
                                      allStats$matchday == mday &
                                      allStats$home == playerStat$home &
                                      allStats$playerId != playerStat$playerId &
                                      allStats$playerAssignment != 'BENCH', ]
        if(playerStat$home) {
            teammateStats <- teammateStats[teammateStats$homeTeamId == 
                                               playerStat$homeTeamId, ]
        } else {
            teammateStats <- teammateStats[teammateStats$visitorsTeamId == 
                                               playerStat$visitorsTeamId, ]
        }
        
        # New, dynamic version
        ts <- extractTimeSeries(pastPlayerMatches, teammateStats, 
                                playerStat, playerFormImpute)
    }
    
    timeSeries = ts$timeSeries
    #plot(timeSeries)
    
    expSmooth <- try(ses(timeSeries, h = 1, alpha = alpha)$mean, silent = TRUE)
    if(class(expSmooth) == "try-error") {
        expSmooth <- NA
    }  
    
    forecasts <- list(meanf = meanf(timeSeries,h=1)$mean,
                      expSmoothing = expSmooth,
                      formMaxMatches = ts$formMaxMatches,
                      formPlayedMatches = ts$formPlayedMatches)
    
    return(forecasts)
}

extractTimeSeriesByStaticImpute <- function(playerMatches, relMatches, mday, 
                                            imputeNotPlayedBy,
                                            imputeBenchBy) {
    formVector <- c()
    qualityVector <- c()
    minMatchday <- min(relMatches$matchday)
    
    # Iterates backwards over past matches of the player
    for(past in (mday - 1):minMatchday) {
        #pastMatch <- playerMatches %>% filter(matchday == (mday - past))
        pastMatch <- playerMatches[playerMatches$matchday == past, ]
        
        # Player was not deployed
        if(nrow(pastMatch) == 0) {
            formVector <- c(formVector, imputeNotPlayedBy)
            qualityVector <- c(qualityVector, FALSE)
        } else {
            # Player didn't get a grade this match
            if(is.na(pastMatch$adjGrade)) {
                formVector <- c(formVector, imputeBenchBy)
                qualityVector <- c(qualityVector, FALSE)
            } else {
                formVector <- c(formVector, pastMatch$adjGrade)
                qualityVector <- c(qualityVector, TRUE)
            }
        }
    }
    
    # reverse Vector
    formVector <- rev(formVector)
    timeSeries <- ts(formVector)
    
    formMax <- length(qualityVector)
    formPlayed <- sum(qualityVector)
    return(list(timeSeries = timeSeries, formMaxMatches = formMax, 
                formPlayedMatches = formPlayed))
}

extractTimeSeries <- function(pastPlayerMatches, teammateStats,
                              playerStat, playerFormImpute) {
    formVector <- c()
    formQuality <- c()
    
    # Iterates backwards over past matches of the player
    for(past in seq(1:(playerStat$matchday - 1))) {
        #pastMatch <- playerMatches %>% filter(matchday == (mday - past))
        pastMatch <- pastPlayerMatches[pastPlayerMatches$matchday == 
                                           (playerStat$matchday - past), ]
        
        form <- NA
        # Player has played and has got a grade
        if(nrow(pastMatch) > 0) {
            if(!is.na(pastMatch$adjGrade)) {
                form <- pastMatch$adjGrade
                quality <- TRUE
            }
        } 
        if(is.na(form)) {
            playerPrice <- playerStat$fitPrice
            if(is.na(playerPrice)) {
                playerPrice <- 25000
            }
            
            # Getting player position
            pos <- playerStat$transPos
            if(is.na(pos) & nrow(pastPlayerMatches) > 0) {
                for(ind in seq(1:nrow(pastPlayerMatches))) {
                    pos <- pastPlayerMatches[ind, 'transPos']
                    if(!is.na(pos)) {
                        break
                    }
                }
            }
            
            # Extract price to compare
            if(is.na(pos)) {
                comparisonPrice <- mean(teammateStats$fitPrice, na.rm = TRUE)
            } else {
                teammateOnPosition <- teammateStats[!is.na(teammateStats$transPos) &
                                                        teammateStats$transPos == pos, ]
                if(nrow(teammateOnPosition) > 0) {
                    comparisonPrice <- mean(teammateOnPosition$fitPrice, na.rm = TRUE)
                } else {
                    comparisonPrice <- mean(teammateStats$fitPrice, na.rm = TRUE)
                }
            }
            if(is.na(comparisonPrice) | is.nan(comparisonPrice)) {
                comparisonPrice <- mean(teammateStats$fitPrice, na.rm = TRUE)
            }
            
            playerPriceRate <- playerPrice / comparisonPrice
            
            if(nrow(pastMatch) > 0) {
                if(pastMatch$playerAssignment == 'BENCH') {
                    relImpute <- playerFormImpute[playerFormImpute$bench == TRUE, ]
                } else {
                    relImpute <- playerFormImpute[playerFormImpute$bench == FALSE, ]
                }
            } else {
                relImpute <- playerFormImpute[playerFormImpute$bench == FALSE, ]
            }
            
            value <- NA
            for(impIndex in seq(1:nrow(relImpute))) {
                aktImpute <- relImpute[impIndex, ]
                # DEBUG
                if(is.na(playerPriceRate) | is.na(aktImpute$min) |
                       is.na(aktImpute$max)) {
                    print(playerStat)
                }
                if(playerPriceRate >= aktImpute$min &
                       playerPriceRate < aktImpute$max) {
                    value <- aktImpute$value
                }
            }
            
            # DEBUG
            if(is.na(value)) {
                print(playerStat)
            }
            
            form <- value
            quality <- FALSE
        }
        formVector <- c(formVector, form)
        formQuality <- c(formQuality, quality)
    }
    
    # reverse Vector
    formVector <- rev(formVector)
    timeSeries <- ts(formVector)
    
    formQual <- sum(formQuality) / length(formQuality)
    return(list(timeSeries = timeSeries, formQuality = formQual))
}