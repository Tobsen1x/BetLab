# Enriches player stats with a predicted prematch form qualifier.
# Input data frame has to contain adjGrade.
enrichForm <- function(playerStats, matchesToBet, minMatchdays, 
                       imputeBenchBy = NA, imputeNotPlayedBy = NA) {
    require(forecast)
    require(dplyr)
    
    playerFormImpute <- read.table('data/playerFormImpute.csv', sep = ';', 
                                   quote = '', header = TRUE)
    
    playerStats$meanf <- NA
    playerStats$sesOptimal <- NA
    playerStats$sesSimple <- NA
    playerStats$formQuality <- NA
    for(i in seq(1:nrow(matchesToBet))) {
        row <- matchesToBet[i, ]
        actPlayers <- playerStats[playerStats$matchId == row$matchId, ]
        for(j in seq(1:nrow(actPlayers))) {
            actPlayer <- actPlayers[j, ]
            
            # Time series forecasting
            forecasts <- calcTSForm(playerStats, actPlayer,
                                    minMatchdays = minMatchdays, 
                                    playerFormImpute = playerFormImpute,
                                    imputeBenchBy = imputeBenchBy,
                                    imputeNotPlayedBy = imputeNotPlayedBy)
            
            playerStats$meanf[playerStats$playerId == actPlayer$playerId &
                                  playerStats$matchId == row$matchId] <- forecasts$meanf
            playerStats$sesOptimal[playerStats$playerId == actPlayer$playerId &
                                       playerStats$matchId == row$matchId] <- forecasts$sesOptimal
            playerStats$sesSimple[playerStats$playerId == actPlayer$playerId &
                                      playerStats$matchId == row$matchId] <- forecasts$sesSimple
            playerStats$formQuality[playerStats$playerId == actPlayer$playerId &
                                      playerStats$matchId == row$matchId] <- forecasts$formQuality
        }
    }
    
    return(playerStats)
}

# Calculates player form with time series analysis over the
# player performances of the past matches
calcTSForm <- function(allStats, playerStat, minMatchdays, 
                       playerFormImpute, imputeBenchBy, imputeNotPlayedBy) {
    mday <- playerStat$matchday
    if(mday <= minMatchdays) {
        return(list(meanf = NA,
                    sesOptimal = NA,
                    sesSimple = NA,
                    formQuality = NA))
    }
    
    #playerMatches <- allStats %>% filter(playerId == pId, season == sea,
    #                                     matchday < mday)
    pastPlayerMatches <- allStats[allStats$playerId == playerStat$playerId & 
                                      allStats$season == playerStat$season &
                                      allStats$matchday < mday, ]
    pastPlayerMatches <- pastPlayerMatches[order(-pastPlayerMatches$matchday), ]
    
    # Old, static version
    #timeSeries <- extractTimeSeriesByStaticImpute(playerMatches, mday, 
    #                                              imputeNotPlayedBy,
    #                                              imputeBenchBy)
    
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
    
    # New, dynamix version
    ts <- extractTimeSeries(pastPlayerMatches, teammateStats, 
                                    playerStat, playerFormImpute)
    
    timeSeries = ts$timeSeries
    #plot(timeSeries)
    
    forecasts <- list(meanf = meanf(timeSeries,h=1)$mean,
                      sesOptimal = ses(timeSeries, h = 1, initial = 'optimal')$mean,
                      sesSimple = ses(timeSeries, h = 1, initial = 'simple')$mean,
                      formQuality = ts$formQuality)
    
    return(forecasts)
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

extractTimeSeriesByStaticImpute <- function(playerMatches, mday, 
                                            imputeNotPlayedBy,
                                            imputeBenchBy) {
    formVector <- c()
    
    # Iterates backwards over past matches of the player
    for(past in seq(1:(mday - 1))) {
        #pastMatch <- playerMatches %>% filter(matchday == (mday - past))
        pastMatch <- playerMatches[playerMatches$matchday == (mday - past), ]
        
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
}