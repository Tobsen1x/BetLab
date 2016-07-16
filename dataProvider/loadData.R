loadTrainingData <- function(toMatchday, seasons, 
                             leagues, dbName = 'soccerlabdata', fitPriceImpute) {
    
    matches <- loadFinishedMatches(toMatchday = toMatchday, seasons = seasons, leagues = leagues)
    playerStats <- loadPlayerStats(matches$matchId, fitPriceImpute = fitPriceImpute)
    odds <- loadOdds(matches$matchId, 'SfStat')
    prices <- loadPrices(toMatchday, seasons, leagues)
    
    result <- list('stats' = playerStats, 'odds' = odds, 'matches' = matches, 'prices' = prices)
    return(result)
}

## returns the last price of a player before matchtime
getFitPrice <- function(player, prices, matchtime) {
    playerPrices <- filter(prices, playerId == player)
    # No price information for player
    if(nrow(playerPrices) == 0) {
        return(c(0, NA))
    }
    for(i in seq_len(nrow(playerPrices))) {
        price <- playerPrices[i, ]
        # playerPrices is sorted, so the first date which is in past of 
        # the match is returned
        if(price$informationDate - as.Date(matchtime) <= 0) {
            # Bei price = 0 wird der nächst neuere Preis zurück gegeben 
            #if(price$price == 0) {
                # der neuste Preis == 0
            #    if(i <= 1) {
            #        return(c(0, NA))
            #    } else {
            #        return(c(playerPrices[i - 1, ]$price, NA))
            #    }
            #}
            return(c(price$price, price$informationDate))
        }
    }
    
    # If no past price is found, the oldest one is returned
    #priceToReturn <- playerPrices[nrow(playerPrices), ]
    #return(c(priceToReturn$price, NA))
        
    # no past price is found
    return(c(0, NA))
}

loadPredictionData <- function(matchId, dbName = 'soccerlabdata2.0') {
  library(RMySQL)
  library(plyr)
  library(dplyr)
  con <- dbConnect(MySQL(), user="root", password="root",
                   dbname=dbName)
  
  homeProgLineupQuerry <- sprintf('select sp.id as matchId, prog.pos as position, 
                                  prog.progPreis as fitPrice, \'1\' as home 
                                  from spiel sp
                                  inner join aufstellung heimAuf on sp.heimAuf_id = heimAuf.id
                                  inner join aufstellung_progspieler aufProg on heimAuf.id = aufstellung_id
                                  inner join progspielerpreis prog on prog.id = progSpieler_id
                                  where sp.id = %i', matchId)
  
  visitorsProgLineupQuerry <- sprintf('select sp.id as matchId, prog.pos as position, 
                                      prog.progPreis as fitPrice, \'0\' as home 
                                      from spiel sp
                                      inner join aufstellung auswAuf on sp.auswAuf_id = auswAuf.id
                                      inner join aufstellung_progspieler aufProg on auswAuf.id = aufstellung_id
                                      inner join progspielerpreis prog on prog.id = progSpieler_id
                                      where sp.id = %i', matchId)
  
  playerStats <- dbGetQuery(con, homeProgLineupQuerry)
  playerStats <- rbind(playerStats, dbGetQuery(con, visitorsProgLineupQuerry))
  
  playerStats$position[playerStats$position == 'TW'] <- 'Torwart'
  playerStats$position[playerStats$position == 'IV'] <- 'Innenverteidiger'
  playerStats$position[playerStats$position == 'LV'] <- 'Linker Verteidiger'
  playerStats$position[playerStats$position == 'RV'] <- 'Rechter Verteidiger'
  playerStats$position[playerStats$position == 'DMIT'] <- 'Defensives Mittelfeld'
  playerStats$position[playerStats$position == 'ZMIT'] <- 'Zentrales Mittelfeld'
  playerStats$position[playerStats$position == 'LMIT'] <- 'Linkes Mittelfeld'
  playerStats$position[playerStats$position == 'RMIT'] <- 'Rechtes Mittelfeld'
  playerStats$position[playerStats$position == 'OMIT'] <- 'Offensives Mittelfeld'
  playerStats$position[playerStats$position == 'HS'] <- 'Haengende Spitze'
  playerStats$position[playerStats$position == 'MS'] <- 'Mittelstuermer'
  playerStats$position[playerStats$position == 'LS'] <- 'Linksaussen'
  playerStats$position[playerStats$position == 'RS'] <- 'Rechtsaussen'
  
  playerStats$playerAssignment <- 'DURCHGESPIELT'
  
  # Transformations
  playerStats <- transform(playerStats, 
                           playerAssignment = as.factor(playerAssignment),
                           home = as.logical(as.numeric(home)),
                           position = factor(position, c("Torwart", "Innenverteidiger",
                                                         "Linker Verteidiger", "Rechter Verteidiger",
                                                         "Defensives Mittelfeld", "Zentrales Mittelfeld",
                                                         "Linkes Mittelfeld", "Rechtes Mittelfeld",
                                                         "Offensives Mittelfeld", "Haengende Spitze",
                                                         "Mittelstuermer", "Linksaussen", "Rechtsaussen")))
  
  ## Reading matches
  matchQuery <- sprintf('select sp.id as matchId, 
                        sp.liga as league, sp.saison as season, sp.spieltag as matchday, 
                        sp.spielZeit as matchtime, sp.heimMan_id as homeTeamId, 
                        sp.auswMan_id as visitorsTeamId, sp.toreHeim as goalsHome,
                        sp.toreAusw as goalsVisitors
                        from spiel sp                    
                        where sp.id = %i', matchId)
  matches <- dbGetQuery(con, matchQuery)
  
  result <- list('stats' = playerStats, 'matches' = matches)
  return(result)
}