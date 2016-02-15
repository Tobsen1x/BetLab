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

loadTrainingData <- function(toMatchday, seasons, 
                             leagues, dbName = 'soccerlabdata2.0') {
    require(RMySQL)
    library(plyr)
    library(dplyr)
    con <- dbConnect(MySQL(), user="root", password="root",
                     dbname=dbName)
    
    fullSeasons <- seasons[-length(seasons)]
    lastSeason <- seasons[length(seasons)]
    seasonsStr <- paste('\'', fullSeasons, '\'', sep = '')
    seasonsStr <- do.call(paste, c(as.list(seasonsStr), sep = ','))
    leaguesStr <- paste('\'', leagues, '\'', sep = '')
    leaguesStr <- do.call(paste, c(as.list(leaguesStr), sep = ','))
    lastSeasonStr <- paste('\'', lastSeason, '\'', sep = '')
    
    # Reading home starting eleven
    homeLineupStatsQuery <- sprintf('select sp.id as matchId, sp.liga as league, 
        sp.saison as season, sp.spieltag as matchday, sp.spielZeit as matchtime,
        sp.toreHeim as goalsHome, heimAuf.transFormation as formation,
        sp.toreAusw as goalsVisitors, stat.spieler_id as playerId,
        stat.transPos as position,
        stat.einsatz as playerAssignment, \'1\' as home,
        stat.note as grade
    from spiel sp 
        inner join aufstellung heimAuf on sp.heimAuf_id = heimAuf.id
        inner join aufstellung_spieler heimAufSp on heimAuf.id = heimAufSp.Aufstellung_id
        inner join spieler heimSpieler on heimAufSp.startElf_id = heimSpieler.id 
        inner join postspielerspielstatistik stat on sp.id = stat.spiel_id and heimSpieler.id = stat.spieler_id
    where (sp.saison in (%s) OR (sp.saison = %s and sp.spieltag <= %i)) and liga in (%s) 
        ', seasonsStr, lastSeasonStr, toMatchday, leaguesStr)
    
    # Reading home bench
    homeBenchStatsQuery <- sprintf('select sp.id as matchId, sp.liga as league, 
        sp.saison as season, sp.spieltag as matchday, sp.spielZeit as matchtime,
        sp.toreHeim as goalsHome, heimAuf.transFormation as formation,
        sp.toreAusw as goalsVisitors, stat.spieler_id as playerId,
        stat.transPos as position, 
        stat.einsatz as playerAssignment, \'1\' as home, stat.note as grade
    from spiel sp 
        inner join aufstellung heimAuf on sp.heimAuf_id = heimAuf.id
        inner join aufstellung_bench heimAufSp on heimAuf.id = heimAufSp.Aufstellung_id
        inner join spieler heimSpieler on heimAufSp.bench_id = heimSpieler.id 
        inner join postspielerspielstatistik stat on sp.id = stat.spiel_id and heimSpieler.id = stat.spieler_id
    where (sp.saison in (%s) OR (sp.saison = %s and sp.spieltag <= %i)) and liga in (%s) 
        ', seasonsStr, lastSeasonStr, toMatchday, leaguesStr)
    
    # Reading visitors starting eleven
    visitorsLineupStatsQuery <- sprintf('select sp.id as matchId, sp.liga as league, 
        sp.saison as season, sp.spieltag as matchday, sp.spielZeit as matchtime,
        sp.toreHeim as goalsHome, auswAuf.transFormation as formation,
        sp.toreAusw as goalsVisitors, stat.spieler_id as playerId,
        stat.transPos as position,
        stat.einsatz as playerAssignment, \'0\' as home, stat.note as grade
    from spiel sp 
        inner join aufstellung auswAuf on sp.auswAuf_id = auswAuf.id
        inner join aufstellung_spieler auswAufSp on auswAuf.id = auswAufSp.Aufstellung_id
        inner join spieler auswSpieler on auswAufSp.startElf_id = auswSpieler.id 
        inner join postspielerspielstatistik stat on sp.id = stat.spiel_id and auswSpieler.id = stat.spieler_id
    where (sp.saison in (%s) OR (sp.saison = %s and sp.spieltag <= %i)) and liga in (%s) 
        ', seasonsStr, lastSeasonStr, toMatchday, leaguesStr)
    
    # Reading visitors bench
    visitorsBenchStatsQuery <- sprintf('select sp.id as matchId, sp.liga as league, 
        sp.saison as season, sp.spieltag as matchday, sp.spielZeit as matchtime,
        sp.toreHeim as goalsHome, auswAuf.transFormation as formation,
        sp.toreAusw as goalsVisitors, stat.spieler_id as playerId,
        stat.transPos as position,
        stat.einsatz as playerAssignment, \'0\' as home, stat.note as grade
    from spiel sp 
        inner join aufstellung auswAuf on sp.auswAuf_id = auswAuf.id
        inner join aufstellung_bench auswAufSp on auswAuf.id = auswAufSp.Aufstellung_id
        inner join spieler auswSpieler on auswAufSp.bench_id = auswSpieler.id 
        inner join postspielerspielstatistik stat on sp.id = stat.spiel_id and auswSpieler.id = stat.spieler_id
    where (sp.saison in (%s) OR (sp.saison = %s and sp.spieltag <= %i)) and liga in (%s) 
        ', seasonsStr, lastSeasonStr, toMatchday, leaguesStr)
    
    playerStats <- dbGetQuery(con, homeLineupStatsQuery)
    playerStats <- rbind(playerStats, dbGetQuery(con, homeBenchStatsQuery))
    playerStats <- rbind(playerStats, dbGetQuery(con, visitorsLineupStatsQuery))
    playerStats <- rbind(playerStats, dbGetQuery(con, visitorsBenchStatsQuery))
    
    # replace Libero transPos with Innenverteidiger
    playerStats$position[playerStats$position == 'Libero'] = 'Innenverteidiger'
    playerStats$position[playerStats$position == 'Sturm'] = 'Mittelstuermer'
    playerStats$position[playerStats$position == 'Mittelfeld'] = 'Zentrales Mittelfeld'
    
    # Replace umlauts
    playerStats$position[grepl('ngende Spitze', playerStats$position)] <- 'Haengende Spitze'
    playerStats$position[grepl('Mittelst', playerStats$position)] <- 'Mittelstuermer'
    playerStats$position[grepl('Linksau', playerStats$position)] <- 'Linksaussen'
    playerStats$position[grepl('Rechtsau', playerStats$position)] <- 'Rechtsaussen'
    
    # Transform staticPosition
    #playerStats$staticPosition[playerStats$staticPosition == 'Abwehr - Innenverteidiger'] <- 'Innenverteidiger'
    #playerStats$staticPosition[playerStats$staticPosition == 'Abwehr - Rechter Verteidiger'] <- 'Rechter Verteidiger'
    #playerStats$staticPosition[playerStats$staticPosition == 'Abwehr - Linker Verteidiger'] <- 'Linker Verteidiger'
    #playerStats$staticPosition[playerStats$staticPosition == 'Abwehr'] <- 'InnenVerteidiger'
    #playerStats$staticPosition[playerStats$staticPosition == 'Mittelfeld - Defensives Mittelfeld'] <- 'Defensives Mittelfeld'
    #playerStats$staticPosition[playerStats$staticPosition == 'Mittelfeld - Zentrales Mittelfeld'] <- 'Zentrales Mittelfeld'
    #playerStats$staticPosition[playerStats$staticPosition == 'Mittelfeld - Rechtes Mittelfeld'] <- 'Rechtes Mittelfeld'
    #playerStats$staticPosition[playerStats$staticPosition == 'Mittelfeld - Linkes Mittelfeld'] <- 'Linkes Mittelfeld'
    #playerStats$staticPosition[grepl('Offensives Mittelfeld', playerStats$staticPosition)] <- 'Offensives Mittelfeld'
    #playerStats$staticPosition[playerStats$staticPosition == 'Mittelfeld'] <- 'Zentrales Mittelfeld'
    #playerStats$staticPosition[grepl('Mittelfeld - Linksau', playerStats$staticPosition)] <- 'Linkes Mittelfeld'
    #playerStats$staticPosition[grepl('Mittelfeld - Rechtsau', playerStats$staticPosition)] <- 'Rechtes Mittelfeld'
    #playerStats$staticPosition[grepl('ngende Spitze', playerStats$staticPosition)] <- 'Haengende Spitze'
    #playerStats$staticPosition[grepl('Sturm - Mittelst', playerStats$staticPosition)] <- 'Mittelstuermer'
    #playerStats$staticPosition[grepl('Sturm - Linksau', playerStats$staticPosition)] <- 'Linksaussen'
    #playerStats$staticPosition[grepl('Sturm - Rechtsau', playerStats$staticPosition)] <- 'Rechtsaussen'
    #playerStats$staticPosition[playerStats$staticPosition == 'Sturm'] <- 'Mittelstuermer'
    
    positionLevels <- c("Torwart", "Innenverteidiger",
                        "Linker Verteidiger", "Rechter Verteidiger",
                        "Defensives Mittelfeld", "Zentrales Mittelfeld",
                        "Linkes Mittelfeld", "Rechtes Mittelfeld",
                        "Offensives Mittelfeld", "Haengende Spitze",
                        "Mittelstuermer", "Linksaussen", "Rechtsaussen")
    
    # Transformations
    playerStats <- transform(playerStats, league = as.factor(league),
                             matchtime = as.POSIXct(strptime(matchtime, '%Y-%m-%d %H:%M:%S')),
                             playerAssignment = as.factor(playerAssignment),
                             formation = as.factor(formation),
                             home = as.logical(as.numeric(home)),
                             position = factor(position, positionLevels))#,
                             #staticPosition = factor(staticPosition, positionLevels))
    
   #playerStats$staticPosition[is.na(playerStats$staticPosition)] <- 'Zentrales Mittelfeld'
    
    ### Reading player prices
    priceQuery <- 'select sp.id as playerId, preis.informationDate, 
        preis.preis as price 
    from marktpreis preis
        inner join spieler_marktpreis spPreis on preis.id = spPreis.marktpreise_id
        inner join spieler sp on spPreis.Spieler_id = sp.id 
    order by sp.id, preis.informationDate desc'
    
    prices <- dbGetQuery(con, priceQuery)
    prices <- transform(prices, informationDate = 
                            as.Date(informationDate, format = '%Y-%m-%d'))
    
    # Refining player statistics with price and information date
    
    mergedData <- dplyr:::full_join(playerStats, prices, by = c('playerId' = 'playerId'))
    filteredMergedData <- dplyr:::filter(mergedData, as.Date(matchtime) > as.Date(informationDate))
    groupedData <- group_by(filteredMergedData, playerId, matchId)
    sumData <- summarise(groupedData, dateMax = max(informationDate))
    secMergedData <- dplyr:::inner_join(sumData, prices, by = c('playerId' = 'playerId', 'dateMax' = 'informationDate'))
    finData <- dplyr:::left_join(playerStats, secMergedData, by = c('playerId' = 'playerId', 'matchId' = 'matchId'))
    playerStats <- dplyr:::rename(finData, fitPrice = price, fitPriceDate = dateMax)
    
    #TEST
    #finData <- arrange(finData, matchId, home, position, playerId)
    #examples <- sample_n(finData, size = 10)
    #View(examples)
    #paste(examples$playerId, sep = ',')
    
    # Old way
    #playerStats$fitPrice <- NA
    #playerStats$fitPriceDate <- NA
    #for(i in 1:nrow(playerStats)) {
    #    row <- playerStats[i, ]
    #    price <- getFitPrice(player = row$playerId, prices, matchtime = row$matchtime)
    #    playerStats[i, ]$fitPrice <- price[1]
    #    playerStats[i, ]$fitPriceDate <- price[2]
    #}
    
    playerStats$fitPriceDate <- as.Date(playerStats$fitPriceDate, 
                                        origin = "1970-01-01")
    
    
    ### Reading Booky Odds
    oddQuery <- 'select spiel_id as matchId, quoteHeim as homeOdd, 
        quoteAusw as visitorsOdd, quoteUnent as drawOdd
    from quote where quelle = \'SfStat\'';
    odds <- dbGetQuery(con, oddQuery)
    
    # Enrich odds with probabilities
    odds$HomeVictory <- 1 / odds$homeOdd
    odds$VisitorsVictory <- 1 / odds$visitorsOdd
    odds$Draw <- 1 / odds$drawOdd
    
    odds <- filter(odds, !is.infinite(Draw) & 
                       !is.infinite(VisitorsVictory) & !is.infinite(HomeVictory))
    
    ## Reading matches
    matchQuery <- sprintf('select sp.id as matchId, 
        sp.liga as league, sp.saison as season, sp.spieltag as matchday, 
        sp.spielZeit as matchtime, sp.heimMan_id as homeTeamId, 
        sp.auswMan_id as visitorsTeamId, 
        heimAuf.transFormation as heimFormation,
        auswAuf.transFormation as auswFormation,
        sp.toreHeim as goalsHome,
        sp.toreAusw as goalsVisitors
    from spiel sp
        inner join aufstellung heimAuf on sp.heimAuf_id = heimAuf.id
        inner join aufstellung auswAuf on sp.auswAuf_id = auswAuf.id
    where (sp.saison in (%s) OR (sp.saison = %s and sp.spieltag <= %i)) and liga in (%s)  
        order by sp.spielZeit desc, sp.id asc', 
                          seasonsStr, lastSeasonStr, toMatchday, leaguesStr)
    matches <- dbGetQuery(con, matchQuery)
    
    matches <- mutate(matches, 
                      goalDiff = goalsHome - goalsVisitors,
                      matchResult = ifelse(goalDiff > 0, 'HomeVictory', ifelse(goalDiff < 0, 'VisitorsVictory', 'Draw')))
    matches <- mutate(matches, matchResult = 
                             factor(matchResult, levels = c('VisitorsVictory', 'Draw', 'HomeVictory'), 
                                    labels = c('VisitorsVictory', 'Draw', 'HomeVictory'), 
                                    ordered = TRUE))
    matches <- arrange(matches, season, matchday, matchtime, matchId)
    
    
    result <- list('stats' = playerStats, 'prices' = prices, 'odds' = odds,
                   'matches' = matches)
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