loadData <- function(league, dbName = 'soccerlabdata1.0', verbose = FALSE) {
    require(RMySQL)
    con <- dbConnect(MySQL(), user="root", password="root",
                     dbname=dbName)
    
    ## Reading Match statistics for each player
    homeLineupStatsQuery <- paste('select sp.id as matchId, sp.liga as league, 
        sp.saison as season, sp.spieltag as matchday, sp.spielZeit as matchtime,
        sp.heimMan_id as homeTeamId, sp.auswMan_id as visitorsTeamId, 
        sp.toreHeim as goalsHome, sp.toreAusw as goalsVisitors, auf.kickerFormation,
        auf.transFormation, spieler.id as playerId, spieler.kickerName, 
        spieler.transName, spieler.kickerPosition, 
        spielerStat.einsatz as playerAssignment, 
        spielerStat.kickerNote as kickerGrade, spielerStat.transNote as transGrade, 
        spielerStat.transPos, \'1\' as home
    from spiel sp 
    inner join aufstellung auf on sp.heimAuf_id = auf.id
    inner join aufstellung_spieler aufSp on auf.id = aufSp.Aufstellung_id
    inner join spieler spieler on aufSp.startElf_id = spieler.id 
    inner join postspielerspielstatistik spielerStat on sp.id = spielerStat.spiel_id 
        and spieler.id = spielerStat.spieler_id
    where sp.liga = \'', league, '\'', sep = '')
    
    homeBenchStatsQuery <- paste('select sp.id as matchId, sp.liga as league, 
        sp.saison as season, sp.spieltag as matchday, sp.spielZeit as matchtime,
        sp.heimMan_id as homeTeamId, sp.auswMan_id as visitorsTeamId, 
        sp.toreHeim as goalsHome, sp.toreAusw as goalsVisitors, 
        auf.kickerFormation, auf.transFormation, spieler.id as playerId, 
        spieler.kickerName, spieler.transName, spieler.kickerPosition,
        spielerStat.einsatz as playerAssignment, 
        spielerStat.kickerNote as kickerGrade, 
        spielerStat.transNote as transGrade, spielerStat.transPos, \'1\' as home
    from spiel sp 
    inner join aufstellung auf on sp.heimAuf_id = auf.id
    inner join aufstellung_bench aufSp on auf.id = aufSp.Aufstellung_id
    inner join spieler spieler on aufSp.bench_id = spieler.id 
    inner join postspielerspielstatistik spielerStat on sp.id = spielerStat.spiel_id 
        and spieler.id = spielerStat.spieler_id
    where sp.liga = \'', league, '\'', sep = '')
    
    visitorsLineupStatsQuery <- paste('select sp.id as matchId, 
        sp.liga as league, sp.saison as season, sp.spieltag as matchday, 
        sp.spielZeit as matchtime, sp.heimMan_id as homeTeamId, 
        sp.auswMan_id as visitorsTeamId, sp.toreHeim as goalsHome,
        sp.toreAusw as goalsVisitors, auf.kickerFormation, auf.transFormation, 
        spieler.id as playerId, spieler.kickerName, spieler.transName, 
        spieler.kickerPosition, spielerStat.einsatz as playerAssignment, 
        spielerStat.kickerNote as kickerGrade, 
        spielerStat.transNote as transGrade, spielerStat.transPos, \'0\' as home
    from spiel sp 
    inner join aufstellung auf on sp.auswAuf_id = auf.id
    inner join aufstellung_spieler aufSp on auf.id = aufSp.Aufstellung_id
    inner join spieler spieler on aufSp.startElf_id = spieler.id 
    inner join postspielerspielstatistik spielerStat on sp.id = spielerStat.spiel_id 
        and spieler.id = spielerStat.spieler_id
    where sp.liga = \'', league, '\'', sep = '')
    
    visitorsBenchStatsQuery <- paste('select sp.id as matchId, 
        sp.liga as league, sp.saison as season, sp.spieltag as matchday, 
        sp.spielZeit as matchtime, sp.heimMan_id as homeTeamId, 
        sp.auswMan_id as visitorsTeamId, sp.toreHeim as goalsHome,
        sp.toreAusw as goalsVisitors, auf.kickerFormation, auf.transFormation, 
        spieler.id as playerId, spieler.kickerName, spieler.transName, 
        spieler.kickerPosition, spielerStat.einsatz as playerAssignment, 
        spielerStat.kickerNote as kickerGrade, 
        spielerStat.transNote as transGrade, spielerStat.transPos, \'0\' as home
    from spiel sp 
    inner join aufstellung auf on sp.auswAuf_id = auf.id
    inner join aufstellung_bench aufSp on auf.id = aufSp.Aufstellung_id
    inner join spieler spieler on aufSp.bench_id = spieler.id 
    inner join postspielerspielstatistik spielerStat on sp.id = spielerStat.spiel_id 
        and spieler.id = spielerStat.spieler_id
    where sp.liga = \'', league, '\'', sep = '')
    
    playerStats <- dbGetQuery(con, homeLineupStatsQuery)
    playerStats <- rbind(playerStats, dbGetQuery(con, homeBenchStatsQuery))
    playerStats <- rbind(playerStats, dbGetQuery(con, visitorsLineupStatsQuery))
    playerStats <- rbind(playerStats, dbGetQuery(con, visitorsBenchStatsQuery))
    
    # replace Libero transPos with Innenverteidiger
    playerStats$transPos[playerStats$transPos == 'Libero'] = 'Innenverteidiger'
    
    # Transformations
    playerStats <- transform(playerStats, league = as.factor(league),
                             matchtime = as.POSIXct(strptime(matchtime, '%Y-%m-%d %H:%M:%S')),
                             kickerPosition = as.factor(kickerPosition),
                             playerAssignment = as.factor(playerAssignment),
                             transFormation = as.factor(transFormation),
                             transPos = factor(transPos, c("Torwart", "Innenverteidiger",
                                                           "Linker Verteidiger", "Rechter Verteidiger",
                                                           "Defensives Mittelfeld", "Zentrales Mittelfeld",
                                                           "Linkes Mittelfeld", "Rechtes Mittelfeld",
                                                           "Offensives Mittelfeld", "Hängende Spitze",
                                                           "Mittelstürmer", "Linksaußen", "Rechtsaußen")),
                             home = as.logical(as.numeric(home)))
    
    ## Reading player prices
    priceQuery <- 'select sp.id as playerId, preis.informationDate, 
        preis.preis as price 
    from marktpreis preis
    inner join spieler_marktpreis spPreis on preis.id = spPreis.marktpreise_id
    inner join spieler sp on spPreis.Spieler_id = sp.id 
        order by sp.id, preis.informationDate desc'
    
    prices <- dbGetQuery(con, priceQuery)
    prices <- transform(prices, informationDate = 
                            as.Date(informationDate, format = '%Y-%m-%d'))
    
    ## Reading Booky Odds
    oddQuery <- 'select spiel_id as matchId, quoteHeim as homeOdd, 
        quoteAusw as visitorsOdd, quoteUnent as drawOdd
    from quote where quelle = \'SfStat\'';
    odds <- dbGetQuery(con, oddQuery)
    
    # Enrich odds with probabilities
    odds$HomeVictory <- 1 / odds$homeOdd
    odds$VisitorsVictory <- 1 / odds$visitorsOdd
    odds$Draw <- 1 / odds$drawOdd
    
    ## Reading matches
    matchQuery <- paste('select sp.id as matchId, 
        sp.liga as league, sp.saison as season, sp.spieltag as matchday, 
        sp.spielZeit as matchtime, sp.heimMan_id as homeTeamId, 
    sp.auswMan_id as visitorsTeamId, sp.toreHeim as goalsHome,
    sp.toreAusw as goalsVisitors,
    stat.hChancen as homeChances, stat.aChancen as visitorsChances, 
    stat.hEcken as homeCorners, stat.aEcken as visitorsCorners, 
    stat.spielnote as matchGrade, stat.spielerDesSpiels_id as matchWinnerId
    from spiel sp 
    inner join postspielstatistik stat on stat.id = sp.postSpielStatistik_id                    
    where sp.liga = \'', league, '\'', ' order by sp.spielZeit desc', 
                        sep = '')
    matches <- dbGetQuery(con, matchQuery)
    require(dplyr)
    matches <- mutate(matches, 
                  goalDiff = goalsHome - goalsVisitors,
                  matchResult = ifelse(goalDiff > 0, 1, ifelse(goalDiff < 0, 2, 0)))
    matches <- transform(matches, matchResult = 
                             factor(matchResult, levels = c(2, 0, 1), 
                                    labels = c('VisitorsVictory', 'Draw', 'HomeVictory'), 
                                    ordered = TRUE))
    matches <- arrange(matches, season, matchday, matchtime, matchId)
    
    dbDisconnect(con)
    
    #-----------------------------------------------------------------
         
    # Refining player statistics with price and information date
    playerStats$fitPrice <- NA
    playerStats$fitPriceDate <- NA
    for(i in 1:nrow(playerStats)) {
        row <- playerStats[i, ]
        price <- getFitPrice(player = row$playerId, prices, matchtime = row$matchtime, 
                             verbose = verbose)
        if(!is.na(price[1]) & price[1] == 0) {
            playerStats[i, ]$fitPrice <- 25000
        } else {
            playerStats[i, ]$fitPrice <- price[1]
        }
        
        playerStats[i, ]$fitPriceDate <- price[2]
    }
    
    playerStats$fitPriceDate <- as.Date(playerStats$fitPriceDate, 
                                        origin = "1970-01-01")
    list(playerStats = playerStats, prices = prices, odds = odds, matches = matches)
}

## returns the last price of a player before matchtime
getFitPrice <- function(player, prices, matchtime, verbose = FALSE) {
    playerPrices <- subset(prices, playerId == player)
    # No price information for player
    if(nrow(playerPrices) == 0) {
        if(verbose == TRUE) {
            print(paste('No price information for player with ID', player))
        }
        return(c(NA, NA))
    }
    for(i in seq_len(nrow(playerPrices))) {
        price <- playerPrices[i, ]
        # playerPrices is sorted, so the first date which is in past of 
        # the match is returned
        if(price$informationDate - as.Date(matchtime) <= 0) {
            return(c(price$price, price$informationDate))
        }
    }
    # If no past price is found, the oldest one is returned
    priceToReturn <- playerPrices[nrow(playerPrices), ]
    if(verbose == TRUE) {
        print(paste('No past price for player', player, 'and matchtime', 
                    matchtime, 'is found, so the oldest price is returned:', 
                    priceToReturn$price))
    }
    return(c(priceToReturn$price, priceToReturn$informationDate))
}