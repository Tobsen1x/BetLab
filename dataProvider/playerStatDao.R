loadRelevantPlayers <- function(toMatchday, seasons, 
                                leagues, dbName = 'soccerlabdata') {
  matches <- loadFinishedMatches(toMatchday = toMatchday, seasons = seasons, leagues = leagues)
  playerStats <- loadPlayerStats(matches$matchId, fitPriceImpute = 0)
  return(unique(playerStats$playerId))
}

loadPrices <- function(toMatchday, seasons, leagues, dbName = 'soccerlabdata') {
  con <- dbConnect(MySQL(), user="root", password="root",
                   dbname=dbName)
  relPlayers <- loadRelevantPlayers(toMatchday, seasons, leagues)
  playerIdsStr <- do.call(paste, c(as.list(paste(relPlayers, sep = '')), sep = ','))
  
  pricesQuery <- sprintf('select sm.Spieler_id as playerId, m.informationDate as informationDate, 
                          m.preis as price, sp.geburtstag as birthday 
                  from marktpreis m
                  inner join spieler_marktpreis sm on sm.marktpreise_id = m.id
                  inner join spieler sp on sp.id = sm.spieler_id
                  where sm.Spieler_id in (%s)', playerIdsStr)
  prices <- dbGetQuery(con, pricesQuery)
  dbDisconnect(con)
  prices <- mutate(prices, 
                   informationDate = as.Date(informationDate, format = '%Y-%m-%d'),
                   birthday = as.Date(birthday, format = '%Y-%m-%d'))
  
  return(prices)
}


loadPlayerStats <- function(matchIds, fitPriceImpute, dbName = 'soccerlabdata') {
  con <- dbConnect(MySQL(), user="root", password="root",
                   dbname=dbName)

  matchIdStr <- paste(matchIds, sep = ',', collapse = ',')
  # Reading home starting eleven
  homeLineupStatsQuery <- paste('select sp.id as matchId, sp.liga as league, 
                                  sp.saison as season, sp.spieltag as matchday, sp.spielZeit as matchtime,
                                  sp.toreHeim as goalsHome, heimAuf.transFormation as formation,
                                  sp.toreAusw as goalsVisitors, stat.spieler_id as playerId,
                                  stat.transPos as position,
                                  stat.einsatz as playerAssignment, \'1\' as home,
                                  stat.note as grade,
                                  heimSpieler.geburtstag as birthday
                                  from spiel sp 
                                  inner join aufstellung heimAuf on sp.heimAuf_id = heimAuf.id
                                  inner join aufstellung_spieler heimAufSp on heimAuf.id = heimAufSp.Aufstellung_id
                                  inner join spieler heimSpieler on heimAufSp.startElf_id = heimSpieler.id 
                                  inner join postspielerspielstatistik stat on sp.id = stat.spiel_id and heimSpieler.id = stat.spieler_id 
                                  where sp.id in (', matchIdStr, ')', sep = '')
  
  # Reading home bench
  homeBenchStatsQuery <- paste('select sp.id as matchId, sp.liga as league, 
                                 sp.saison as season, sp.spieltag as matchday, sp.spielZeit as matchtime,
                                 sp.toreHeim as goalsHome, heimAuf.transFormation as formation,
                                 sp.toreAusw as goalsVisitors, stat.spieler_id as playerId,
                                 stat.transPos as position, 
                                 stat.einsatz as playerAssignment, \'1\' as home, stat.note as grade,
                                 heimSpieler.geburtstag as birthday
                                 from spiel sp 
                                 inner join aufstellung heimAuf on sp.heimAuf_id = heimAuf.id
                                 inner join aufstellung_bench heimAufSp on heimAuf.id = heimAufSp.Aufstellung_id
                                 inner join spieler heimSpieler on heimAufSp.bench_id = heimSpieler.id 
                                 inner join postspielerspielstatistik stat on sp.id = stat.spiel_id and heimSpieler.id = stat.spieler_id 
                                 where sp.id in (', matchIdStr, ')', sep = '')
  
  # Reading visitors starting eleven
  visitorsLineupStatsQuery <- paste('select sp.id as matchId, sp.liga as league, 
                                      sp.saison as season, sp.spieltag as matchday, sp.spielZeit as matchtime,
                                      sp.toreHeim as goalsHome, auswAuf.transFormation as formation,
                                      sp.toreAusw as goalsVisitors, stat.spieler_id as playerId,
                                      stat.transPos as position,
                                      stat.einsatz as playerAssignment, \'0\' as home, stat.note as grade,
                                      auswSpieler.geburtstag as birthday
                                      from spiel sp 
                                      inner join aufstellung auswAuf on sp.auswAuf_id = auswAuf.id
                                      inner join aufstellung_spieler auswAufSp on auswAuf.id = auswAufSp.Aufstellung_id
                                      inner join spieler auswSpieler on auswAufSp.startElf_id = auswSpieler.id 
                                      inner join postspielerspielstatistik stat on sp.id = stat.spiel_id and auswSpieler.id = stat.spieler_id 
                                      where sp.id in (', matchIdStr, ')', sep = '')
  
  # Reading visitors bench
  visitorsBenchStatsQuery <- paste('select sp.id as matchId, sp.liga as league, 
                                     sp.saison as season, sp.spieltag as matchday, sp.spielZeit as matchtime,
                                     sp.toreHeim as goalsHome, auswAuf.transFormation as formation,
                                     sp.toreAusw as goalsVisitors, stat.spieler_id as playerId,
                                     stat.transPos as position,
                                     stat.einsatz as playerAssignment, \'0\' as home, stat.note as grade,
                                     auswSpieler.geburtstag as birthday
                                     from spiel sp 
                                     inner join aufstellung auswAuf on sp.auswAuf_id = auswAuf.id
                                     inner join aufstellung_bench auswAufSp on auswAuf.id = auswAufSp.Aufstellung_id
                                     inner join spieler auswSpieler on auswAufSp.bench_id = auswSpieler.id 
                                     inner join postspielerspielstatistik stat on sp.id = stat.spiel_id and auswSpieler.id = stat.spieler_id 
                                     where sp.id in (', matchIdStr, ')', sep = '')
  
  playerStats <- dbGetQuery(con, homeLineupStatsQuery)
  playerStats <- rbind(playerStats, dbGetQuery(con, homeBenchStatsQuery))
  playerStats <- rbind(playerStats, dbGetQuery(con, visitorsLineupStatsQuery))
  playerStats <- rbind(playerStats, dbGetQuery(con, visitorsBenchStatsQuery))
  
  
  # Remove players who wrongly play in the same match in both teams
  statsGroup <- group_by(playerStats, matchId, playerId)
  statsSum <- summarise(statsGroup, sum = n())
  multiStats <- filter(statsSum, sum != 1)
  multiStats <- filter(playerStats, matchId %in% multiStats$matchId, playerId %in% multiStats$playerId)
  # remove home Entries
  playerStats <- filter(playerStats, !(matchId %in% multiStats$matchId &
                                         playerId %in% multiStats$playerId & home == 1))
  
  
  # replace Libero transPos with Innenverteidiger
  playerStats$position[playerStats$position == 'Libero'] = 'Innenverteidiger'
  playerStats$position[playerStats$position == 'Sturm'] = 'Mittelstuermer'
  playerStats$position[playerStats$position == 'Mittelfeld'] = 'Zentrales Mittelfeld'
  
  # Replace umlauts
  playerStats$position[grepl('ngende Spitze', playerStats$position)] <- 'Haengende Spitze'
  playerStats$position[grepl('Mittelst', playerStats$position)] <- 'Mittelstuermer'
  playerStats$position[grepl('Linksau', playerStats$position)] <- 'Linksaussen'
  playerStats$position[grepl('Rechtsau', playerStats$position)] <- 'Rechtsaussen'
  
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
                           position = factor(position, positionLevels),
                           birthday = as.Date(birthday))
  
  ### Reading player prices
  priceQuery <- 'select sp.id as playerId, preis.informationDate, 
  preis.preis as price
  from marktpreis preis
  inner join spieler_marktpreis spPreis on preis.id = spPreis.marktpreise_id
  inner join spieler sp on spPreis.Spieler_id = sp.id 
  order by sp.id, preis.informationDate desc'
  
  prices <- dbGetQuery(con, priceQuery)
  dbDisconnect(con)
  
  prices <- transform(prices, informationDate = as.Date(informationDate, format = '%Y-%m-%d'))
  # Refining player statistics with price and information date
  
  mergedData <- dplyr:::full_join(playerStats, prices, by = c('playerId' = 'playerId'))
  filteredMergedData <- dplyr:::filter(mergedData, as.Date(matchtime) > as.Date(informationDate))
  groupedData <- group_by(filteredMergedData, playerId, matchId)
  sumData <- summarise(groupedData, dateMax = max(informationDate))
  secMergedData <- dplyr:::inner_join(sumData, prices, by = c('playerId' = 'playerId', 'dateMax' = 'informationDate'))
  finData <- dplyr:::left_join(playerStats, secMergedData, by = c('playerId' = 'playerId', 'matchId' = 'matchId'))
  playerStats <- dplyr:::rename(finData, fitPrice = price, fitPriceDate = dateMax)
  
  
  playerStats$fitPriceDate <- as.Date(playerStats$fitPriceDate, 
                                      origin = "1970-01-01")
  
  ## Impute fit price
  playerStats$fitPrice[is.na(playerStats$fitPrice)] <- fitPriceImpute
  playerStats$fitPriceDate[is.na(playerStats$fitPriceDate)] <- as.Date('2004-06-01', origin = "1970-01-01")
  
  return(playerStats)
}