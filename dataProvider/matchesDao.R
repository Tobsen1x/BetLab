loadOdds <- function(matchIds, quelle, dbName = 'soccerlabdata') {
  matchIdStr <- paste(matchIds, sep = ',', collapse = ',')
  
  ### Reading Booky Odds
  oddQuery <- paste('select spiel_id as matchId, quoteHeim as homeOdd, 
    quoteAusw as visitorsOdd, quoteUnent as drawOdd
    from quote where quelle = \'', quelle, '\' and spiel_id in (', matchIdStr, ')', sep = '')
  con <- dbConnect(MySQL(), user="root", password="root",
                   dbname=dbName)
  odds <- dbGetQuery(con, oddQuery)
  dbDisconnect(con)
  
  # Enrich odds with probabilities
  odds$HomeVictory <- 1 / odds$homeOdd
  odds$VisitorsVictory <- 1 / odds$visitorsOdd
  odds$Draw <- 1 / odds$drawOdd
  
  odds <- filter(odds, !is.infinite(Draw) & 
                   !is.infinite(VisitorsVictory) & !is.infinite(HomeVictory))
  odds <- filter(odds, matchId %in% matchIds)
  
  return(odds)
}

loadFinishedMatches <- function(toMatchday, seasons, 
                                leagues, dbName = 'soccerlabdata') {
  fullSeasons <- seasons[-length(seasons)]
  lastSeason <- seasons[length(seasons)]
  seasonsStr <- paste('\'', fullSeasons, '\'', sep = '')
  seasonsStr <- do.call(paste, c(as.list(seasonsStr), sep = ','))
  leaguesStr <- paste('\'', leagues, '\'', sep = '')
  leaguesStr <- do.call(paste, c(as.list(leaguesStr), sep = ','))
  lastSeasonStr <- paste('\'', lastSeason, '\'', sep = '')
  
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
                      and sp.toreHeim is not null and sp.toreAusw is not null 
                      order by sp.spielZeit desc, sp.id asc', 
                        seasonsStr, lastSeasonStr, toMatchday, leaguesStr)
  con <- dbConnect(MySQL(), user="root", password="root",
                   dbname=dbName)
  matches <- dbGetQuery(con, matchQuery)
  dbDisconnect(con)
  
  matches <- mutate(matches, 
                    goalDiff = goalsHome - goalsVisitors,
                    matchResult = ifelse(goalDiff > 0, 'HomeVictory', ifelse(goalDiff < 0, 'VisitorsVictory', 'Draw')))
  matches <- mutate(matches, matchResult = 
                      factor(matchResult, levels = c('VisitorsVictory', 'Draw', 'HomeVictory'), 
                             labels = c('VisitorsVictory', 'Draw', 'HomeVictory'), 
                             ordered = TRUE))
  matches <- arrange(matches, season, matchday, matchtime, matchId)
  return(matches)
}