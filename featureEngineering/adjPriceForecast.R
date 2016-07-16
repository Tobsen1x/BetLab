enrichStatsWithAdjPriceForecast <- function(toMatchday, seasons, leagues, fitPriceImpute) {
  data <- loadTrainingData(toMatchday = toMatchday, seasons = seasons, 
                           leagues = leagues, fitPriceImpute = fitPriceImpute)
  odds <- data$odds
  stats <- data$stats
  
  assignedPositions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 
                         'mid', 'off', 'off', 'off', 'off', 'off')
  groupedPos <- groupPositions(stats$position, assignedPositions)
  stats$groupedPosition <- groupedPos
  stats <- data.table(stats)
  setkey(stats, playerId, matchtime)
  
  matches <- data$matches
  matches <- data.table(matches)
  setkey(matches, matchId, season, matchday)
  
  prices <- data$prices
  prices <- filter(prices, informationDate > as.Date('2005-03-01', format = '%Y-%m-%d'))
  prices <- arrange(prices, playerId, informationDate)
  prices <- data.table(prices)
  setkey(prices, playerId)
  
  priceRangeData <- extractAdjPriceFeatures(prices, stats, matches)
  
  
}

extractAdjPriceFeatures <- function(prices, stats, matches) {
  priceRangeData <- data.frame()
  for(actPlayerId in unique(prices$playerId)) {
    playerStats <- stats[playerId == actPlayerId]
    playerPrices <- prices[playerId == actPlayerId]
    actRange <- NULL
    for(priceIndex in 1:nrow(playerPrices)) {
      actPrice <- playerPrices[priceIndex, ]
      if(is.null(actRange)) {
        actRange <- data.frame('playerId' = actPrice$playerId, 'startDate' = actPrice$informationDate,
                               'startPrice' = actPrice$price)
      } else {
        actRange <- cbind(actRange, data.frame('endDate' = actPrice$informationDate, 'endPrice' = actPrice$price))
        # Enrich with form features
        relMatchdays <- getRelMatchdays(matches, actRange$startDate, actRange$endDate)
        relPlayerStats <- merge(x = playerStats, y = relMatchdays, by.x = c('matchday','season'), by.y = c('matchday','season'),
                   all.x = FALSE, all.y = FALSE)
        
        actRange <- cbind(actRange, data.frame('matchdayCount' = nrow(relMatchdays),
                                               'statsCount' = nrow(relPlayerStats),
                                               'completedMatches' = nrow(relPlayerStats[playerAssignment == 'DURCHGESPIELT']),
                                               'benchMatches' = nrow(relPlayerStats[playerAssignment == 'BENCH']),
                                               'notPlayedMatches' = nrow(relMatchdays) - nrow(relPlayerStats),
                                               'swappedInMatches' = nrow(relPlayerStats[playerAssignment == 'EINGEWECHSELT']),
                                               'swappedOutMatches' = nrow(relPlayerStats[playerAssignment == 'AUSGEWECHSELT']),
                                               'meanGrade' = mean(relPlayerStats$grade, na.rm = TRUE),
                                               'medianGrade' = median(relPlayerStats$grade, na.rm = TRUE)
        ))
        
        actRange <- mutate(actRange, 
                           age = round(as.numeric(difftime(actRange$endDate, actPrice$birthday, units = 'weeks'), units='weeks') / 52, digits = 0),
                           weekDiff = ceiling(as.numeric(difftime(actRange$endDate, actRange$startDate, units = 'weeks'), units='weeks')),
                           completedPerWeek = completedMatches / weekDiff,
                           benchPerWeek = benchMatches / weekDiff,
                           notPlayedPerWeek = notPlayedMatches / weekDiff,
                           swappedInPerWeek = swappedInMatches / weekDiff,
                           swappedOutPerWeek = swappedOutMatches / weekDiff,
                           statsPerWeek = statsCount / weekDiff,
                           primaryPosition = ifelse(sum(table(relPlayerStats$groupedPosition)) == 0, NA, 
                                                    names(which.max(table(relPlayerStats$groupedPosition)))[1]),
                           # Outcome
                           priceChangePerWeek = (actRange$endPrice - actRange$startPrice) / weekDiff
        )
        priceRangeData <- rbind(priceRangeData, actRange)
        # Starting point for new range
        actRange <- data.frame('playerId' = actPrice$playerId, 'startDate' = actPrice$informationDate,
                               'startPrice' = actPrice$price)
      }
    }
  }
  
  priceRangeData$meanGrade[is.nan(priceRangeData$meanGrade)] <- NA
  priceRangeData$primaryPosition <- as.factor(priceRangeData$primaryPosition)
  return(priceRangeData)
}

getRelMatchdays <- function(matches, startDate, endDate) {
  relMatches <- matches[matchtime > startDate & matchtime < endDate]
  relMatchdays <- unique(select(relMatches, season, matchday))
  return(relMatchdays)
}