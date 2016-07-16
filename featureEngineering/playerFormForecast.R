fillAllPlayerForm <- function(stats, matches, args) {
  stats$formForecast <- NA
  stats <- data.table(stats)
  setkey(stats, matchId, playerId, season)
  version <- as.vector(strsplit(args$version, '')[[1]], mode = 'numeric')
  seasonStatsList <- initializeSeasonStatsList(stats)
  leagues <- unique(stats$league)
  for(lea in leagues) {
    leagueStats <- stats[league == lea, ]
    aktProgress <- 0.0
    for(statIndex in 1:nrow(leagueStats)) {
      stat <- leagueStats[statIndex, ]
      relStats <- getRelevantStats(seasonStatsList, stat$season)
      playerForm <- calculatePlayerForm(stats = relStats, matches = matches, statToCalcFor = stat, 
                                        versions = version, args = args)
      ##### Set form in stats #####
      stats$formForecast[stats$matchId == stat$matchId & stats$playerId == stat$playerId] <- playerForm
      
      #Logging 1 % steps
      if(statIndex / nrow(leagueStats) > (aktProgress + 0.01)) {
        aktProgress <- aktProgress + 0.01
        loginfo(paste('Filling form forecast:', round(aktProgress * 100, 0), '% done.', sep = ' '))
      }
    }
    
  }
  return(stats)
}

calculatePlayerForm <- function(stats, matches, statToCalcFor, 
                                versions, args) {
  if(length(versions) != 3) {
    stop('wrong version string.')
  }
  ##### relevant match selection #####
  pastStats <- pastMatchSelection(relStats = stats, relSeason = statToCalcFor$season, 
                                  relMatchday = statToCalcFor$matchday, 
                                  relPlayerId = statToCalcFor$playerId, 
                                  lastPriceDate = statToCalcFor$fitPriceDate,
                                  weeksBeforeLastPriceDate = args$weeksBeforeLastPriceDate,
                                  version = versions[1])
  
  ##### grade imputation #####
  impStats <- gradeImputation(pastStats = pastStats, matches = matches, statToCalcFor = statToCalcFor,
                              versions[2], args = args)
  
  ##### choose forecast method #####
  forecastApply <- setForcastApply(statToCalcFor = statToCalcFor, forecastData = impStats, version = versions[3],
                                   args = args)
  
  ##### calculate form #####
  playerForm <- applyForecastMethod(forecastData = impStats, forecastMethod = forecastApply$method, 
                                    forecastMethodArgs = forecastApply$args)
  
  #### Last Imputation #####
  if(is.na(playerForm)) {
    playerForm <- args$lastFormImpute
  }
  
  return(playerForm)
}

pastMatchSelection <- function(relStats, relSeason, relMatchday, relPlayerId, version,
                               lastPriceDate = NULL, weeksBeforeLastPriceDate = NULL) {
  #### 1.1 Same season ####
  if(version == 1) {
    pastStats <- filter(relStats, season == relSeason, matchday < relMatchday, playerId == relPlayerId)
  } 
  #### 1.2 x weeks before last marketprice ####
  else if(version == 2) {
    relTime <- getWeeksBefore(lastPriceDate, weeksBeforeLastPriceDate)
    pastStats <- relStats[matchtime > relTime & playerId == relPlayerId & matchday < relMatchday, ]
  }
  
  return(pastStats)
}

getWeeksBefore <- function(lastPriceDate, weeksBeforeLastPriceDate) {
  lastPDay <- as.POSIXct(lastPriceDate)
  lowerBound <- lastPDay - 60 * 60* 24 * 7 * weeksBeforeLastPriceDate
  return(lowerBound)
}

gradeImputation <- function(pastStats, matches, statToCalcFor, version, args) {
  #### 2.1 No Imputation ####
  if(version == 1) {
    impStats <- data.frame('matchTime' = pastStats$matchtime,
                           'purgedGrade' = pastStats$purgedGrade)
    impStats <- filter(impStats, !is.na(purgedGrade))
  } 
  #### 2.2 static Imputation ####
  else if(version == 2) {
    staticBenchImpute <- args$staticBenchImpute
    staticNotplayedImpute <- args$staticNotPlayedImpute
    lowerBound <- getWeeksBefore(statToCalcFor$fitPriceDate, args$weeksBeforeLastPriceDate)
    upperBound <- statToCalcFor$matchtime
    frameMatches <- filter(matches, matchtime > lowerBound, matchtime < upperBound, 
                           !(season == statToCalcFor$season & matchday >= statToCalcFor$matchday))
    matchdays <- summarise(group_by(frameMatches, season, matchday))
    matchdays <- matchdays[order(matchdays$season, matchdays$matchday),]
    impStats <- data.frame()
    if(nrow(matchdays) > 0) {
      for(index in 1:nrow(matchdays)) {
        aktDay <- matchdays[index,]
        aktStat <- filter(pastStats, matchday == aktDay$matchday, season == aktDay$season)
        # playerMatchStat found
        if(nrow(aktStat) == 1) {
          if(aktStat$playerAssignment == 'BENCH' | is.na(aktStat$purgedGrade)) {
            purgedGrade <- staticBenchImpute
          } else {
            purgedGrade <- aktStat$purgedGrade
          }
          aktFormStat <- data.frame('matchTime' = as.POSIXct(aktStat$matchtime), 
                                    'purgedGrade' = as.numeric(purgedGrade))
        } 
        # Player not played
        else if(nrow(aktStat) == 0) {
          purgedGrade <- staticNotplayedImpute
          matchdayTime <- frameMatches[frameMatches$matchday == aktDay$matchday &
                         frameMatches$season == aktDay$season, ][1, 'matchtime']
          aktFormStat <- data.frame('matchTime' = as.POSIXct(matchdayTime), 
                                    'purgedGrade' = as.numeric(purgedGrade))
        } else {
          logwarn(paste('Multiple Stats for player', aktStat[1,]$playerId, 'and match', 
                        aktStat[1,]$matchId, sep = ' '))
          if(is.na(mean(aktStat$purgedGrade, na.rm = TRUE))) {
            purgedGrade <- staticBenchImpute
          } else {
            purgedGrade <- mean(aktStat$purgedGrade, na.rm = TRUE)
          }
          aktFormStat <- data.frame('matchTime' = as.POSIXct(aktStat[1, ]$matchtime), 
                                    'purgedGrade' = as.numeric(purgedGrade))
        }
        
        impStats <- rbind(impStats, aktFormStat)
      }
    }
  }
  
  return(impStats)
}

setForcastApply <- function(statToCalcFor, forecastData, version, args) {
  # 3.1
  if(version == 1) {
    forecastMethod <- mean
    forecastMethodArgs <- list('x' = forecastData$purgedGrade, 'na.rm' = TRUE)
  }
  # 3.2
  else if(version == 2) {
    forecastMethod <- linearFormDecrease
    forecastMethodArgs <- list('x' = forecastData, 'actMatchtime' = statToCalcFor$matchtime, 
                               'maxPastDays' = args$pastDays)
  }
  
  methodApply <- list('method' = forecastMethod, 'args' = forecastMethodArgs)
  return(methodApply)
}

# Calculates the linear weighted form 
linearFormDecrease <- function(x, actMatchtime, maxPastDays) {
  daysDiff <- round(difftime(actMatchtime, x$matchTime, units="days"), 0)
  x <- mutate(x, pastdays = daysDiff, weight = calculateLinearWeight(x = pastdays, maxPastDays = maxPastDays))
  result <- sum(x$weight * x$purgedGrade) / sum(x$weight)
  return(result)
}

# Calculate the linear weights
calculateLinearWeight <- function(x, maxPastDays) {
  x1 <- maxPastDays
  y1 <- 0.0
  x2 <- 1
  y2 <- 1.0
  x < maxPastDays
  # Keine Gewichtung für Werte außerhalb des linearen Bereichs
  y <- ifelse(x < x2 | x > x1, 0, (y2 - y1) / (x2 - x1) * (x - x1) + y1) 
  return(y)
}

applyForecastMethod <- function(forecastData, forecastMethod, forecastMethodArgs) {
  if(nrow(forecastData) == 0) {
    return(NA)
  }
  forecastedGrade <- do.call(forecastMethod, forecastMethodArgs)
  return(forecastedGrade)
}

initializeSeasonStatsList <- function(stats) {
  seasons <- unique(stats$season)
  seasons <- seasons[order(seasons)]
  seasonStatList <- list()
  for(sea in seasons) {
    seasonStats <- stats[season == sea,]
    aktList <- list(seasonStats)
    names(aktList) <- sea
    seasonStatList <- append(seasonStatList, aktList)
  }
  return(seasonStatList)
}

getSeasonShift <- function(season, shift) {
  yearFrom <- strsplit(season, '-')[[1]][1]
  yearTo <- strsplit(season, '-')[[1]][2]
  newFrom <- as.integer(yearFrom) + shift
  newTo <- as.integer(yearTo) + shift
  newSeason <- paste(newFrom, newTo, sep = '-')
  return(newSeason)
}

getRelevantStats <- function(statList, season) {
  beforeSeason <- getSeasonShift(season, shift = - 1)
  a <- statList[[beforeSeason]]
  b <- statList[[season]]
  stats <- rbind(a, b)
  return(stats)
}

