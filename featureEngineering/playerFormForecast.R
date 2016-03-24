
fillAllPlayerForm <- function(stats) {
  require(dplyr)
  stats$formForecast <- NA
  leagues <- unique(stats$league)
  for(lea in leagues) {
    leagueStats <- filter(stats, league == lea)
    seasons <- unique(leagueStats$season)
    for(sea in seasons) {
      seasonStats <- filter(leagueStats, season == sea)
      for(statIndex in 1:nrow(seasonStats)) {
        stat <- seasonStats[statIndex, ]
        playerForm <- calculatePlayerForm(seasonStats, sea, stat$matchday, stat$playerId)
        ##### Set form in stats #####
        stats$formForecast[stats$season == sea & stats$playerId == stat$playerId 
                          & stats$matchday == stat$matchday] <- playerForm
      }
    }
  }
  return(stats)
}

calculatePlayerForm <- function(stats, season, matchday, playerId) {
  ##### relevant match selection #####
  pastStats <- pastMatchSelection(relStats = stats, relSeason = season, 
                                  relMatchday = matchday, relPlayerId = playerId)
  
  ##### grade imputation #####
  impStats <- gradeImputation(pastStats = pastStats)
  
  # Ordered relevant matchdays
  relDays <- impStats$matchday[order(impStats$matchday, decreasing = TRUE)]
  
  ##### choose forecast method #####
  forecastMethod <- setForcastMethod()
  
  # Prepare forecast data
  forecastData <- prepareForecastData(impStats = impStats, relDays = relDays)
  
  ##### calculate form #####
  playerForm <- applyForecastMethod(forecastData = forecastData, forecastMethod = forecastMethod)
  
  return(playerForm)
}

pastMatchSelection <- function(relStats, relSeason, relMatchday, relPlayerId) {
  # 1.1
  pastStats <- filter(relStats, season == relSeason, matchday < relMatchday, playerId == relPlayerId)
  
  return(pastStats)
}

gradeImputation <- function(pastStats) {
  # 2.1
  impStats <- pastStats
  
  return(impStats)
}

setForcastMethod <- function() {
  # 3.1
  forecastMethod <- mean
  
  return(forecastMethod)
}

prepareForecastData <- function(impStats, relDays) {
  relFormStats <- data.frame()
  ### Extract data for form calculation ###
  for(day in relDays) {
    aktStat <- filter(impStats, matchday == day)
    # playerMatchStat found
    if(nrow(aktStat) > 0) {
      aktFormStat <- data.frame('matchday' = aktStat$matchday, 
                                'playerAssignment' = aktStat$playerAssignment,
                                'home' = aktStat$home,
                                'grade' = aktStat$grade)
      relFormStats <- rbind(relFormStats, aktFormStat)
    }
  }
  return(relFormStats)
}

applyForecastMethod <- function(forecastData, forecastMethod) {
  if(nrow(forecastData) == 0) {
    return(NA)
  }
  # SabrinaSimple #
  forecastedGrade <- forecastMethod(forecastData$grade, na.rm = TRUE)
  
  return(forecastedGrade)
}

