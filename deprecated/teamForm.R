
enrichTeamForm <- function(stats, matches, minMatchdays, maxMatchdays, homeAdvantage) {
    require(dplyr)
    require(magrittr)
    
    enrichedMatches <- enrichTeamPricePlace(stats, matches)
    
    formDevEnrichedMatches <- enrichFormDev(enrichedMatches, homeAdvantage)
    
    teamFormEnrichedMatches <- enrichTeamForm(formDevEnrichedMatches,
                                              minMatchdays, maxMatchdays)
    return(teamFormEnrichedMatches)
}

enrichTeamForm <- function(formDevEnrichedMatches,
                           minMatchdays, maxMatchdays) {
    # calculate Team Form with beta distribution
    formDevEnrichedMatches$homeTeamForm <- NA
    formDevEnrichedMatches$visitorsTeamForm <- NA
    for(i in seq(1:nrow(formDevEnrichedMatches))) {
        row <- formDevEnrichedMatches[i, ]
        homeTeamForm <- calcTeamForm(formDevEnrichedMatches, row$homeTeamId, 
                                     row$season, row$matchday, minMatchdays, 
                                     maxMatchdays)
        visitorsTeamForm <- calcTeamForm(formDevEnrichedMatches, row$visitorsTeamId, 
                                         row$season, row$matchday, minMatchdays, 
                                         maxMatchdays)
        formDevEnrichedMatches[formDevEnrichedMatches$matchId == row$matchId, 
                               'homeTeamForm'] <- homeTeamForm
        formDevEnrichedMatches[formDevEnrichedMatches$matchId == row$matchId, 
                               'visitorsTeamForm'] <- visitorsTeamForm
    }
    
    return(formDevEnrichedMatches)
}

calcTeamForm <- function(formDevEnrichedMatches, teamId, sea, mday, 
                         minMatchdays, maxMatchdays) {

    relTeamMatches <- formDevEnrichedMatches %>%
        filter(season == sea, matchday < mday, 
               homeTeamId == teamId | visitorsTeamId == teamId) %>%
        arrange(desc(matchday))
    
    matchCount <- maxMatchdays
    if(mday <= maxMatchdays) {
        matchCount <- mday - 1
    }
    
    if(mday <= minMatchdays | matchCount < 1) {
        return(NA)
    }
    
    formVector <- c()
    weightVector <- c()
    for(past in seq(1:matchCount)) {
        aktMatch <- relTeamMatches[past, ]
        weight <- 1.5 - punif(q = past, 1, matchCount)
        
        form <- NA
        if(aktMatch$homeTeamId == teamId) {
            form <- aktMatch$homeFormDev
        } else {
            form <- aktMatch$visitorsFormDev
        }
        
        formVector <- c(formVector, form)
        weightVector <- c(weightVector, weight)
    }
    
    enumerator <- sum(formVector * weightVector)
    denominator <- sum(weightVector)
    return(enumerator / denominator)
}

enrichFormDev <- function(enrichedMatches, homeAdvantage) {
    enrichedMatches$homeFormDev <- NA
    enrichedMatches$visitorsFormDev <- NA
    allSeasons <- distinct(select(enrichedMatches, season))[,1]
    for(sea in allSeasons) {
        seasonMatches <- enrichedMatches %>% filter(season == sea)
        teamCount <- nrow(distinct(select(seasonMatches, homeTeamId)))
        for(i in seq(1:nrow(seasonMatches))) {
            row <- seasonMatches[i, ]
            homeFormDev <- calcTeamFormDev(row$homeTeamPricePlace, row$visitorsTeamPricePlace,
                                           row$matchResult, teamCount, TRUE)
            visitorsFormDev <- calcTeamFormDev(row$homeTeamPricePlace, row$visitorsTeamPricePlace,
                                               row$matchResult, teamCount, FALSE)
            
            enrichedMatches[enrichedMatches$matchId == row$matchId, 'homeFormDev'] <- 
                homeFormDev - homeAdvantage
            enrichedMatches[enrichedMatches$matchId == row$matchId, 'visitorsFormDev'] <- 
                visitorsFormDev + homeAdvantage
        }
    }
    return(enrichedMatches)
}

calcTeamFormDev <- function(homePlace, visitorsPlace, matchResult, maxTeams, home) {
    relPlace <- NA
    compPlace <- NA
    relWin <- NA
    if(home) {
        relPlace <- homePlace
        compPlace <- visitorsPlace
        if(matchResult == 'HomeVictory') {
            relWin = TRUE
        } else if(matchResult == 'VisitorsVictory') {
            relWin = FALSE
        }
    } else {
        relPlace <- visitorsPlace
        compPlace <- homePlace
        if(matchResult == 'HomeVictory') {
            relWin = FALSE
        } else if(matchResult == 'VisitorsVictory') {
            relWin = TRUE
        }
    }
    
    #Draw
    if(is.na(relWin)) {
        return(relPlace - compPlace)
    } else if(relWin) {
        return(relPlace + (maxTeams - compPlace))
    } else {
        return((compPlace + (maxTeams - relPlace)) * -1) 
    }
}

enrichTeamPricePlace <- function(stats, matches) {
    allSeason <- distinct(select(stats, season)) %>%
        arrange(season)
    
    matches$homeTeamPricePlace <- NA
    matches$visitorsTeamPricePlace <- NA
    
    # Iterate over all seasons
    for(sea in allSeason[, 1]) {
        seasonStats <- stats %>% filter(season == sea)
        allMatchDays <- distinct(select(seasonStats, matchday)) %>%
            arrange(matchday)
        # Iterate over all matchdays
        for(day in allMatchDays[, 1]) {
            matchdayStats <- seasonStats %>% filter(matchday == day)
            # Calculate the price order of the teams
            teamOrder <- getTeamPriceTable(matchdayStats, day, sea)
            
            relMatches <- matches %>% filter(season == sea, matchday == day)
            # Iterate over all matches of the matchday to set the price places
            for(i in seq(1:nrow(relMatches))) {
                row <- relMatches[i, ]
                matches[matches$season == sea & matches$matchday == day &
                            matches$matchId == row$matchId, 'homeTeamPricePlace'] <-
                    match(row$homeTeamId, teamOrder$teamId)
                matches[matches$season == sea & matches$matchday == day &
                            matches$matchId == row$matchId, 'visitorsTeamPricePlace'] <-
                    match(row$visitorsTeamId, teamOrder$teamId)
            }
        }
    }
    
    return(matches)
}

getTeamPriceTable <- function(stats, mday, sea) {
    require(dplyr)
    require(magrittr)
    relStats <- stats %>% 
        filter(matchday == mday, season == sea, playerAssignment != 'BENCH') %>%  
        mutate(teamId = ifelse(home, homeTeamId, visitorsTeamId)) %>%
        group_by(teamId) %>% 
        select(teamId, fitPrice) %>%
        summarise(avgPrice = mean(fitPrice)) %>%
        arrange(desc(avgPrice))
    relStats
}