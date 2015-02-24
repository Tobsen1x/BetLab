simpleMatchFeatureExtract <- function(playerStats, matches) {
    matches$homePrice <- NA
    matches$visitorsPrice <- NA
    matches$homeForm <- NA
    matches$visitorsForm <- NA
    
    for(i in seq(1:nrow(matches))) {
        actMatch <- matches[i, ]
        
        homePrice <- calcTeamPrice(playerStats, TRUE, 
                                   actMatch$matchId)
        visitorsPrice <- calcTeamPrice(playerStats, FALSE, 
                                       actMatch$matchId)
        
        homeForm <- calcTeamForm(playerStats, TRUE, actMatch$matchId)
        visitorsForm <- calcTeamForm(playerStats, FALSE, actMatch$matchId)
        
        matches[matches$matchId == actMatch$matchId, 'homePrice'] <- homePrice
        matches[matches$matchId == actMatch$matchId, 'visitorsPrice'] <- visitorsPrice
        matches[matches$matchId == actMatch$matchId, 'homeForm'] <- homeForm
        matches[matches$matchId == actMatch$matchId, 'visitorsForm'] <- visitorsForm
    }
    
    matches
}

calcTeamPrice <- function(playerStats, h, mId) {
    featureStats <- filter(playerStats, matchId == mId, home == h,
                           playerAssignment != 'BENCH')
    return(mean(featureStats$fitPrice))
}

calcTeamForm <- function(playerStats, h, mId) {
    featureStats <- filter(playerStats, matchId == mId, home == h,
                           playerAssignment != 'BENCH')
    return(mean(featureStats$playerForm, na.rm = TRUE))
}