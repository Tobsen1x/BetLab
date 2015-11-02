extractFeatures <- function(playerStats, matches, minFormQualityMatches = 3) {
    require(forecast)
    require(dplyr)
    
    # Enrich with formQuality
    playerStats <- mutate(playerStats, formQuality = ifelse(formMaxMatches >= minFormQualityMatches, 
                                             formPlayedMatches / formMaxMatches,
                                             formPlayedMatches / minFormQualityMatches))
    
    matches$homePrice <- NA
    matches$visitorsPrice <- NA
    matches$homeMeanfForm <- NA
    matches$visitorsMeanfForm <- NA
    matches$homeExpSmoothForm <- NA
    matches$visitorsExpSmoothForm <- NA
    

    for(i in seq(1:nrow(matches))) {
        actMatch <- matches[i, ]
        
        homePrice <- calcTeamPrice(playerStats, TRUE, 
                                   actMatch$matchId)
        visitorsPrice <- calcTeamPrice(playerStats, FALSE, 
                                       actMatch$matchId)
        
        homeMeanfForm <- calcTeamForm('meanf', playerStats, TRUE, actMatch$matchId)
        visitorsMeanfForm <- calcTeamForm('meanf', playerStats, FALSE, actMatch$matchId)
        
        homeExpSmoothForm <- calcTeamForm('expSmooth', playerStats, TRUE, actMatch$matchId)
        visitorsExpSmoothForm <- calcTeamForm('expSmooth', playerStats, FALSE, actMatch$matchId)
        
        matches[matches$matchId == actMatch$matchId, 'homePrice'] <- homePrice
        matches[matches$matchId == actMatch$matchId, 'visitorsPrice'] <- visitorsPrice
        matches[matches$matchId == actMatch$matchId, 'homeMeanfForm'] <- homeMeanfForm
        matches[matches$matchId == actMatch$matchId, 'visitorsMeanfForm'] <- visitorsMeanfForm
        matches[matches$matchId == actMatch$matchId, 'homeExpSmoothForm'] <- homeExpSmoothForm
        matches[matches$matchId == actMatch$matchId, 'visitorsExpSmoothForm'] <- visitorsExpSmoothForm
    }
    
    matches[is.nan(matches$homeMeanfForm), 'homeMeanfForm'] <- NA
    matches[is.nan(matches$visitorsMeanfForm), 'visitorsMeanfForm'] <- NA
    matches[is.nan(matches$homeExpSmoothForm), 'homeExpSmoothForm'] <- NA
    matches[is.nan(matches$visitorsExpSmoothForm), 'visitorsExpSmoothForm'] <- NA
    
    matches <- matches %>% mutate(expGoalDiff = homeExpGoals - visitorsExpGoals,
                                  expGoalByChancesDiff = homeExpChances * homeChancesGoalsRate - 
                                    visitorsExpChances * visitorsChancesGoalsRate,
                                  formMeanfDiff = homeMeanfForm - visitorsMeanfForm,
                                  formExpSmoothDiff = homeExpSmoothForm - visitorsExpSmoothForm,
                                  priceDiff = homePrice - visitorsPrice,
                                  logPriceRate = log(homePrice / visitorsPrice))
    return(matches)
}

calcTeamPrice <- function(playerStats, h, mId) {
    featureStats <- filter(playerStats, matchId == mId, home == h,
                           playerAssignment != 'BENCH')
    return(mean(featureStats$fitPrice, na.rm = TRUE))
}

weightAverage <- function(values, weights) {
    result <- sum(values * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
    return(result)
}

calcTeamForm <- function(formAlgo, playerStats, h, mId) {
    featureStats <- filter(playerStats, matchId == mId, home == h,
                           playerAssignment != 'BENCH')
    
    result <- NA
    if(formAlgo == 'meanf') {
        result <- weightAverage(featureStats$meanf, featureStats$formQuality)
    } else if(formAlgo == 'expSmooth') {
        result <- weightAverage(featureStats$expSmoothing, featureStats$formQuality)
    } else {
        throw(paste('Wrong algorithm:', formAlgo))
    }
    return(result)
}