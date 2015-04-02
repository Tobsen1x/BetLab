extractFeatures <- function(playerStats, matches) {
    require(forecast)
    require(dplyr)
    
    matches$homePrice <- NA
    matches$visitorsPrice <- NA
    matches$homeMeanfForm <- NA
    matches$visitorsMeanfForm <- NA
    matches$homeSesOptimalForm <- NA
    matches$visitorsSesOptimalForm <- NA
    matches$homeSesSimpleForm <- NA
    matches$visitorsSesSimpleForm <- NA

    for(i in seq(1:nrow(matches))) {
        actMatch <- matches[i, ]
        
        homePrice <- calcTeamPrice(playerStats, TRUE, 
                                   actMatch$matchId)
        visitorsPrice <- calcTeamPrice(playerStats, FALSE, 
                                       actMatch$matchId)
        
        homeMeanfForm <- calcTeamForm('meanf', playerStats, TRUE, actMatch$matchId)
        visitorsMeanfForm <- calcTeamForm('meanf', playerStats, FALSE, actMatch$matchId)
        
        homeSesOptimalForm <- calcTeamForm('sesOptimal', playerStats, TRUE, actMatch$matchId)
        visitorsSesOptimalForm <- calcTeamForm('sesOptimal', playerStats, FALSE, actMatch$matchId)
        
        homeSesSimpleForm <- calcTeamForm('sesSimple', playerStats, TRUE, actMatch$matchId)
        visitorsSesSimpleForm <- calcTeamForm('sesSimple', playerStats, FALSE, actMatch$matchId)
        
        matches[matches$matchId == actMatch$matchId, 'homePrice'] <- homePrice
        matches[matches$matchId == actMatch$matchId, 'visitorsPrice'] <- visitorsPrice
        matches[matches$matchId == actMatch$matchId, 'homeMeanfForm'] <- homeMeanfForm
        matches[matches$matchId == actMatch$matchId, 'visitorsMeanfForm'] <- visitorsMeanfForm
        matches[matches$matchId == actMatch$matchId, 'homeSesOptimalForm'] <- homeSesOptimalForm
        matches[matches$matchId == actMatch$matchId, 'visitorsSesOptimalForm'] <- visitorsSesOptimalForm
        matches[matches$matchId == actMatch$matchId, 'homeSesSimpleForm'] <- homeSesSimpleForm
        matches[matches$matchId == actMatch$matchId, 'visitorsSesSimpleForm'] <- visitorsSesSimpleForm
    }
    
    matches[is.nan(matches$homeMeanfForm), 'homeMeanfForm'] <- NA
    matches[is.nan(matches$visitorsMeanfForm), 'visitorsMeanfForm'] <- NA
    matches[is.nan(matches$homeSesOptimalForm), 'homeSesOptimalForm'] <- NA
    matches[is.nan(matches$visitorsSesOptimalForm), 'visitorsSesOptimalForm'] <- NA
    matches[is.nan(matches$homeSesSimpleForm), 'homeSesSimpleForm'] <- NA
    matches[is.nan(matches$visitorsSesSimpleForm), 'visitorsSesSimpleForm'] <- NA
    
    matches <- matches %>% mutate(expGoalDiff = homeExpGoals - visitorsExpGoals,
                                  expChancesDiff = homeExpChances - visitorsExpChances,
                                  priceDiff = homePrice - visitorsPrice,
                                  logPriceRate = log(homePrice / visitorsPrice),
                                  formMeanfDiff = homeMeanfForm - visitorsMeanfForm,
                                  formSesOptimalDiff = homeSesOptimalForm - visitorsSesOptimalForm,
                                  formSesSimpleDiff = homeSesSimpleForm - visitorsSesSimpleForm)
    return(matches)
}

calcTeamPrice <- function(playerStats, h, mId) {
    featureStats <- filter(playerStats, matchId == mId, home == h,
                           playerAssignment != 'BENCH')
    return(mean(featureStats$fitPrice, na.rm = TRUE))
}

calcTeamForm <- function(formAlgo, playerStats, h, mId) {
    featureStats <- filter(playerStats, matchId == mId, home == h,
                           playerAssignment != 'BENCH')
    result <- NA
    if(formAlgo == 'meanf') {
        result <- mean(featureStats$meanf, na.rm = TRUE)
    } else if(formAlgo == 'sesOptimal') {
        result <- mean(featureStats$sesOptimal, na.rm = TRUE)
    } else if(formAlgo == 'sesSimple') {
        result <- mean(featureStats$sesSimple, na.rm = TRUE)
    } else {
        throw(paste('Wrong algorithm:', formAlgo))
    }
    return(result)
}