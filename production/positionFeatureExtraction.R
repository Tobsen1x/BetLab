# Adds ratios and differences of specific, static features
addDerivedFeatures <- function(featuredMatches, infiniteReplacement = log(10)) {
    results <- mutate(featuredMatches, 
                      tw_Price_avg_Diff = tw_Price_Home_avg - tw_Price_Visitors_avg,
                      tw_Price_avg_Ratio = ifelse(is.infinite(tw_Price_Home_avg / tw_Price_Visitors_avg), 
                                                  infiniteReplacement, log(tw_Price_Home_avg / tw_Price_Visitors_avg)),
                      def_Price_avg_Diff = def_Price_Home_avg - def_Price_Visitors_avg,
                      def_Price_avg_Ratio = ifelse(is.infinite(def_Price_Home_avg / def_Price_Visitors_avg), 
                                                   infiniteReplacement, log(def_Price_Home_avg / def_Price_Visitors_avg)),
                      def_Price_min_Diff = def_Price_Home_min - def_Price_Visitors_min,
                      def_Price_min_Ratio = ifelse(is.infinite(def_Price_Home_min / def_Price_Visitors_min), 
                                                   infiniteReplacement, log(def_Price_Home_min / def_Price_Visitors_min)),
                      def_Price_max_Diff = def_Price_Home_max - def_Price_Visitors_max,
                      def_Price_max_Ratio = ifelse(is.infinite(def_Price_Home_max / def_Price_Visitors_max), 
                                                   infiniteReplacement, log(def_Price_Home_max / def_Price_Visitors_max)),
                      def_Price_sum_Diff = def_Price_Home_sum - def_Price_Visitors_sum,
                      def_Price_sum_Ratio = ifelse(is.infinite(def_Price_Home_sum / def_Price_Visitors_sum), 
                                                   infiniteReplacement, log(def_Price_Home_sum / def_Price_Visitors_sum)),
                      mid_Price_avg_Diff = mid_Price_Home_avg - mid_Price_Visitors_avg,
                      mid_Price_avg_Ratio = ifelse(is.infinite(mid_Price_Home_avg / mid_Price_Visitors_avg), 
                                                   infiniteReplacement, log(mid_Price_Home_avg / mid_Price_Visitors_avg)),
                      mid_Price_min_Diff = mid_Price_Home_min - mid_Price_Visitors_min,
                      mid_Price_min_Ratio = ifelse(is.infinite(mid_Price_Home_min / mid_Price_Visitors_min), 
                                                   infiniteReplacement, log(mid_Price_Home_min / mid_Price_Visitors_min)),
                      mid_Price_max_Diff = mid_Price_Home_max - mid_Price_Visitors_max,
                      mid_Price_max_Ratio = ifelse(is.infinite(mid_Price_Home_max / mid_Price_Visitors_max), 
                                                   infiniteReplacement, log(mid_Price_Home_max / mid_Price_Visitors_max)),
                      mid_Price_sum_Diff = mid_Price_Home_sum - mid_Price_Visitors_sum,
                      mid_Price_sum_Ratio = ifelse(is.infinite(mid_Price_Home_sum / mid_Price_Visitors_sum), 
                                                   infiniteReplacement, log(mid_Price_Home_sum / mid_Price_Visitors_sum)),
                      off_Price_avg_Diff = off_Price_Home_avg - off_Price_Visitors_avg,
                      off_Price_avg_Ratio = ifelse(is.infinite(off_Price_Home_avg / off_Price_Visitors_avg), 
                                                   infiniteReplacement, log(off_Price_Home_avg / off_Price_Visitors_avg)),
                      off_Price_min_Diff = off_Price_Home_min - off_Price_Visitors_min,
                      off_Price_min_Ratio = ifelse(is.infinite(off_Price_Home_min / off_Price_Visitors_min), 
                                                   infiniteReplacement, log(off_Price_Home_min / off_Price_Visitors_min)),
                      off_Price_max_Diff = off_Price_Home_max - off_Price_Visitors_max,
                      off_Price_max_Ratio = ifelse(is.infinite(off_Price_Home_max / off_Price_Visitors_max), 
                                                   infiniteReplacement, log(off_Price_Home_max / off_Price_Visitors_max)),
                      off_Price_sum_Diff = off_Price_Home_sum - off_Price_Visitors_sum,
                      off_Price_sum_Ratio = ifelse(is.infinite(off_Price_Home_sum / off_Price_Visitors_sum), 
                                                   infiniteReplacement, log(off_Price_Home_sum / off_Price_Visitors_sum)))
    
    return(results)
}

# TEST
#playerStats <- stats
#priceAssignedPositions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 
#                            'mid', 'off', 'off', 'off', 'off', 'off')
#relNormalAssignments <- c('DURCHGESPIELT', 'AUSGEWECHSELT')
#functs = c('min', 'max', 'avg', 'sum')
#benchFuncts = c('max', 'avg')
extractMatchResultFeatures <- function(playerStats, matches, 
                                       priceAssignedPositions, functs,
                                       relNormalAssignments = NA, 
                                       benchFuncts = NA) {
    ### Assemble relevant positions
    # Grouping positions
    colNames <- levels(playerStats$position)
    groupedPositions <- as.data.frame(t(priceAssignedPositions))
    colnames(groupedPositions) <- colNames
    playerStats$groupedStaticPosition <- sapply(playerStats$staticPosition, FUN = function(x) groupedPositions[1, x])
    
    # Name position - price
    featureNames <- sapply(unique(priceAssignedPositions), getFeatureName, 
                           group = 'Price', home = TRUE, functs = functs)
    featureNames <- c(featureNames, sapply(unique(priceAssignedPositions), getFeatureName, 
                                           group = 'Price', home = FALSE, functs = functs))
    if(!is.na(benchFuncts)) {
        # For bench features just take positions def, mid, off
        benchPositions <- unique(priceAssignedPositions)
        benchPositions <- benchPositions[!benchPositions %in% 'tw']
        featureNames <- c(featureNames, sapply(benchPositions, getFeatureName, 
                                               group = 'Bench', home = TRUE, functs = benchFuncts))
        featureNames <- c(featureNames, sapply(benchPositions, getFeatureName, 
                                               group = 'Bench', home = FALSE, functs = benchFuncts))
    }
    
    # set col names
    for(name in featureNames) {
        matches[, name] <- NA
    }
    
    for(i in 1:nrow(matches)) {
        actMatch <- matches[i, ]
        matchStats <- filter(playerStats, matchId == actMatch$matchId)
        for(featureName in featureNames) {
            featureValue <- calcFeatureValue(featureName, matchStats, groupedPositions,
                                             relNormalAssignments)
            matches[matches$matchId == actMatch$matchId, featureName] <- featureValue
        }
    }   
    
    # extract Formations --> Exclude for now
    #majorityVoteFormation <- which.max(table(matches$heimFormation))
    #matches$heimFormation[is.na(matches$heimFormation)] <- majorityVoteFormation
    #matches$auswFormation[is.na(matches$auswFormation)] <- majorityVoteFormation
    #groupedFormationTable <- createGroupedFormationTable(factor(matches$heimFormation))
    #merged <- merge(matches, groupedFormationTable, by.x = 'heimFormation', by.y = 'formation', sort = FALSE)
    #merged <- rename(merged, heimGroupedFormation = group1)
    #merged <- merge(merged, groupedFormationTable, by.x = 'auswFormation', by.y = 'formation', sort = FALSE)
    #merged <- rename(merged, auswGroupedFormation = group1)    
    
    return(matches)
}

# TEST
#formations <- as.factor(matches$heimFormation)
createGroupedFormationTable <- function(formations) {
    #filter(formationTable, is.na(group1))$formation
    #table(formations)
    library(gdata)
    formationTable <- data.frame('formation' = levels(formations), 'group1' = NA)
    formationTable$group1[grepl('4-4-2', formationTable$formation)] <- '4-4-2'
    formationTable$group1[startsWith(formationTable$formation, '3')] <- '3 Def'
    formationTable$group1[is.na(formationTable$group1)] <- 'Strong Def'
    return(formationTable)
}

# TEST
#feature <- 'off_Price_Home_min'
#feature <- 'def_Bench_Visitors_avg'
#relAssignments <- relNormalAssignments
#playerStats <- filter(playerStats, matchId == 1)
calcFeatureValue <- function(feature, playerStats, groupedPositions,
                             relAssignments) {
    relPos <- strsplit(feature, '_')[[1]][1]
    relGroup <- strsplit(feature, '_')[[1]][2]
    isHome <- strsplit(feature, '_')[[1]][3] == 'Home'

    # For Price features, the concrete position is necessary
    if(relGroup == 'Price') {
        # Extracting relevant Positions
        relPps <- t(groupedPositions[1, ])[, 1] == relPos 
        relPositions <- names(relPps[relPps == TRUE])
    } else if(relGroup == 'Bench') {
        # For bench features, just the grouped position is necessary
        relPositions <- relPos
        relAssignments <- c('BENCH', 'EINGEWECHSELT')
    } else {
        stop('Error')
    }
    
    # Extracting function
    func <- strsplit(feature, '_')[[1]][4]
    
    featureStats <- getStatsByPositions(playerStats, isHome, relGroup, 
                                        relPositions, relAssignments)
    featureResult <- extractFeature(featureStats, func = func)
    
    if(is.na(featureResult)) {
        featureResult <- 0
    }
    
    return(featureResult)
}

getStatsByPositions <- function(playerStats, isHome, relGroup, 
                                relPositions, relAssignments) {
    if(relGroup == 'Price') {
        # Subsetting to relevant player stats
        featureStats <- playerStats[playerStats$home == isHome & 
                                    playerStats$position %in% relPositions &
                                    playerStats$playerAssignment %in% relAssignments, ]
    } else if(relGroup == 'Bench') {
        featureStats <- playerStats[playerStats$home == isHome & 
                                        playerStats$groupedStaticPosition %in% relPositions &
                                        playerStats$playerAssignment %in% relAssignments, ]
    }
    return(featureStats)
}

getFeatureName <- function(pos, group, home, functs) {
    if(home) {
        return(paste(paste(pos, group, 'Home', sep = '_'), functs, sep = '_'))
    } else {
        return(paste(paste(pos, group, 'Visitors', sep = '_'), functs, sep = '_'))
    }
}

extractFeature <- function(feaStats, func) {
    if(func == 'sum') {
        return(sum(feaStats$fitPrice, na.rm = TRUE))
    } else if(func == 'avg') {
        return(mean(feaStats$fitPrice, na.rm = TRUE))
    } else if(func == 'min') {
        if(all(is.na(feaStats$fitPrice))) {
            return(0)
        } else {
            return(min(feaStats$fitPrice, na.rm = TRUE))
        }
    } else if(func == 'max') {
        if(all(is.na(feaStats$fitPrice))) {
            return(0)    
        } else {
            return(max(feaStats$fitPrice, na.rm = TRUE))    
        }
        
    } else {
        stop(paste('Wrong function:', func))
    }
}

# TEST
#featuredMatches <- normalMatches
filterFeaturedMatches <- function(featuredMatches) {
    relevantFeatureMatches <- dplyr::select(featuredMatches, matchId, matchResult, goalsHome, goalsVisitors,
                                            grep(pattern = 'Price', colnames(featuredMatches)),
                                            grep(pattern = 'Bench', colnames(featuredMatches))) #,
    #                                        heimGroupedFormation, auswGroupedFormation)
    ind <- apply(relevantFeatureMatches, 1, function(x) !any(is.na(x)))
    relevantFeatureMatches <- relevantFeatureMatches[ind, ]
    relevantFeatureMatches <- dplyr:::select(relevantFeatureMatches, -tw_Price_Home_min, -tw_Price_Home_max,
                                             -tw_Price_Home_sum, -tw_Price_Visitors_min, -tw_Price_Visitors_max,
                                             -tw_Price_Visitors_sum)
    
    # Create dummy vars for Formations
    #dummies <- dummyVars(~ heimGroupedFormation + auswGroupedFormation, data = relevantFeatureMatches)
    #predDummies <- predict(dummies, relevantFeatureMatches)
    #finalMatches <- cbind(relevantFeatureMatches, as.data.frame(predDummies))
    #finalMatches <- dplyr:::select(finalMatches, -heimGroupedFormation, -auswGroupedFormation)
    
    return(relevantFeatureMatches)
}