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

extractMatchResultFeatures <- function(playerStats, matches, 
                                       priceAssignedPositions, functs,
                                       relNormalAssignments = NA) {
    require(dplyr)
    
    ### Assemble relevant positions
    # Grouping positions
    colNames <- levels(playerStats$position)
    groupedPositions <- as.data.frame(t(priceAssignedPositions))
    colnames(groupedPositions) <- colNames
    playerStats$groupedStaticPosition <- sapply(playerStats$staticPosition, FUN = function(x) groupedPositions[1, x])
    
    
    # Name position - price
    featureNames <- sapply(unique(priceAssignedPositions), getFeatureName, home = TRUE, functs = functs)
    featureNames <- c(featureNames, sapply(unique(priceAssignedPositions), getFeatureName, home = FALSE, functs = functs))
    
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
    
    return(matches)
}

calcFeatureValue <- function(feature, playerStats, groupedPositions,
                             relAssignments) {
    isHome <- grepl('Home', feature)
    
    relPos <- strsplit(feature, '_Price')[[1]][1]
    
    # Extracting relevant Positions
    relPps <- t(groupedPositions[1, ])[, 1] == relPos 
    relTransPositions <- names(relPps[relPps == TRUE])
    
    # Extracting function
    func <- strsplit(feature, '_')[[1]][4]
    
    featureStats <- getStatsByPositions(playerStats, isHome, relTransPositions, relAssignments)
    featureResult <- extractFeature(featureStats, func = func)
    
    if(is.na(featureResult)) {
        featureResult <- 0
    }
    return(featureResult)
}

getStatsByPositions <- function(playerStats, isHome, relTransPositions, relAssignments) {
    # Subsetting to relevant player stats
    featureStats <- playerStats[playerStats$home == isHome & 
                                    playerStats$position %in% relTransPositions &
                                    playerStats$playerAssignment %in% relAssignments, ]
    return(featureStats)
}

getFeatureName <- function(pos, home, functs) {
    if(home) {
        return(paste(paste(pos, 'Price', 'Home', sep = '_'), functs, sep = '_'))
    } else {
        return(paste(paste(pos, 'Price', 'Visitors', sep = '_'), functs, sep = '_'))
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

filterFeaturedMatches <- function(featuredMatches) {
    relevantFeatureMatches <- dplyr::select(featuredMatches, matchId, matchResult, 
                                            goalDiff, grep(pattern = 'Price', colnames(featuredMatches)))
    ind <- apply(relevantFeatureMatches, 1, function(x) !any(is.na(x)))
    relevantFeatureMatches <- relevantFeatureMatches[ind, ]
    relevantFeatureMatches <- dplyr:::select(relevantFeatureMatches, -tw_Price_Home_min, -tw_Price_Home_max,
                                             -tw_Price_Home_sum, -tw_Price_Visitors_min, -tw_Price_Visitors_max,
                                             -tw_Price_Visitors_sum)
    return(relevantFeatureMatches)
}