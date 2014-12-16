extractMatchResultFeatures <- function(playerStats) {
    # Grouping positions
    colNames <- c('Torwart', 'Innenverteidiger', 'Linker Verteidiger', 'Rechter Verteidiger',
                  'Defensives Mittelfeld', 'Zentrales Mittelfeld', 'Linkes Mittelfeld',
                  'Rechtes Mittelfeld', 'Offensives Mittelfeld', 'Hängende Spitze', 
                  'Mittelstürmer', 'Linksaußen', 'Rechtsaußen')
    # assignedPositions <- c('tw', 'iv', 'fv', 'fv', 'dm', 'mit', 'mit', 'mit', 'om', 'om', 'st', 'fs', 'fs')
    # assignedPositions <- c('tw', 'iv', 'fv', 'fv', 'mit', 'mit', 'mit', 'mit', 'mit', 'mit', 'st', 'st', 'st')
    assignedPositions <- c('def', 'def', 'def', 'def', 'mid', 'mid', 'mid', 'mid', 'mid', 'off', 'off', 'off', 'off')
    groupedPositions <- as.data.frame(t(assignedPositions))
    colnames(groupedPositions) <- colNames
    groupedPositions
    
    featureNames <- sapply(unique(assignedPositions), getFeatureName, price = TRUE, home = TRUE)
    featureNames <- c(featureNames, sapply(unique(assignedPositions), getFeatureName, price = FALSE, home = TRUE))
    featureNames <- c(featureNames, sapply(unique(assignedPositions), getFeatureName, home = FALSE, price = TRUE))
    featureNames <- c(featureNames, sapply(unique(assignedPositions), getFeatureName, home = FALSE, price = FALSE))
    featureNames
    
    matches <- extractMatches(playerStats)
    
    for(j in seq(1:length(featureNames))) {
        featureName <- featureNames[j]
        matches$tmp <- NA
        
        for(i in seq(1:nrow(matches))) {
            actMatch <- matches[i, ]
            
            featureValue <- calcFeatureValue(featureName, playerStats, groupedPositions, 
                                             actMatch$matchId)
            #print(paste('Feature:', featureName, '| Value:', featureValue))
            matches[matches$matchId == actMatch$matchId, 'tmp'] <- 
                featureValue
        }
        
        matches$tmp[is.nan(matches$tmp)] <- NA
        matches <- rename(matches, replace = setNames(featureName, 'tmp'))
    }
    
    matches
}

calcFeatureValue <- function(feature, playerStats, groupedPositions, matchId) {
    
    isHome <- grepl('Home', feature)
    isPrice <- grepl('Price', feature)
    
    relPos <- character()
    if(isPrice) {
        relPos <- strsplit(feature, 'Price')[[1]][1]
    } else {
        relPos <- strsplit(feature, 'Form')[[1]][1]
    }
    
    # Extracting relevant Positions
    relPps <- t(groupedPositions[1, ])[, 1] == relPos 
    relTransPositions <- names(relPps[relPps == TRUE])
    
    # Subsetting to relevant player stats
    featureStats <- playerStats[playerStats$matchId == matchId & playerStats$home == isHome & 
                                    playerStats$transPos %in% relTransPositions, ]
    extractFeature(featureStats, isPrice)
    
}

getFeatureName <- function(pos, price, home) {
    if(price) {
        if(home) {
            return(paste(pos, 'Price', 'Home', sep = ''))
        } else {
            return(paste(pos, 'Price', 'Visitors', sep = ''))
        }
    } else {
        if(home) {
            return(paste(pos, 'Form', 'Home', sep = ''))
        } else {
            return(paste(pos, 'Form', 'Visitors', sep = ''))
        }
    }
}

extractFeature <- function(feaStats, isPrice) {
    if(isPrice) {
        return(mean(feaStats$fitPrice))
    } else {
        return(mean(feaStats$playerForm, na.rm = TRUE))
    }
}