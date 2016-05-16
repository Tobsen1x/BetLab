extractMatchResultFeatures <- function(playerStats, 
                                       matches, 
                                       assignedPositions, 
                                       relNormalAssignments = NA, 
                                       priceFuncts = NA,
                                       formFuncts = c(),
                                       benchPriceFuncts = NA,
                                       benchFormFuncts = c()) {
  ### Assemble relevant positions
  # Grouping positions
  colNames <- levels(playerStats$position) 
  groupedPositions <- as.data.frame(t(assignedPositions))
  colnames(groupedPositions) <- colNames
  playerStats$groupedPosition <- groupPositions(playerStats$position, assignedPositions)
  
  featureNames <- extractFeatureNames(assignedPositions, priceFuncts, formFuncts, 
                                      benchPriceFuncts, benchFormFuncts)
  
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
  
  matches <- imputeFeaturedValues(matches)
  matches <- selectRelevantFeatures(matches)
  matches <- arrange(matches, desc(matchtime), homeTeamId)
  
  return(matches)
}

# Requires symmetrical Home and Visitors Features
extractInteractionFeatures <- function(featuredMatches, priceImpute = 50000) {
  featureCols <- grepl('_Price', colnames(featuredMatches)) |
    grepl('_Form', colnames(featuredMatches))
  features <- featuredMatches[, featureCols]
  nonFeatures <- featuredMatches[, !featureCols]
  homeFeatures <- features[, grepl('_Home_', colnames(features))]
  
  interactFeaturedMatches <- data.frame()
  for(col in colnames(homeFeatures)) {
    homeFeat <- features[, col]
    # Replace Home with visitors
    visFeatName <- gsub('_Home_', '_Visitors_', col)
    visitorsFeat <- features[, visFeatName]
    
    ### Impute Price Feature ###
    if(grepl('_Price', col)) {
      homeFeat[homeFeat == 0] <- priceImpute
      visitorsFeat[visitorsFeat == 0] <- priceImpute
    }
    
    ### Interact Features ###
    diffFeature <- homeFeat - visitorsFeat
    diffColName <- gsub('_Home_', '_Diff_', col)
    diffFea <- data.frame(diffFeature)
    colnames(diffFea) <- diffColName
    if(nrow(interactFeaturedMatches) == 0) {
      interactFeaturedMatches <- diffFea
    } else {
      interactFeaturedMatches <- cbind(interactFeaturedMatches, diffFea)
    }
    if(grepl('_Price', col)) {
      ratioFeature <- log(homeFeat / visitorsFeat)
      ratioColName <- gsub('_Home_', '_Ratio_', col)
      ratioFea <- data.frame(ratioFeature)
      colnames(ratioFea) <- ratioColName
      interactFeaturedMatches <- cbind(interactFeaturedMatches, ratioFea)
    }
  }
  
  interactFeaturedMatches <- cbind(nonFeatures, interactFeaturedMatches)
  return(interactFeaturedMatches)
}

groupPositions <- function(positions, assignedPositions) {
  colNames <- levels(positions)
  groupedPositions <- as.data.frame(t(assignedPositions))
  colnames(groupedPositions) <- colNames
  groupedPos <- sapply(positions, FUN = function(x) groupedPositions[1, x])
  return(groupedPos)
}


# Imputation should just be for bench features,
# so it is not that important
imputeFeaturedValues <- function(featuredMatches, staticPriceImpute = 50000, staticFormImpute = -0.25) {
  # Impute Price features
  priceColIndexes <- grep('Price', colnames(featuredMatches))
  priceFeatures <- featuredMatches[, priceColIndexes]
  priceFeatures[is.na(priceFeatures)] <- staticPriceImpute
  featuredMatches[, priceColIndexes] <- priceFeatures
  
  # Impute Form features
  formColIndexes <- grep('Form', colnames(featuredMatches))
  formFeatures <- featuredMatches[, formColIndexes]
  formFeatures[is.na(formFeatures)] <- staticFormImpute
  featuredMatches[, formColIndexes] <- formFeatures
  
  return(featuredMatches)
}

extractFeatureNames <- function(assignedPositions, priceFuncts, formFuncts, 
                    benchPriceFuncts, benchFormFuncts) {
  # Name price features
  featureNames <- sapply(unique(assignedPositions), getFeatureName, 
                         group = 'Price', home = TRUE, functs = priceFuncts)
  featureNames <- c(featureNames, sapply(unique(assignedPositions), getFeatureName, 
                                         group = 'Price', home = FALSE, functs = priceFuncts))
  
  if(length(formFuncts) > 0) {
    # Name form features
    featureNames <- c(featureNames, sapply(unique(assignedPositions), getFeatureName, 
                                          group = 'Form', home = TRUE, functs = formFuncts))
    featureNames <- c(featureNames, sapply(unique(assignedPositions), getFeatureName, 
                                          group = 'Form', home = FALSE, functs = formFuncts))
  }
  
  # For bench features just take positions def, mid, off
  benchPositions <- unique(assignedPositions)
  benchPositions <- benchPositions[!benchPositions %in% 'tw']
  # Add Bench Price features
  if(length(benchPriceFuncts) > 0) {
    featureNames <- c(featureNames, sapply(benchPositions, getFeatureName, 
                                           group = 'Price_Bench', home = TRUE, functs = benchPriceFuncts))
    featureNames <- c(featureNames, sapply(benchPositions, getFeatureName, 
                                           group = 'Price_Bench', home = FALSE, functs = benchPriceFuncts))
  }
  # Add Bench Form features
  if(length(benchFormFuncts) > 0) {
    featureNames <- c(featureNames, sapply(benchPositions, getFeatureName, 
                                           group = 'Form_Bench', home = TRUE, functs = benchFormFuncts))
    featureNames <- c(featureNames, sapply(benchPositions, getFeatureName, 
                                           group = 'Form_Bench', home = FALSE, functs = benchFormFuncts))
  }
  return(featureNames)
}

# Not used yet
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

calcFeatureValue <- function(feature, playerStats, groupedPositions,
                             relAssignments) {
  relSplits <- strsplit(feature, '_')[[1]]
  relPos <- relSplits[1]
  isHome <- relSplits[2] == 'Home'
  relFunc <- relSplits[3]
  relGroup <- relSplits[4]
  # for bench features
  if(length(relSplits) > 4) {
    relGroup <- paste(relGroup, relSplits[5], sep = '_')
  }
  
  # For Price features, the concrete position is necessary
  if(relGroup == 'Price' | relGroup == 'Form') {
    # Extracting relevant Positions
    relPps <- t(groupedPositions[1, ])[, 1] == relPos 
    relPositions <- names(relPps[relPps == TRUE])
  } else if(relGroup == 'Price_Bench' | relGroup == 'Form_Bench') {
    # For bench features, just the grouped position is necessary
    relPositions <- relPos
    relAssignments <- c('BENCH', 'EINGEWECHSELT')
  } else {
    stop('Error')
  }
  
  featureStats <- getStatsByPositions(playerStats, isHome, relGroup, 
                                      relPositions, relAssignments)
  featureResult <- extractFeature(featureStats, func = relFunc, group = relGroup)
  
  return(featureResult)
}

getStatsByPositions <- function(playerStats, isHome, relGroup, 
                                relPositions, relAssignments) {
  if(relGroup == 'Price' | relGroup == 'Form') {
    # Subsetting to relevant player stats
    featureStats <- playerStats[playerStats$home == isHome & 
                                  playerStats$position %in% relPositions &
                                  playerStats$playerAssignment %in% relAssignments, ]
  } else if(relGroup == 'Price_Bench' | relGroup == 'Form_Bench') {
    featureStats <- playerStats[playerStats$home == isHome & 
                                  playerStats$groupedPosition %in% relPositions &
                                  playerStats$playerAssignment %in% relAssignments, ]
  }
  return(featureStats)
}

getFeatureName <- function(pos, group, home, functs) {
  if(home) {
    return(paste(pos, 'Home', functs, group, sep = '_'))
  } else {
    return(paste(pos, 'Visitors', functs, group, sep = '_'))
  }
}

applyGroupingFunction <- function(func, data) {
  if(func == 'sum') {
    return(sum(data, na.rm = TRUE))
  } else if(func == 'avg') {
    return(mean(data, na.rm = TRUE))
  } else if(func == 'min') {
    if(sum(!is.na(data)) == 0) {
      return(NA)
    }
    return(min(data, na.rm = TRUE))
  } else if(func == 'max') {
    if(sum(!is.na(data)) == 0) {
      return(NA)
    }
    return(max(data, na.rm = TRUE))    
  } else {
    stop(paste('Wrong function:', func))
  }
}

extractFeature <- function(feaStats, func, group) {
  if(group == 'Price' | group == 'Price_Bench')
    result <- applyGroupingFunction(func, feaStats$fitPrice)
  else if(group == 'Form' | group == 'Form_Bench') {
    result <- applyGroupingFunction(func, feaStats$formForecast)
  } else {
    stop(paste('Wrong group:', group))
  }
}

selectRelevantFeatures <- function(featuredMatches) {
  containsForm <- sum(grepl('_Form', colnames(featuredMatches))) > 0
  relevantFeatureMatches <- select(featuredMatches, 
                                   -tw_Home_min_Price, 
                                   -tw_Home_max_Price,
                                   -tw_Home_sum_Price,
                                   -tw_Visitors_min_Price, 
                                   -tw_Visitors_max_Price,
                                   -tw_Visitors_sum_Price)
  
  if(containsForm) {
    relevantFeatureMatches <- select(relevantFeatureMatches, 
                                     -tw_Home_min_Form, 
                                     -tw_Home_max_Form,
                                     -tw_Visitors_min_Form, 
                                     -tw_Visitors_max_Form)
  }
  
  ### Formations are disabled for now
  # Create dummy vars for Formations
  #dummies <- dummyVars(~ heimGroupedFormation + auswGroupedFormation, data = relevantFeatureMatches)
  #predDummies <- predict(dummies, relevantFeatureMatches)
  #finalMatches <- cbind(relevantFeatureMatches, as.data.frame(predDummies))
  #finalMatches <- dplyr:::select(finalMatches, -heimGroupedFormation, -auswGroupedFormation)
  
  return(relevantFeatureMatches)
}