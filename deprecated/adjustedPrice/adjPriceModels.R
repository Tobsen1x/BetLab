enrichStatsByAgeAndAvgForm <- function(stats, players) {
    joined <- dplyr::inner_join(stats, players, by = c('playerId' = 'id'))
    stats$birthday <- joined[, 'geburtstag']
    stats <- mutate(stats, age = floor(as.numeric(difftime(as.Date(matchtime), as.Date(birthday), 
                                                  unit="weeks") / 52.25)))
    avgForm <- vector(mode = "numeric", length = 0)
    for(row in 1:nrow(stats)) {
        aktStat <- stats[row, ]
        relevantStats <- filter(stats, as.Date(matchtime) < as.Date(aktStat$matchtime),
                                as.Date(matchtime) > as.Date(aktStat$fitPriceDate),
                                playerId == aktStat$playerId)
        avgForm <- c(avgForm, mean(relevantStats$kickerGrade, na.rm = TRUE))
    }
    stats$avgForm <- avgForm
    stats$avgForm[is.nan(stats$avgForm)] <- NA
    
    stats$pastWeeks <- floor(as.numeric(difftime(as.Date(stats$matchtime), as.Date(stats$fitPriceDate), 
                                                 unit="weeks")))
    return(stats)
}

predictPrice <- function(models, data, seed = seed) {
    resultList <- list()
    for(actName in names(models)) {
        actModel <- models[[actName]]
        preds <- predict(actModel, data)
        resultList <- append(resultList, list(preds))
    }
    names(resultList) <- names(models)
    return(resultList)
}


createExampleObservations <- function() {
    startPrice <- c(1000000.0, 5000000.0, 10000000.0, 40000000.0)
    avgForm <- c(2.0, 3.0, 4.0, 5.0)
    age <- seq(18.0, 35.0, by = 1.0)
    
    resultDataFrame <- data.frame()
    for(i in startPrice) {
        for(j in avgForm) {
            for(k in age) {
                act <- data.frame(startPrice = as.integer(i),
                                  avgForm = as.numeric(j),
                                  age = as.integer(k))
                resultDataFrame <- rbind(resultDataFrame, act)
                
            }
        }
    }
    
    resultDataFrame <- arrange(resultDataFrame, startPrice, avgForm, age)
    return(resultDataFrame)
}

enrichByAgeAdjFitPrice <- function(filteredAdjStatsData, adjStatsData, refAge, seed) {
    formula <- 'fitPrice ~ kickerGrade + age'
    formula <- as.formula(formula)
    adjPriceModels <- fitModels(formula, train = filteredAdjStatsData, seed = seed)
    
    gainPerYear <- summary(adjPriceModels$lm$finalModel)$coefficients['age', 'Estimate']
    age <- seq(16, 44, by = 1)
    priceLambda <- age * gainPerYear
    modPrices <- data.frame(age = age, priceLambda = priceLambda)
    
    adjStatsData$ageAdjFitPrice <- NA
    for(i in 1:nrow(adjStatsData)) {
        actStat <- adjStatsData[i,]
        actPredPriceLambda <- modPrices[modPrices$age == actStat$age, ]$priceLambda
        actPredRefPriceLambda <- modPrices[modPrices$age == refAge, 'priceLambda']
        adjStatsData$ageAdjFitPrice[i] <- actStat$fitPrice + actPredRefPriceLambda - actPredPriceLambda
    }
    
    adjStatsData$ageAdjFitPrice[adjStatsData$ageAdjFitPrice < 0] <- 0
    return(adjStatsData)
}

createStatsExampleObservations <- function() {
    grades <- seq(1.0, 6.0, by = 1.0)
    ages <- seq(16, 44, by = 1)
    
    resultDataFrame <- data.frame()
    for(i in grades) {
        for(j in ages) {
            act <- data.frame(age = as.integer(j),
                              kickerGrade = as.numeric(i))
            resultDataFrame <- rbind(resultDataFrame, act)
        }
    }
    
    resultDataFrame <- arrange(resultDataFrame, kickerGrade, age)
    return(resultDataFrame)
}