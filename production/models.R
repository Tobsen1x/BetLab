# Split the matches into different folds. Each fold serving as testset.
# Remaining data is split into two different training sets.
splitMatches <- function(matchesToSplit, splitBy, trainingSetRest = NA, 
                         testingMatches, folds, seed) {
    require(caret)
    require(dplyr)
    
    if(is.na(trainingSetRest)) {
        trainingSetRest <- data.frame()
    }
    
    # Split data
    set.seed(seed)
    testFold <- createFolds(splitBy, k = folds,
                            list = FALSE)
    matchesToSplit <- cbind(matchesToSplit, testFold)
    
    allSets <- list()
    for(i in 1:folds) {
        train <- filter(matchesToSplit, testFold != i)
        train <- dplyr:::select(train, -c(testFold))
        trainingMatchIds <- unique(train$matchId)
        
        # Konsolidating trainset and testset
        restMatches <- filter(trainingSetRest, matchId %in% trainingMatchIds)
        train <- rbind(train, restMatches)
        
        #for(j in 1:nrow(train)) {
        #    aktMatch <- train[j, ]
        #    restMatch <- filter(trainingSetRest, matchId == aktMatch$matchId)
        #    train <- rbind(train, restMatch)
        #}
        
        test <- filter(testingMatches, !(matchId %in% trainingMatchIds))
        
        splitData <- list('train' = train, 'test' = test)
        allSets <- append(allSets, list(splitData))
    }
    
    return(allSets)
}

# Custom metric summary function
betMetricsSummary <- function(data, lev, model, probRatioToBet = 1.1, stake = 1) {
    metricData<- cbind('matchId' = filteredFeatureMatches[as.integer(rownames(data)), 'matchId'], data)
    redOdds <- dplyr:::select(odds, matchId, 'bookyVisitorsVictory' = VisitorsVictory, 
                              'bookyDraw' = Draw, 
                              'bookyHomeVictory' = HomeVictory)
    metricData <- merge(metricData,redOdds, sort = FALSE)
    metricData <- dplyr:::mutate(metricData, bookyPred = factor(ifelse(bookyHomeVictory > bookyVisitorsVictory & bookyHomeVictory > bookyDraw,
                                                          'HomeVictory', 
                                                          ifelse(bookyVisitorsVictory > bookyHomeVictory & bookyVisitorsVictory > bookyDraw,
                                                                 'VisitorsVictory', 'Draw')),
                                                          levels = c('VisitorsVictory', 'Draw', 'HomeVictory'), 
                                                          labels = c('VisitorsVictory', 'Draw', 'HomeVictory'), 
                                                          ordered = TRUE))
    
    bookyMetrics <- defaultSummary(dplyr:::select(metricData, obs, 'pred' = bookyPred), lev = c('VisitorsVictory', 'Draw', 'HomeVictory'), model = model)
    names(bookyMetrics) <- c('BookyAccuracy', 'BookyKappa')
    
    defaultMetrics <- defaultSummary(dplyr:::select(metricData, obs, pred), lev = c('VisitorsVictory', 'Draw', 'HomeVictory'), model = model)
    
    # Calculating custom Metrics
    valueDiffVec <- vector(mode = 'numeric')
    placedBets <- data.frame()
    for(i in 1:nrow(metricData)) {
        aktData <- metricData[i, ]
        
        if(aktData$obs == 'HomeVictory') {
            valueDiffVec <- c(valueDiffVec, aktData$HomeVictory - aktData$bookyHomeVictory)
        } else if(aktData$obs == 'VisitorsVictory') {
            valueDiffVec <- c(valueDiffVec, aktData$VisitorsVictory - aktData$bookyVisitorsVictory)
        } else {
            valueDiffVec <- c(valueDiffVec, aktData$Draw - aktData$bookyDraw)
        }
        
        # Bet on HomeVictory
        if(aktData$HomeVictory / aktData$bookyHomeVictory >= 
           probRatioToBet) {
            # Bet won
            if(aktData$obs == 'HomeVictory') {
                matchGain <- 1 / aktData$bookyHomeVictory * stake - stake
            } 
            # Bet lost
            else {
                matchGain <- -stake
            }
            placedBets <- rbind(placedBets, 
                                data.frame(matchId = aktData$matchId,
                                           matchResult = aktData$obs,
                                           betOnOutcome = 'HomeVictory',
                                           predProb = aktData$HomeVictory,
                                           bookyProb = aktData$bookyHomeVictory,
                                           stake = stake,
                                           gain = matchGain))
        }
        # Bet on VisitorsVictory
        if(aktData$VisitorsVictory / aktData$bookyVisitorsVictory >= 
           probRatioToBet) {
            # Bet won
            if(aktData$obs == 'VisitorsVictory') {
                matchGain <- 1 / aktData$bookyVisitorsVictory * stake - stake
            } 
            # Bet lost
            else {
                matchGain <- -stake
            }
            placedBets <- rbind(placedBets, 
                                data.frame(matchId = aktData$matchId,
                                           matchResult = aktData$obs,
                                           betOnOutcome = 'VisitorsVictory',
                                           predProb = aktData$VisitorsVictory,
                                           bookyProb = aktData$bookyVisitorsVictory,
                                           stake = stake,
                                           gain = matchGain))
        }
        # NO Bets on Draw
    }
    valueDiff <- mean(valueDiffVec)
    stake <- sum(placedBets$stake)
    gain <- sum(placedBets$gain)
    
    gainPerc <- 0.0
    if(stake > 0) {
        gainPerc <- as.numeric(gain) / as.numeric(stake) * 100
    }
    
    allMetrics <- c(defaultMetrics, bookyMetrics, 'GainPerc' = gainPerc, 'ValueDiffPerc' = valueDiff * 100)
    return(allMetrics)
}

#Multi-Class Summary Function implemented by Zach Mayer
#Based on caret:::twoClassSummary
multiClassSummary <- function (data, lev = NULL, model = NULL){
    
    #Load Libraries
    require(compiler)
    require(Metrics)
    require(caret)
    
    print(nrow(data))
    
    #Check data
    if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
        stop("levels of observed and predicted data do not match")
    
    #Calculate custom one-vs-all stats for each class
    prob_stats <- lapply(levels(data[, "pred"]), function(class){
        
        #Grab one-vs-all data for the class
        pred <- ifelse(data[, "pred"] == class, 1, 0)
        obs  <- ifelse(data[,  "obs"] == class, 1, 0)
        prob <- data[,class]
        
        #Calculate one-vs-all AUC and logLoss and return
        cap_prob <- pmin(pmax(prob, .000001), .999999)
        prob_stats <- c(auc(obs, prob), logLoss(obs, cap_prob))
        names(prob_stats) <- c('ROC', 'logLoss')
        return(prob_stats) 
    })
    prob_stats <- do.call(rbind, prob_stats)
    rownames(prob_stats) <- paste('Class:', levels(data[, "pred"]))
    
    #Calculate confusion matrix-based statistics
    CM <- confusionMatrix(data[, "pred"], data[, "obs"])
    
    #Aggregate and average class-wise stats
    #Todo: add weights
    class_stats <- cbind(CM$byClass, prob_stats)
    class_stats <- colMeans(class_stats)
    
    #Aggregate overall stats
    overall_stats <- c(CM$overall)
    
    #Combine overall with class-wise stats and remove some stats we don't want 
    stats <- c(overall_stats, class_stats)
    stats <- stats[! names(stats) %in% c('AccuracyNull', 
                                         'Prevalence', 'Detection Prevalence')]
    
    #Clean names and return
    names(stats) <- gsub('[[:blank:]]+', '_', names(stats))
    return(stats)
}