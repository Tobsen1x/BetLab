fillByAllHomeBenchmark <- function(inputData) {
  enrichedData <- mutate(inputData,
                         HomeVictory = 1.0, VisitorsVictory = 0.0, Draw = 0.0)
  enrichedData <- addPred(enrichedData)
  
  enrichedData <- select(enrichedData, pred, obs, VisitorsVictory, Draw, HomeVictory, rowIndex)
  return(enrichedData)
}

fillByAllRandom <- function(inputData) {
  enrichedData <- mutate(inputData,
                         HomeVictory = runif(nrow(inputData), 0, 1), 
                         VisitorsVictory = runif(nrow(inputData), 0, (1.0 - HomeVictory)), 
                         Draw = runif(nrow(inputData), 0, (1.0 - HomeVictory - VisitorsVictory)))
  enrichedData <- addPred(enrichedData)
  
  enrichedData <- select(enrichedData, pred, obs, VisitorsVictory, Draw, HomeVictory, rowIndex)
  return(enrichedData)
}

# Bet metric summary function
# matchData contains VisitorsOdd, DrawOdd, HomeOdd, VisitorsPred, DrawPred, HomePred
betMetricsSummary <- function(matchData, probRatioToBet, stake) {
  # Booky Metrics
  x <- select(matchData, VisitorsOdd, DrawOdd, HomeOdd)
  y <- matchData$matchResult
  predBooky <- as.factor(predictOutcome(x))
  predBooky <- mapvalues(predBooky, from = c("HomeOdd", "VisitorsOdd", "DrawOdd"), 
            to = c("HomeVictory", "VisitorsVictory", "Draw"))
  # Add Draw
  if(length(levels(predBooky)) < 3) {
    predBooky <- factor(predBooky, levels=c(levels(predBooky), 'Draw'))
  }
  predBooky <- factor(predBooky, levels = c('VisitorsVictory', 'Draw', 'HomeVictory'), 
                      labels = c('VisitorsVictory', 'Draw', 'HomeVictory'), 
                      ordered = TRUE)
  bookyConf <- confusionMatrix(predBooky, y)
  
  bookyMetrics <- data.frame('bookyAccuracy' = bookyConf$overall['Accuracy'],
                       'bookyKappa' = bookyConf$overall['Kappa'], row.names = NULL)
  
  # Pred Metrics
  xPred <- select(matchData, VisitorsPred, DrawPred, HomePred)
  pred <- as.factor(predictOutcome(xPred))
  pred <- mapvalues(pred, from = c("HomePred", "VisitorsPred", "DrawPred"), 
                         to = c("HomeVictory", "VisitorsVictory", "Draw"))
  # Add Draw
  if(length(levels(pred)) < 3) {
    pred <- factor(pred, levels=c(levels(pred), 'Draw'))
  }
  pred <- factor(pred, levels = c('VisitorsVictory', 'Draw', 'HomeVictory'), 
                      labels = c('VisitorsVictory', 'Draw', 'HomeVictory'), 
                      ordered = TRUE)
  predConf <- confusionMatrix(pred, y)
  
  predMetrics <- data.frame('accuracy' = predConf$overall['Accuracy'],
                             'kappa' = predConf$overall['Kappa'], row.names = NULL)
  
  # Calculating custom Metrics
  valueDiffVec <- vector(mode = 'numeric')
  placedBets <- data.frame()
  for(i in 1:nrow(matchData)) {
    aktData <- matchData[i, ]
    
    if(aktData$matchResult == 'HomeVictory') {
      valueDiffVec <- c(valueDiffVec, aktData$HomePred - aktData$HomeOdd)
    } else if(aktData$matchResult == 'VisitorsVictory') {
      valueDiffVec <- c(valueDiffVec, aktData$VisitorsPred - aktData$VisitorsOdd)
    } else {
      valueDiffVec <- c(valueDiffVec, aktData$DrawPred - aktData$DrawOdd)
    }
    
    # Bet on HomeVictory
    if(aktData$HomePred / aktData$HomeOdd >= 
       probRatioToBet) {
      # Bet won
      if(aktData$matchResult == 'HomeVictory') {
        matchGain <- 1 / aktData$HomeOdd * stake - stake
      } 
      # Bet lost
      else {
        matchGain <- -stake
      }
      placedBets <- rbind(placedBets, 
                          data.frame(matchId = aktData$matchId,
                                     matchResult = aktData$matchResult,
                                     betOnOutcome = 'HomeVictory',
                                     predProb = aktData$HomePred,
                                     bookyProb = aktData$HomeOdd,
                                     stake = stake,
                                     gain = matchGain))
    }
    # Bet on VisitorsVictory
    if(aktData$VisitorsPred / aktData$VisitorsOdd >= 
       probRatioToBet) {
      # Bet won
      if(aktData$matchResult == 'VisitorsVictory') {
        matchGain <- 1 / aktData$VisitorsOdd * stake - stake
      } 
      # Bet lost
      else {
        matchGain <- -stake
      }
      placedBets <- rbind(placedBets, 
                          data.frame(matchId = aktData$matchId,
                                     matchResult = aktData$matchResult,
                                     betOnOutcome = 'VisitorsVictory',
                                     predProb = aktData$VisitorsPred,
                                     bookyProb = aktData$VisitorsOdd,
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
  
  allMetrics <- cbind(data.frame('profitPerc' = gainPerc, 'valueDiffPerc' = valueDiff * 100), 
                      predMetrics, bookyMetrics, 'betsPerMatch' = as.numeric(nrow(placedBets) / nrow(matchData)))
  return(allMetrics)
}

predictOutcome <- function(x) {
  eins <- x[,1] >= x[,2] & x[,1] >= x[,3]
  zwei <- x[,2] > x[,1] & x[,2] >= x[,3]
  drei <- x[,3] > x[,1] & x[,3] > x[,2]
  
  binTable <- data.frame(eins, zwei, drei)
  colnames(binTable) <- colnames(x)
  # Assert equals 0
  assertZero <- sum(apply(as.matrix(binTable), 1, sum) != 1)
  if(assertZero != 0) {
    logwarn('Error in predictOutcome.')
  }
  
  result <- apply(binTable, 1, FUN = function(z) {
    ifelse(z[1], colnames(binTable)[1],
           ifelse(z[2], colnames(binTable)[2],
                  ifelse(z[3], colnames(binTable)[3], NA)))
  })
  return(result)
}

#Multi-Class Summary Function implemented by Zach Mayer
#Based on caret:::twoClassSummary
multiClassSummary <- function (data, lev = NULL, model = NULL){
  
  #Load Libraries
  require(compiler)
  require(Metrics)
  require(caret)
  
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