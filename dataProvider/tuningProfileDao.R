loadAllTuningResults <- function(dbName = 'soccerlabdata') {
  con <- dbConnect(MySQL(), user="root", password="root",
                   dbname=dbName)
  findQuery <- 'SELECT * FROM tuningresult res
                       inner join featureengineeringconfig fea on fea.id = res.featureConfig_id
                       inner join xgboosttuninginstance inst on inst.id = res.xgboostConfig_id'
  results <- dbGetQuery(con, findQuery)
  dbDisconnect(con)
  
  # remove duplicate columns
  results <- results[, colnames(results) != 'id' & colnames(results) != 'created' &
                     colnames(results) != 'updated']
  # Extract
  featureConfigs <- select(results, featureSelection, lastFormImpute, lastSaison, lastSpieltag, liga, 
                          pastDays, staticNotPlayedImpute, version)
  xgbConfigs <- select(results, alpha, colsample, eta, gamma, lambda, maxDepth, minChildWeight,
                      nRounds, scalePosWeight, subsample, cvNumber, cvRepeats)
  metrics <- select(results, accuracy, betsPerMatch, bookyAccuracy, bookyKappa, kappa, profitPerc,
                    trainAccuracy, trainKappa, trainProfitPerc, trainValueDiffPerc, valueDiffPerc)
  
  instances <- list('featureConfigs' = featureConfigs, 'xgbConfigs' = xgbConfigs, 'metrics' = metrics)
  return(instances)
}

findTuningProfile <- function(profileId, dbName = 'soccerlabdata') {
  con <- dbConnect(MySQL(), user="root", password="root",
                   dbname=dbName)
  findQuery <- sprintf('SELECT pro.id as id, featConfig.liga as league, featConfig.lastSaison as season, featConfig.lastSpieltag as matchday, 
                       pro.created as created, 
                       conf.cvNumber as cvNumber, conf.cvRepeats as cvRepeats, nround.nRounds as nrounds, 
                       maxdepth.maxDepth as maxDepth, eta.eta as eta, gamma.gamma as gamma,
                       colsamp.colsample as colsample, minchild.minChildWeight as minChildWeight,
                       lambda.lambda as lambda, alpha.alpha as alpha, subsamp.subsample as subsample,
                       scalePos.scalePosWeight as scalePosWeight,
                       featConfig.version as version, featConfig.staticBenchImpute as staticBenchImpute,
                       featConfig.staticNotPlayedImpute as staticNotPlayedImpute, featConfig.pastDays as pastDays,
                       featConfig.lastFormImpute as lastFormImpute, featConfig.featureSelection as featureSelection,
                       featConfig.id as featureId
                       FROM tuningprofile pro
                       INNER JOIN tuningconfig conf on conf.id = pro.tuningconfig_id
                       INNER JOIN xgboosttuningconfig xgbConf on xgbConf.id = conf.id
                       INNER JOIN xgboosttuningconfig_nrounds nround on nround.xgboosttuningconfig_id = conf.id
                       INNER JOIN xgboosttuningconfig_maxdepth maxdepth on maxdepth.xgboosttuningconfig_id = conf.id
                       INNER JOIN xgboosttuningconfig_eta eta on eta.xgboosttuningconfig_id = conf.id
                       INNER JOIN xgboosttuningconfig_gamma gamma on gamma.xgboosttuningconfig_id = conf.id
                       INNER JOIN xgboosttuningconfig_colsample colsamp on colsamp.xgboosttuningconfig_id = conf.id
                       INNER JOIN xgboosttuningconfig_minchildweight minchild on minchild.xgboosttuningconfig_id = conf.id
                       INNER JOIN xgboosttuningconfig_lambda lambda on lambda.xgboosttuningconfig_id = conf.id
                       INNER JOIN xgboosttuningconfig_alpha alpha on alpha.xgboosttuningconfig_id = conf.id
                       INNER JOIN xgboosttuningconfig_subsample subsamp on subsamp.xgboosttuningconfig_id = conf.id
                       INNER JOIN xgboosttuningconfig_scaleposweight scalePos on scalePos.xgboosttuningconfig_id = conf.id
                       INNER JOIN featureengineeringconfig featConfig on featConfig.id = pro.featureConfig_id
    where pro.id = %i', profileId)
  tuning <- dbGetQuery(con, findQuery)
  dbDisconnect(con)
  return(tuning)
}

insertTuningResults <- function(tuningResults, featureId, calcTime, profileId, 
                                minPercProfit, cvNumber, cvRepeats, dbName = 'soccerlabdata') {
  con <- dbConnect(MySQL(), user="root", password="root",
                   dbname=dbName)
  
  for(row in 1:nrow(tuningResults)) {
    result <- tuningResults[row,]
    #Saving Condition
    if(result$profitPerc > minPercProfit) {
      saved <- saveTuningResult(result, featureId, profileId, cvNumber, cvRepeats, con)
      if(saved) {
        loginfo(paste('Saved Tuning Result and updated Tuning Profile:', profileId))
      } else {
        loferror(paste('ERROR while saving Tuning Results and updated Tuning Profile:', profileId))
      }
    }
  }
  
  updateTuningProfile(profileId, calcTime, con)
  dbDisconnect(con)
}

updateTuningProfile <- function(profileId, calcTime, con) {
  # Update Profile
  updateProfileQuery <- sprintf('UPDATE tuningProfile SET updated = NOW(), calcTime = %i 
                                WHERE id = %i', calcTime, profileId)
  logdebug(paste('Execute Query:', updateProfileQuery))
  tmp <- dbSendQuery(con, updateProfileQuery)
  dbClearResult(tmp)
}

saveTuningResult <- function(result, featureId, profileId, cvNumber, cvRepeats, con) {
  # Persist xgboostinstance #
  insertInstanceQuery <- sprintf('INSERT INTO xgboosttuninginstance (created, colsample, eta, gamma, maxDepth, 
                                 minChildWeight, nRounds, alpha, lambda, subsample, scalePosWeight) 
                       VALUES (NOW(), %f, %f, %f, %i, %i, %i, %f, %f, %f, %f)', result$colsample_bytree, result$eta, result$gamma, 
                                 result$max_depth, result$min_child_weight, result$nrounds, result$alpha, 
                                 result$lambda, result$subsample, result$scale_pos_weight)
  logdebug(paste('Execute Query:', insertInstanceQuery))
  tmp <- dbSendQuery(con, insertInstanceQuery)
  xgbId <- dbGetQuery(con, "SELECT LAST_INSERT_ID()")[1,1]
  
  # Persist Result
  insertResultQuery <- sprintf('INSERT INTO tuningresult (created, accuracy, betsPerMatch, bookyAccuracy, bookyKappa,
                               cvNumber, cvRepeats, kappa, profitPerc, trainAccuracy, trainKappa, trainProfitPerc, trainvalueDiffPerc, 
                               valueDiffPerc, featureConfig_id, xgBoostConfig_id) 
                               VALUES (NOW(), %f, %f, %f, %f, %i, %i, %f, %f, %f, %f, %f, %f, %f, %i, %i)', 
                               result$accuracy, result$betsPerMatch, result$bookyAccuracy, result$bookyKappa, 
                               cvNumber, cvRepeats, result$kappa, result$profitPerc, result$trainAccuracy, result$trainKappa,
                               result$trainProfit, result$trainValueDiff, result$valueDiffPerc, featureId, xgbId)
  logdebug(paste('Execute Query:', insertResultQuery))
  tmp <- dbSendQuery(con, insertResultQuery)
  resultId <- dbGetQuery(con, "SELECT LAST_INSERT_ID()")[1,1]
  
  # Create Profile_Result link
  insertProfResQuery <- sprintf('INSERT INTO profile_result (TuningProfile_id, results_id) 
                                VALUES (%i, %i)', profileId, resultId)
  logdebug(paste('Execute Query:', insertProfResQuery))
  tmp <- dbSendQuery(con, insertProfResQuery)
  dbClearResult(tmp)
  
  return(TRUE)
}