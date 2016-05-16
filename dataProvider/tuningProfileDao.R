findTuningProfile <- function(profileId, dbName = 'soccerlabdata') {
  con <- dbConnect(MySQL(), user="root", password="root",
                   dbname=dbName)
  findQuery <- sprintf('SELECT pro.id as id, pro.rawDataFileName as rawDataFileName, pro.created as created, 
                       conf.cv as cv, nround.nRounds as nrounds, maxdepth.maxDepth as maxDepth, eta.eta as eta, gamma.gamma as gamma,
                       colsamp.colsample as colsample, minchild.minChildWeight as minChildWeight,
                       featConfig.version as version, featConfig.staticBenchImpute as staticBenchImpute,
                       featConfig.staticNotPlayedImpute as staticNotPlayedImpute, featConfig.pastDays as pastDays,
                       featConfig.lastFormImpute as lastFormImpute, featConfig.featureSelection as featureSelection
                       FROM tuningprofile pro
                       INNER JOIN tuningconfig conf on conf.id = pro.tuningconfig_id
                       INNER JOIN xgboosttuningconfig xgbConf on xgbConf.id = conf.id
                       INNER JOIN xgboosttuningconfig_nrounds nround on nround.xgboosttuningconfig_id = conf.id
                       INNER JOIN xgboosttuningconfig_maxdepth maxdepth on maxdepth.xgboosttuningconfig_id = conf.id
                       INNER JOIN xgboosttuningconfig_eta eta on eta.xgboosttuningconfig_id = conf.id
                       INNER JOIN xgboosttuningconfig_gamma gamma on gamma.xgboosttuningconfig_id = conf.id
                       INNER JOIN xgboosttuningconfig_colsample colsamp on colsamp.xgboosttuningconfig_id = conf.id
                       INNER JOIN xgboosttuningconfig_minchildweight minchild on minchild.xgboosttuningconfig_id = conf.id
                       INNER JOIN featureengineeringconfig featConfig on featConfig.id = pro.featureConfig_id
    where pro.id = %i', profileId)
  tuning <- dbGetQuery(con, findQuery)
  
  return(tuning)
}

insertTuningResults <- function(tuneConfig, calcTime, profileId, dbName = 'soccerlabdata') {
  con <- dbConnect(MySQL(), user="root", password="root",
                   dbname=dbName)
  insertInstanceQuery <- sprintf('INSERT INTO xgboosttuninginstance (created, colsample, eta, gamma, maxDepth, minChildWeight, nRounds) 
                       VALUES (NOW(), %f, %f, %f, %i, %i, %i)', tuneConfig$colsample_bytree, tuneConfig$eta, tuneConfig$gamma, 
                                 tuneConfig$max_depth, tuneConfig$min_child_weight, tuneConfig$nrounds)
  loginfo(paste('Execute Query:', insertInstanceQuery))
  tmp <- dbSendQuery(con, insertInstanceQuery)
  gridId <- dbGetQuery(con, "SELECT LAST_INSERT_ID()")[1,1]
  
  # Insert into TuningResult
  insertResultQuery <- sprintf('INSERT INTO tuningResult (created, accuracy, bookyAccuracy, profitPerc, valueDiffPerc, bestTune_id) 
                               VALUES (NOW(), %f, %f, %f, %f, %i)', tuneConfig$Accuracy, tuneConfig$BookyAccuracy,
                               tuneConfig$GainPerc, tuneConfig$ValueDiffPerc, gridId)
  loginfo(paste('Execute Query:', insertResultQuery))
  tmp <- dbSendQuery(con, insertResultQuery)
  resultId <- dbGetQuery(con, "SELECT LAST_INSERT_ID()")[1,1]
  
  # Update TuneProfile
  updateProfileQuery <- sprintf('UPDATE tuningProfile SET tuningResult_id = %i, calcTime = %i, locked = %i WHERE id = %i',
                                resultId, as.integer(calcTime), 0, profileId)
  loginfo(paste('Execute Query:', updateProfileQuery))
  tmp <- dbSendQuery(con, updateProfileQuery)
}