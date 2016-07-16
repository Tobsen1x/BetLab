fillAdjGrade <- function(playerStats, seed, cvNumber) {
  assignedPositions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 
                         'mid', 'off', 'off', 'off', 'off', 'off')
  groupedPos <- groupPositions(playerStats$position, assignedPositions)
  
  playerStats$groupedPosition <- groupedPos
  modelInput <- filter(playerStats, !is.na(grade), !is.na(fitPrice), !is.na(groupedPosition))
  modelInput <- select(modelInput, matchId, playerId, grade, fitPrice, groupedPosition, home)
  
  cvContr <- trainControl(method = 'cv', number = cvNumber)
  gradeFormula <- as.formula('grade ~ . -matchId -playerId')
  
  lmModel <- train(form = gradeFormula, data = modelInput, method = 'lm',
                   trControl = cvContr)
  loginfo('Purged Grade model:')
  loginfo(paste(names(lmModel$results), lmModel$results))
  
  modelInput$predGrade <- predict(lmModel, modelInput)
  modelInput$purgedGrade <- modelInput$predGrade - modelInput$grade
  
  resultStats <- merge(playerStats, select(modelInput, matchId, playerId, purgedGrade), 
                 by = c('matchId', 'playerId'), all.x = TRUE, sort = FALSE)
  
  return(resultStats)
}