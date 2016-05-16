data <- readRDS(file="data/BL1_2005-2015.Rds")

stats <- data$stats
matches <- data$matches

# Select model input
source(file = 'featureEngineering/positionFeatureExtraction.R', echo = FALSE, encoding = 'UTF-8')
assignedPositions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 
                       'mid', 'off', 'off', 'off', 'off', 'off')
groupedPos <- groupPositions(stats$position, assignedPositions)
describe(groupedPos)

playerStats <- stats
playerStats$groupedPosition <- groupedPos
modelInput <- filter(playerStats, !is.na(grade), !is.na(fitPrice), !is.na(groupedPosition))
modelInput <- select(modelInput, matchId, playerId, grade, fitPrice, groupedPosition)
describe(modelInput)

seed <- 16450
cvContr <- trainControl(method = 'cv', number = 10)
gradeFormula <- as.formula('grade ~ . -matchId -playerId')

lmModel <- train(form = gradeFormula, data = modelInput, method = 'lm',
                 trControl = cvContr)
lmModel
modelInput$predGrade <- predict(lmModel, modelInput)
modelInput$purgedGrade <- modelInput$predGrade - modelInput$grade

stats <- merge(stats, select(modelInput, matchId, playerId, purgedGrade), 
           by = c('matchId', 'playerId'), all.x = TRUE, sort = FALSE)
data$stats <- stats
saveRDS(data, file="data/BL1_2005-2015.Rds")

describe(data$stats$purgedGrade)

