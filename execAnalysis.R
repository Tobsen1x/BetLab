### Load Data
source(file = 'loadData.R', echo = FALSE, encoding = 'UTF-8')
data <- loadData('BL1')
stats <- data$playerStats
odds <- data$odds

### Enrich with adjGrades
source(file = 'adjGradeModel.R', echo = FALSE, encoding = 'UTF-8')
adjGradeStats <- enrichAdjGrade(stats)
# Merges adjusted grade in stats
mergedStats <- merge(stats, subset(adjGradeStats, select = c(matchId, playerId, adjGrade)),
                     by = c('matchId', 'playerId'),  all.x = TRUE)

### Enrich with form data
source(file = 'formModel.R', echo = FALSE, encoding = 'UTF-8')
formEnrichedPlayerStats <- enrichForm(mergedStats, 'beta',  
                                      maxMatchdays = 8, minMatchdays = 3)

### Simple Feature Extraction for matches
source(file = 'simpleResultFeatureExtraction.R', echo = FALSE, encoding = 'UTF-8')
simpleMatcheFeatures <- simpleMatchFeatureExtract(formEnrichedPlayerStats)

