### Load Data
source(file = 'loadData.R', echo = FALSE, encoding = 'UTF-8')
data <- loadData('BL1')
stats <- data$playerStats
matches <- data$matches
odds <- data$odds
# --> loadedData.RData

### Enrich with adjGrades
source(file = 'adjGradeModel.R', echo = FALSE, encoding = 'UTF-8')
adjGradeData <- extractFeaturesForAdjGradeModel(stats)
enrichedStats <- enrichAdjGrade(adjGradeData)
# Merges adjusted grade in stats
mergedStats <- merge(stats, select(
    enrichedStats, matchId, playerId, adjGrade),
    by = c('matchId', 'playerId'),  all.x = TRUE)
summary(mergedStats$adjGrade)

### Enrich diagonal inflated bivariate poisson expected goals
load('data/bivPois.RData')
source(file = 'bivpois/estimateGoals.R', echo = FALSE, encoding = 'UTF-8')
enrMatches <- enrichBivPoisExpGoals(matches, minMatchdays = 5)
library(psych)
describe(select(enrMatches, goalsHome, goalsVisitors, 
                homeExpGoals, visitorsExpGoals))
# --> expGoalsEnrichedData.RData

### Enrich double poisson expected chances
source(file = 'bivpois/estimateChances.R', echo = FALSE, encoding = 'UTF-8')
chancesEnrMatches <- enrichDoublePoissonExpChances(enrMatches, minMatchdays = 5)
describe(select(chancesEnrMatches, homeChances, visitorsChances, 
                homeExpChances, visitorsExpChances))

### Enrich player form
source(file = 'formModel.R', echo = FALSE, encoding = 'UTF-8')
suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
library(psych)

#formEnrichedPlayerStats <- enrichForm(mergedStats, matches, minMatchdays = 5,
#                                      imputeBenchBy = -0.05, imputeNotPlayedBy = -0.1)
# --> formEnrichedData_staticImpute.RData

formEnrichedPlayerStats <- enrichForm(mergedStats, matches, minMatchdays = 5)
# --> formEnrichedData_dynamicImpute.RData
describe(select(formEnrichedPlayerStats, adjGrade, meanf, 
                sesOptimal, sesSimple, formQuality))
summary(select(formEnrichedPlayerStats, adjGrade, meanf, 
               sesOptimal, sesSimple, formQuality))

### Feature extraction
source(file = 'featureExtraction.R', echo = FALSE, encoding = 'UTF-8')
featuredMatches <- extractFeatures(formEnrichedPlayerStats, chancesEnrMatches)
describe(select(featuredMatches, priceDiff, logPriceRate, formArimaDiff, formHoltWintersDiff,
                formNaiveDiff, formMeanfDiff, formDriftDiff, expGoalDiff))
