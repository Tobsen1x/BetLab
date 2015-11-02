toMatchday <- 38
seasons <- c('2005-2006', '2006-2007', '2007-2008', '2008-2009', '2009-2010',
             '2010-2011', '2011-2012', '2012-2013', '2013-2014', '2014-2015')
leagues <- c('BL1', 'PL')

source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/production/loadData.R', 
       echo = FALSE, encoding = 'UTF-8')
trainingData <- loadTrainingData(toMatchday, seasons, leagues)

baseDataFilename <- 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/rds/2005-2015_1_20150829.rds'
saveRDS(trainingData, baseDataFilename)

trainingData <- readRDS(baseDataFilename)
source(file = 'C:/Users/Tobsen1X/RStudioWorkspace/BetLab/production/positionFeatureExtraction.R', 
       echo = FALSE, encoding = 'UTF-8')

### Preparation
#[1] "Torwart"               "Innenverteidiger"      "Linker Verteidiger"    "Rechter Verteidiger"   "Defensives Mittelfeld"
#[6] "Zentrales Mittelfeld"  "Linkes Mittelfeld"     "Rechtes Mittelfeld"    "Offensives Mittelfeld" "Haengende Spitze"     
#[11] "Mittelstuermer"        "Linksaussen"           "Rechtsaussen"
positions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 'mid', 'off', 'off', 'off', 'off', 'off')
relNormalAssignments <- c('DURCHGESPIELT', 'EINGEWECHSELT', 'AUSGEWECHSELT')
# Nur Startelf
#   relAssignments <- c('DURCHGESPIELT', 'AUSGEWECHSELT')
normalMatches <- extractMatchResultFeatures(playerStats = trainingData$stats,
                                              matches = trainingData$matches,
                                              priceAssignedPositions = positions,
                                              functs = c('min', 'max', 'avg', 'sum'), 
                                            'normal', relNormalAssignments)

derivedNormalMatches <- addDerivedFeatures(normalMatches)
filteredDerivedNormalMatches <- filterFeaturedMatches(derivedNormalMatches)
saveRDS(filteredDerivedNormalMatches, 'C:/BetLab/SoccerLab/ModelFiles/normalFeatured.rds')


predAufstellungMatches <- extractMatchResultFeatures(playerStats = trainingData$stats,
                                            matches = trainingData$matches,
                                            priceAssignedPositions = positions,
                                            functs = c('min', 'max', 'avg', 'sum'), 
                                            'predAufstellung')

derivedPredAufstellungMatches <- addDerivedFeatures(predAufstellungMatches)
filteredDerivedPredAufstellungMatches <- filterFeaturedMatches(derivedPredAufstellungMatches)
saveRDS(filteredDerivedPredAufstellungMatches, 'C:/BetLab/SoccerLab/ModelFiles/predAufstellungFeatured.rds')