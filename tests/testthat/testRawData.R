context("Raw Data")
source(file = paste(Sys.getenv('PROJECT_PATH'), 'dataProvider/loadData.R', sep = ''), 
       echo = FALSE, encoding = 'UTF-8')
source(file = 'C:/RStudioWorkspace/BetLab/dataProvider/matchesDao.R', echo = FALSE, encoding = 'UTF-8')
source(file = 'C:/RStudioWorkspace/BetLab/dataProvider/playerStatDao.R', echo = FALSE, encoding = 'UTF-8')

# Matches
seasons <- c('2014-2015', '2015-2016')
toMatchday <- 10
matches <- loadFinishedMatches(toMatchday, seasons = seasons, c('BL1'))

test_that('loadFinishedMatches test', {
  errorMatches <- filter(matches, season == '2015-2016', matchday > toMatchday)
  expect_true(nrow(errorMatches) == 0)
  expect_equal(nrow(matches), 396)
})

# PlayerStats
matchIds <- matches$matchId
playerStats <- loadPlayerStats(matchIds, fitPriceImpute = 50000)

test_that('Player Stats assignment group test', {
  statsGroup <- group_by(playerStats, matchId, home, playerAssignment)
  statsSum <- summarise(statsGroup, sum = n())
  assignmentGroups <- spread(statsSum, playerAssignment, sum)
  expect_true(max(assignmentGroups$EINGEWECHSELT, na.rm = TRUE) == 3)
  # Usually <= 3, but someimes it is 4 probably because of injuries -> TOPROVE
  expect_true(max(assignmentGroups$AUSGEWECHSELT, na.rm = TRUE) <= 4)
  expect_true(max(assignmentGroups$DURCHGESPIELT, na.rm = TRUE) == 11)
  expect_true(max(assignmentGroups$BENCH, na.rm = TRUE) == 7)
  
  expect_true(min(assignmentGroups$EINGEWECHSELT, na.rm = TRUE) >= 0)
  expect_true(min(assignmentGroups$AUSGEWECHSELT, na.rm = TRUE) >= 0)
  expect_true(min(assignmentGroups$DURCHGESPIELT, na.rm = TRUE) >= 7)
  expect_true(min(assignmentGroups$BENCH, na.rm = TRUE) >= 1)
})

# Odds
odds <- loadOdds(matchIds, 'SfStat')

test_that('odds test', {
  expect_equal(nrow(odds), nrow(matches))
})

# Everything
trainData <- loadTrainingData(toMatchday, seasons, c('BL1'), fitPriceImpute = 50000)
