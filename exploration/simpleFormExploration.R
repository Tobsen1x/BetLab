source(file = 'rawData/loadData.R', echo = FALSE, encoding = 'UTF-8')

toMatchday <- 34
seasons <- c('2005-2006', '2006-2007', '2007-2008', '2008-2009', '2009-2010', 
             '2010-2011', '2011-2012', '2012-2013', '2013-2014', '2014-2015')
leagues <- c('BL1')

data <- loadTrainingData(toMatchday, seasons, leagues)
stats <- data$stats
matches <- data$matches

######### Match Predicion ##########
library(dplyr)
relSeason <- '2014-2015'
relMatchday <- 8
relMatches <- filter(matches, season == relSeason)
firstMatch <- filter(matches, season == relSeason, matchday == relMatchday)[1, ]
relStats <- filter(stats, season == relSeason)
firstStats <- filter(stats, matchId == firstMatch$matchId)

firstStat <- firstStats[1, ]

##### get simple Form for playerStat #####

# get Formstats for playerStat #
pastStats <- filter(relStats, matchday < firstStat$matchday, playerId == firstStat$playerId)
relDays <- unique(pastStats$matchday)
relDays <- relDays[order(relDays, decreasing = TRUE)]
#TEST
day <- relDays[1]
for(day in relDays) {
    aktStat <- filter(pastStats, matchday == day)
    aktFormStat <- data.frame('matchday' = aktStat$matchday, 
                              'pastWeeks' = getPastWeaks(
                                  firstMatch$matchtime, aktStat$matchtime),
                              'playerAssignment' = aktStat$playerAssignment,
                              'home' = aktStat$home,
                              'grade' = aktStat$grade)
}

# Get weeks 