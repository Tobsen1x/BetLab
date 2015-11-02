require(dplyr)
require(tidyr)
source(file = 'loadData.R', echo = FALSE, encoding = 'UTF-8')

fromMatchday <- 1
toMatchday <- 34
seasons <- c('2007-2008', '2008-2009', '2009-2010', 
             '2010-2011', '2011-2012', '2012-2013', '2013-2014', '2014-2015')
leagues <- c('BL1')

data <- loadTrainingData(fromMatchday, toMatchday, seasons, leagues)
stats <- data$stats

# Exploring match - player - stats
describe(dplyr:::select(data$stats, season, position, playerAssignment, fitPrice))
# All positions have to be set
filter(data$stats, is.na(position))

matchStats <- summarise(group_by(stats, matchId, home, playerAssignment), sum = n())
tidyMatchStats <- spread(matchStats, playerAssignment, sum)
tidyMatchStats[is.na(tidyMatchStats)] <- 0
## Test for count EINGEWECHSELT == AUSGEWECHSELT
## It is possible, that EINGEWECHSELT > AUSGEWECHSELT. In this case one player
## got in and out in one match
tidyMatchStats <- mutate(tidyMatchStats, valid = AUSGEWECHSELT == EINGEWECHSELT)
filter(tidyMatchStats, !valid)

## Prices
### 
naStats <- filter(data$stats, is.na(fitPriceDate), playerAssignment %in% c('DURCHGESPIELT', 'EINGEWECHSELT', 'AUSGEWECHSELT'))
naStatsSeasonAgr <- summarise(group_by(naStats, season), sum = n())
mean(naStats$fitPrice)

head(naStats)
unique(naStats$playerId)


## Validate a sample
select(sample_n(stats, 10), matchId, season, matchday, goalsHome, goalsVisitors,
       matchtime, fitPrice, fitPriceDate)
sample_n(filter(stats, is.na(fitPriceDate)), 10)

## Odds
describe(data$odds)
exploreOdds <- mutate(data$odds, bookyProbSum = 1 - (HomeVictory + 
                                                         VisitorsVictory + Draw))
describe(exploreOdds$bookyProbSum)
# TODO bookyProbSum aggregated over season