### Execute load data from DB ###
source(file = 'dataProvider/loadData.R', echo = FALSE, encoding = 'UTF-8')
toMatchday <- 34
seasons <- c('2005-2006', '2006-2007', '2007-2008', '2008-2009', '2009-2010', 
             '2010-2011', '2011-2012', '2012-2013', '2013-2014', '2014-2015')
leagues <- c('BL1')
data <- loadTrainingData(toMatchday, seasons, leagues)
# Save it Digga
saveRDS(data, file="data/BL1_2005-2015.Rds")
###

data <- readRDS(file="data/BL1_2005-2015.Rds")

stats <- data$stats
matches <- data$matches

# Exploring match - player - stats
describe(select(data$stats, season, position, playerAssignment, fitPrice))

# TODO Move to test
# All positions have to be set
nrow(filter(data$stats, is.na(position)))

matchStats <- summarise(group_by(stats, matchId, home, playerAssignment), sum = n())
tidyMatchStats <- spread(matchStats, playerAssignment, sum)
tidyMatchStats[is.na(tidyMatchStats)] <- 0
## Test for count EINGEWECHSELT == AUSGEWECHSELT
## It is possible, that EINGEWECHSELT > AUSGEWECHSELT. In this case one player
## got in and out in one match
tidyMatchStats <- mutate(tidyMatchStats, valid = AUSGEWECHSELT == EINGEWECHSELT)
head(filter(tidyMatchStats, !valid))

## Prices
### Stats without prices
naStats <- filter(data$stats, is.na(fitPriceDate), playerAssignment %in% c('DURCHGESPIELT', 'EINGEWECHSELT', 'AUSGEWECHSELT'))
naStatsSeasonAgr <- summarise(group_by(naStats, season), sum = n())

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


### Exploring kickerGrade
describe(stats$grade)
naRmGrade <- stats$grade[!is.na(stats$grade)]
vioplot(naRmGrade, names = 'Grade', col = 'yellow')
title('Violin Plot of Grades')
summary(stats$grade)

#Expand stats with team Ids
mergeMatches <- select(matches, matchId, homeTeamId, visitorsTeamId)
mergedStats <- merge(stats, mergeMatches)
mergedStats <- filter(mergedStats, !is.na(grade))
mergedStats <- mutate(mergedStats, teamId = ifelse(home, homeTeamId, visitorsTeamId))
mergedStats <- select(mergedStats, -homeTeamId, -visitorsTeamId)
# Grouping positions
colNames <- levels(mergedStats$position)
positions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 'mid', 'off', 'off', 'off', 'off', 'off')
groupedPositions <- as.data.frame(t(positions))
colnames(groupedPositions) <- colNames
mergedStats$groupedPosition <- sapply(mergedStats$position, FUN = function(x) groupedPositions[1, x])
mergedStats <- mutate(mergedStats, teamId = as.factor(teamId))
describe(mergedStats$groupedPosition)
describe(mergedStats)

s1 <- group_by(mergedStats, season, teamId, groupedPosition)
s2 <- summarise(s1, meanGrade = mean(grade), sdGrade = sd(grade))

t1 <- filter(s2, season == '2014-2015', groupedPosition == 'off')
ggplot(data = t1, aes(x = teamId, y = meanGrade, fill = teamId)) + 
    geom_bar(stat = 'identity')

