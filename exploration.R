##########   Exploration of the raw DB data and the fit price assignments    ##########
source(file = 'loadData.R', echo = FALSE, encoding = 'UTF-8')
data <- loadData('BL1')
stats <- data$playerStats
dim(stats)

# Contained match days
library(plyr)
ddply(stats, c('matchday', 'season'), summarise, matchCount = length(unique(matchId)),
      statsCount = length(matchId))


# Trans Position
table(stats$transPos, useNA = 'always')
# Player Assignment
table(stats$playerAssignment, useNA = 'always')

# Exloration of grades and fitted prices
library(psych)
describe(subset(stats, select = c(kickerGrade, transGrade, fitPrice)))

# Group by players
playerStats <- ddply(stats, c('playerId'), summarize, meanPrice = mean(fitPrice))
describe(playerStats$meanPrice)

# Group by Trans Position
posStats <- ddply(stats, c('playerId', 'transPos'), summarize, 
                  meanPrice = mean(fitPrice, na.rm = TRUE))
posStats <- subset(posStats, !is.na(transPos) & !is.nan(meanPrice))
library(ggplot2)
ggplot(posStats, aes(x = transPos, y = meanPrice, fill = transPos)) + geom_boxplot()

# Group by Player Assignment
assignmentStats <- ddply(stats, c('playerId', 'playerAssignment'), summarize, 
                         meanPrice = mean(fitPrice, na.rm = TRUE), 
                         meanGrade = mean(kickerGrade, na.rm = TRUE))
priceStats <- subset(assignmentStats, !is.nan(meanPrice))

ggplot(priceStats, aes(x = playerAssignment, y = meanPrice, fill = playerAssignment)) + 
    geom_boxplot()

gradeStats <- subset(assignmentStats, !is.nan(meanGrade))
ggplot(gradeStats, aes(x = playerAssignment, y = meanGrade, fill = playerAssignment)) + 
    geom_boxplot()




ggplot(subset(stats, !is.na(kickerGrade)), aes(x = home, y = kickerGrade, fill = home)) +
    geom_boxplot() +
    scale_y_continuous(breaks = seq(2.5, 4.5, 0.5)) +
    facet_wrap(~ transPos)

##########  Exploration of the adjusted grade calculation   ##########
source(file = 'adjGradeModel.R', echo = FALSE, encoding = 'UTF-8')
enrichedStats <- enrichAdjGrade(stats)
library(plyr)
# Merges adjusted grade in stats
mergedStats <- merge(stats, subset(enrichedStats, select = c(matchId, playerId, adjGrade)),
                     by = c('matchId', 'playerId'),  all.x = TRUE)

subset(mergedStats, !is.na(kickerGrade) & is.na(adjGrade))
describe(mergedStats$adjGrade)
ggplot(mergedStats, aes(x = 'adjGrade', y = adjGrade)) + 
    geom_boxplot()


##########  Exploration of the player form data     ##########
source(file = 'formModel.R', echo = FALSE, encoding = 'UTF-8')
formEnrichedPlayerStats <- enrichForm(mergedStats, 'beta',  
                                      maxMatchdays = 8, minMatchdays = 3)
#formEnrichedPlayerStats <- enrichForm(mergedStats, 'arima', 0, -0.1, 
#                                      minMatchdays = 5)

print(describe(formEnrichedPlayerStats$playerForm))

subset(formEnrichedPlayerStats, playerForm > 3)
bsp <- subset(formEnrichedPlayerStats, playerId == 438 & season =='2011-2012' & matchday <= 15)
bsp <- bsp[order(bsp$matchday), ]



########### simpleResultFeatureExtraction   #################
source(file = 'simpleResultFeatureExtraction.R', echo = FALSE, encoding = 'UTF-8')
matches <- simpleMatchFeatureExtract(formEnrichedPlayerStats)
describe(subset(matches, select = c(homePrice, visitorsPrice, homeForm, visitorsForm)))
summary(subset(matches, select = c(homePrice, visitorsPrice, homeForm, visitorsForm)))

library(ggplot2)
qplot(formDiff, goalDiff, data = ergModelData, geom = c("point", "smooth"))

ggplot(ergModelData, aes(x = formDiff, y = goalDiff)) +
    geom_point()

##########  simpleResultModel   #############################
source(file = 'simpleResultModelFit.R', echo = FALSE, encoding = 'UTF-8')



##########  Exploration of the Match Result Features    ##########
source(file = 'matchResultFeatureExtraction.R', echo = FALSE, encoding = 'UTF-8')
modelMatches <- extractMatchResultFeatures(formEnrichedPlayerStats)
describe(modelMatches)


##########  Exploration of the Match Result Model   ##########
source(file = 'matchResultModel.R', echo = FALSE, encoding = 'UTF-8')
head(modelMatches)
dim(modelMatches)



##############
#spieleToBet <- spieleToBet[, -(10:41)]
#dim(spieleToBet)
#summary(spieleToBet[, 10:41])
#sum(apply(spieleToBet[spieleToBet$spieltag != 1, 10:29],1,function(x)!any(is.na(x)))) / 
#    nrow(spieleToBet[spieleToBet$spieltag != 1,])
#spieleToBet[apply(spieleToBet[, 10:29],1,function(x)!any(is.na(x))), 10:29]
#allNotNa <- spieleToBet[sum(is.na(spieleToBet[,10:41]))]
#sum(is.na(spieleToBet[, 10:41])) == 0
#spieleToBet[1, 10:41]
##############

#attach(spielerStats)
#set <- subset(spielerStats, spielerId == 474)
#set <- set[order(set$saison, set$spieltag), ]
#set
#naName <- relSpielerStats[is.na(transName) & spieltag == 2,]
#paste(naName$spielerId, ',', sep = '')

# Spieltage für eine Saison in der der Trans Parser noch nicht gelaufen ist
#unique(subset(spielerStats, is.na(transPos) & saison == '2013-2014' & einsatz == 'DURCHGESPIELT')$spieltag)

#table(kickerPosition, useNA = 'always')
#table(transPos, useNA = 'always')
# Warum tauchen NAs in transPos auf???
# Eigentlich nur im Einsatz BENCH!!

#table(einsatz, useNA = 'always')

#sum(is.na(transName)) / length(transName)

#library(psych)
#describe(subset(spielerStats, select = c(kickerNote, transNote, fitPreis)))

#detach(spielerStats)

## Explore NAs

# Spieler ohne Preis
#unique(subset(spielerStats, is.na(fitPrice), c(spielerId))$spielerId)


# Sonstige Explorationen
#summary(naomitSpielerStats)
#str(naomitSpielerStats)

#library(ggplot2)
#ggplot(naomitSpielerStats, aes(x = heim, y = kickerNote, fill = heim)) +
#    geom_boxplot() +
#    scale_y_continuous(breaks = seq(2.5, 4.5, 0.5)) +
#    facet_wrap(~ transPos)

#library(plyr)
#groupedStats <- ddply(naomitSpielerStats, c('transPos', 'heim'), 
#                      summarise, meanNote = mean(kickerNote), sdNote = sd(kickerNote),
#                      meanPreis = mean(fitPrice), sdPreis = sd(fitPrice), N = length(fitPrice))
#head(groupedStats)

# Fitpreis - NOte
#ggplot(naomitSpielerStats, aes(x = fitPrice, y = kickerNote)) +
#    geom_point()
s