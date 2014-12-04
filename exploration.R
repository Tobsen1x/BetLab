attach(spielerStats)

set <- subset(spielerStats, spielerId == 474)
set <- set[order(set$saison, set$spieltag), ]
set
naName <- relSpielerStats[is.na(transName) & spieltag == 2,]
paste(naName$spielerId, ',', sep = '')

# Spieltage für eine Saison in der der Trans Parser noch nicht gelaufen ist
unique(subset(spielerStats, is.na(transPos) & saison == '2013-2014' & einsatz == 'DURCHGESPIELT')$spieltag)

table(kickerPosition, useNA = 'always')
table(transPos, useNA = 'always')
# Warum tauchen NAs in transPos auf???
# Eigentlich nur im Einsatz BENCH!!

table(einsatz, useNA = 'always')

sum(is.na(transName)) / length(transName)


library(psych)
describe(subset(spielerStats, select = c(kickerNote, transNote, fitPreis)))

detach(spielerStats)

## Explore NAs

# Spieler ohne Preis
unique(subset(spielerStats, is.na(fitPrice), c(spielerId))$spielerId)


# Sonstige Explorationen
summary(naomitSpielerStats)
str(naomitSpielerStats)

library(ggplot2)
ggplot(naomitSpielerStats, aes(x = heim, y = kickerNote, fill = heim)) +
    geom_boxplot() +
    scale_y_continuous(breaks = seq(2.5, 4.5, 0.5)) +
    facet_wrap(~ transPos)

library(plyr)
groupedStats <- ddply(naomitSpielerStats, c('transPos', 'heim'), 
                      summarise, meanNote = mean(kickerNote), sdNote = sd(kickerNote),
                      meanPreis = mean(fitPrice), sdPreis = sd(fitPrice), N = length(fitPrice))
head(groupedStats)

# Fitpreis - NOte
ggplot(naomitSpielerStats, aes(x = fitPrice, y = kickerNote)) +
    geom_point()
