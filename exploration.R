attach(spielerStats)

table(kickerPosition, useNA = 'always')
table(transPos, useNA = 'always')
# Warum tauchen NAs in transPos auf???

table(einsatz, useNA = 'always')

library(psych)
describe(subset(spielerStats, select = c(kickerNote, transNote, fitPrice)))

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
groupedStats <- ddply(naomitSpielerStats, c('transPos', 'heim', 'einsatz'), 
                      summarise, meanNote = mean(kickerNote), sdNote = sd(kickerNote),
                      meanPreis = mean(fitPrice), sdPreis = sd(fitPrice), N = length(fitPrice))
groupedStats
