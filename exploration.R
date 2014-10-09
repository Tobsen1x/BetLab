naomitSpielerStats <- subset(spielerStats, !is.na(kickerNote) & !is.na(transPos) & !is.na(fitPrice))
naomitSpielerStats <- transform(naomitSpielerStats, spielZeit = as.POSIXct(spielZeit))
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
