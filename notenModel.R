# Laden Pos Preis Gewichtung Matrix
posPreisGew <- read.table('gegnerPreisGewichtung.csv', sep = ',', quote = '', skip = 1)
posPreisGew[, 1] <- NULL
rownames(posPreisGew) <- levels(naomitSpielerStats$transPos)
colnames(posPreisGew) <- levels(naomitSpielerStats$transPos)

getGegnerPreis <- function(spielerId, sId, h, transPos) {
    relStats <- subset(naomitSpielerStats, spielId == sId & heim != h, c(spielerId, transPos, fitPrice))
    gewSum <- 0
    preisSum <- 0
    for(i in 1:nrow(relStats)) {
        row <- relStats[i,]
        gew <- posPreisGew[transPos, row$transPos]
        gewSum <- gewSum + gew
        preisSum <- preisSum + gew * row$fitPrice
    }
    preisSum / gewSum
}

naomitSpielerStats <- cbind(naomitSpielerStats, gegnerPreis = mapply(
    getGegnerPreis, naomitSpielerStats$spielerId, naomitSpielerStats$spielId, 
    naomitSpielerStats$heim, naomitSpielerStats$transPos))

fit1 <- lm(kickerNote ~ fitPrice + gegnerPreis + heim, data = naomitSpielerStats)
summary(fit1)
fit2 <- lm(kickerNote ~ fitPrice + gegnerPreis + heim + transPos, data = naomitSpielerStats)
#anova(fit1, fit2)
summary(fit2)
#plot(fit2)

# fit2 scheint gut zu sein

naomitSpielerStats$predKickerNote <- predict(fit2, newdata = naomitSpielerStats)
naomitSpielerStats$modNote <- naomitSpielerStats$predKickerNote - naomitSpielerStats$kickerNote
