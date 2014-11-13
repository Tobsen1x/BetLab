# Test fit price
stopifnot(getFitPrice(1, as.Date("2011-09-19", "%Y-%m-%d")) == 28000000)

# Test fit price enrichment
stopifnot(subset(spielerStats, spielerId == 1 & spielId == 1)$fitPrice == 30000000)
stopifnot(subset(spielerStats, spielerId == 108 & spielId == 306)$fitPrice == 5500000)

# Test getGegnerPreis
gegnerPreis1 <- getGegnerPreis(1, 1, TRUE, transPos = 'Torwart')
stopifnot(gegnerPreis1 > 5000000 & gegnerPreis1 < 7000000)