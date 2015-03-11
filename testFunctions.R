# Test fit price
stopifnot(getFitPrice(1, as.Date("2011-09-19", "%Y-%m-%d")) == 28000000)

# Test fit price enrichment
stopifnot(subset(spielerStats, spielerId == 1 & spielId == 1)$fitPrice == 30000000)
stopifnot(subset(spielerStats, spielerId == 108 & spielId == 306)$fitPrice == 5500000)

# Test getGegnerPreis
gegnerPreis1 <- getGegnerPreis(1, 1, TRUE, transPos = 'Torwart')
stopifnot(gegnerPreis1 > 5000000 & gegnerPreis1 < 7000000)

# Test Beta5Form Calculation
neuerForm <- calcBetaForm(spId = 1, sai = '2013-2014', st = 23, maxPastSpieltage = 5)

# Test teamForm
enrichedMatches <- enrichTeamPricePlace(stats, matches)
homeAdvantage <- 3
formDevEnrichedMatches <- enrichFormDev(enrichedMatches, homeAdvantage)
minMatchdays <- 3
maxMatchdays <- 8
teamFormEnrichedMatches <- enrichTeamForm(formDevEnrichedMatches,
                                          minMatchdays, maxMatchdays)

mId <- 1567
row <- filter(teamFormEnrichedMatches, matchId == mId)
homeTeamForm <- calcTeamForm(formDevEnrichedMatches, row$homeTeamId, 
                             row$season, row$matchday, minMatchdays, 
                             maxMatchdays)
visitorsTeamForm <- calcTeamForm(formDevEnrichedMatches, row$visitorsTeamId, 
                                 row$season, row$matchday, minMatchdays, 
                                 maxMatchdays)