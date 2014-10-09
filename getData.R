library(RMySQL)
con <- dbConnect(MySQL(), user="root", password="root",
                 dbname="soccerlabdata3.0")

## Reading SpielerSpielStatistiks
heimStartelfStatsQuery <- 'select sp.id as spielId, sp.liga, sp.saison, sp.spieltag, sp.spielZeit,
    sp.heimMan_id as heimManId, sp.auswMan_id as auswManId, sp.toreHeim,
    sp.toreAusw, auf.kickerFormation, auf.transFormation, spieler.id as spielerId,
    spieler.transName, spieler.kickerPosition, spielerStat.einsatz,
    spielerStat.kickerNote, spielerStat.transNote, spielerStat.transPos, \'1\' as heim
from spiel sp 
inner join aufstellung auf on sp.heimAuf_id = auf.id
inner join aufstellung_spieler aufSp on auf.id = aufSp.Aufstellung_id
inner join spieler spieler on aufSp.startElf_id = spieler.id 
inner join postspielerspielstatistik spielerStat on sp.id = spielerStat.spiel_id 
    and spieler.id = spielerStat.spieler_id'

heimBenchStatsQuery <- 'select sp.id as spielId, sp.liga, sp.saison, sp.spieltag, sp.spielZeit,
    sp.heimMan_id as heimManId, sp.auswMan_id as auswManId, sp.toreHeim,
    sp.toreAusw, auf.kickerFormation, auf.transFormation, spieler.id as spielerId,
    spieler.transName, spieler.kickerPosition, spielerStat.einsatz,
    spielerStat.kickerNote, spielerStat.transNote, spielerStat.transPos, \'1\' as heim
from spiel sp 
inner join aufstellung auf on sp.heimAuf_id = auf.id
inner join aufstellung_bench aufSp on auf.id = aufSp.Aufstellung_id
inner join spieler spieler on aufSp.bench_id = spieler.id 
inner join postspielerspielstatistik spielerStat on sp.id = spielerStat.spiel_id 
    and spieler.id = spielerStat.spieler_id'

auswStartelfStatsQuery <- 'select sp.id as spielId, sp.liga, sp.saison, sp.spieltag, sp.spielZeit,
    sp.heimMan_id as heimManId, sp.auswMan_id as auswManId, sp.toreHeim,
    sp.toreAusw, auf.kickerFormation, auf.transFormation, spieler.id as spielerId,
    spieler.transName, spieler.kickerPosition, spielerStat.einsatz,
    spielerStat.kickerNote, spielerStat.transNote, spielerStat.transPos, \'0\' as heim
from spiel sp 
inner join aufstellung auf on sp.auswAuf_id = auf.id
inner join aufstellung_spieler aufSp on auf.id = aufSp.Aufstellung_id
inner join spieler spieler on aufSp.startElf_id = spieler.id 
inner join postspielerspielstatistik spielerStat on sp.id = spielerStat.spiel_id 
    and spieler.id = spielerStat.spieler_id'

auswBenchStatsQuery <- 'select sp.id as spielId, sp.liga, sp.saison, sp.spieltag, sp.spielZeit,
    sp.heimMan_id as heimManId, sp.auswMan_id as auswManId, sp.toreHeim,
    sp.toreAusw, auf.kickerFormation, auf.transFormation, spieler.id as spielerId,
    spieler.transName, spieler.kickerPosition, spielerStat.einsatz,
    spielerStat.kickerNote, spielerStat.transNote, spielerStat.transPos, \'0\' as heim
from spiel sp 
inner join aufstellung auf on sp.auswAuf_id = auf.id
inner join aufstellung_bench aufSp on auf.id = aufSp.Aufstellung_id
inner join spieler spieler on aufSp.bench_id = spieler.id 
inner join postspielerspielstatistik spielerStat on sp.id = spielerStat.spiel_id 
    and spieler.id = spielerStat.spieler_id'

spielerStats <- dbGetQuery(con, heimStartelfStatsQuery)
spielerStats <- rbind(spielerStats, dbGetQuery(con, heimBenchStatsQuery))
spielerStats <- rbind(spielerStats, dbGetQuery(con, auswStartelfStatsQuery))
spielerStats <- rbind(spielerStats, dbGetQuery(con, auswBenchStatsQuery))

spielerStats <- transform(spielerStats, liga = as.factor(liga),
                          spielZeit = strptime(spielZeit, '%Y-%m-%d %H:%M:%S'),
                          kickerPosition = as.factor(kickerPosition),
                          einsatz = as.factor(einsatz),
                          transFormation = as.factor(transFormation),
                          transPos = as.factor(transPos),
                          heim = as.factor(heim))
summary(spielerStats)

## Reading Preise
preiseQuery <- 'select sp.id as spielerId, preis.informationDate, preis.preis from marktpreis preis
    inner join spieler_marktpreis spPreis on preis.id = spPreis.marktpreise_id
    inner join spieler sp on spPreis.Spieler_id = sp.id order by sp.id, preis.informationDate desc'

preise <- dbGetQuery(con, preiseQuery)
preise <- transform(preise, informationDate = as.Date(informationDate, format = '%Y-%m-%d'))
summary(preise)

dbDisconnect(con)

#-----------------------------------------------------------------

## Refining SpielerSpielStatistiks with Preise
getFitPrice <- function(spieler, spielZeit) {
    spielerPreise <- subset(preise, spielerId == spieler)
    # No price information for player
    if(nrow(spielerPreise) == 0) {
        print(paste('No price information for player with ID', spieler))
        return(NA)
    }
    for(i in seq_len(nrow(spielerPreise))) {
        preis <- spielerPreise[i, ]
        # spielerPreise is sorted, so the first date which is in past of the game is returned
        if(preis$informationDate - spielZeit <= 0) {
            return(preis$preis)
        }
    }
    # If no past price is found, the oldest is returned
    priceToReturn <- spielerPreise[nrow(spielerPreise), ]
    print(paste('No past price for player', spieler, 'and game time', spielZeit, 
                  'is found, so the oldest price is returned:', priceToReturn$preis))
    return(priceToReturn$preis)
}

spielerStats <- cbind(spielerStats, fitPrice = mapply(
    getFitPrice, spielerStats$spielerId, as.Date(spielerStats$spielZeit)))
summary(spielerStats)