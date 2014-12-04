source('notenModel.R', encoding = 'UTF-8')

library(plyr)
relSpielerStats <- merge(relSpielerStats, 
              subset(withNoteSpielerStats, select = c(spielId, spielerId, 
                                                      modNote)),
              by = c('spielId', 'spielerId'),  all.x = TRUE)

relSpielerStats$gegnerPreis[is.nan(relSpielerStats$gegnerPreis)] <- NA
#relSpielerStats[relSpielerStats$spielId == 1, c('kickerFormation', 'transFormation',
#                                                'heim')]
spieleToBet <- ddply(relSpielerStats, c('spielId', 'liga', 'saison', 'spieltag', 'spielZeit',
                         'heimManId', 'auswManId', 'toreHeim', 'toreAusw'), c('nrow') )
# Column not necessary
spieleToBet$nrow <- NULL

calcBetaForm <- function(spId, sai, st, maxPastSpieltage, notAufgestelltModNote = -0.5,
                         noModNote = -0.1) {
    if(st <= maxPastSpieltage) {
        maxPastSpieltage <- st - 1
    }
    relEinsaetze <- subset(relSpielerStats, spielerId == spId & saison == sai &
                                    spieltag < st & spieltag > st - (maxPastSpieltage + 1))
   
    if(st == 1 | maxPastSpieltage <= 1) {
        return(NA)
    }
    
    
    formVector <- c()
    gewVector <- c()
    for(pastSpieltag in seq(1:maxPastSpieltage)) {
        pastSpiel <- relEinsaetze[relEinsaetze$spieltag == (st - pastSpieltag), ]
        
        gew <- 1 - punif(q = pastSpieltag, 1, maxPastSpieltage)
        
        # Spieler war in diesem Spiel nicht aufgestellt
        if(nrow(pastSpiel) == 0) {
            form <- notAufgestelltModNote
        } else {
            # Spieler hat in diesem Spiel keine ModNote
            if(is.na(pastSpiel$modNote)) {
                form <- noModNote
            } else {
                form <- pastSpiel$modNote
            }
            
        }
        
        # print(paste(paste('gew:', gew), paste('form:', form)))
        formVector <- c(formVector, form)
        gewVector <- c(gewVector, gew)
    }
    
    zaehler <- sum(formVector * gewVector)
    nenner <- sum(gewVector)
    zaehler / nenner
}

relSpielerStats$preBeta5Form <- NA
for(i in seq(1:nrow(spieleToBet))) {
    row <- spieleToBet[i, ]
    relSpieler <- relSpielerStats[relSpielerStats$spielId == row$spielId, ]
    for(j in seq(1:nrow(relSpieler))) {
        spieler <- relSpieler[j, ]
        form <- calcBetaForm(spieler$spielerId, spieler$saison, spieler$spieltag, 5)
        relSpielerStats$preBeta5Form[relSpielerStats$spielerId == spieler$spielerId &
                                     relSpielerStats$spielId == row$spielId] <- form
    }
}

summary(relSpielerStats$preBeta5Form)

rm(row)
rm(relSpieler)
rm(i)
rm(j)
rm(form)
rm(spieler)