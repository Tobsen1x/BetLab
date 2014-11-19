# Predict modNote with timeseries of past modNoten
library(forecast)

#sId <- 62
#gId <- 257
#imputeBy <- -0.1
#pastGames <- 4
#head(subset(naomitSpielerStats, spieltag == 30))

naomitSpielerStats <- naomitSpielerStats[
    order(naomitSpielerStats$saison, naomitSpielerStats$spieltag, 
          naomitSpielerStats$spielId), ]

predModNote <- function(sId, gId, imputeBy = -0.1, pastGames = 4) {
    toPredict <- subset(naomitSpielerStats, subset = (spielId == gId & spielerId == sId), 
                        select = c(liga, saison, spieltag, spielerId))
    pastPerformances <- subset(naomitSpielerStats, 
                               subset = (liga == toPredict$liga & spielerId == toPredict$spielerId & 
                                             saison == toPredict$saison & spieltag < toPredict$spieltag),
                               select = c(spieltag, modNote))
    
    if(nrow(pastPerformances) < pastGames) {
        return(c(NA, NA))
    }
    
    spieltage <- 1:max(pastPerformances$spieltag)
    toadd <- spieltage %in% pastPerformances$spieltag
    for(i in spieltage) {
        if(!toadd[i]) {
            pastPerformances <- rbind(pastPerformances, c(i, imputeBy))
        }
    }
    pastPerformances <- pastPerformances[order(pastPerformances$spieltag, decreasing = TRUE), ]
    pastPerformances <- head(pastPerformances, pastGames)
    pastPerformances <- pastPerformances[order(pastPerformances$spieltag), ]
    timeSeries <- ts(pastPerformances$modNote)
    
    #plot.ts(timeSeries)
    #ggplot(pastPerformances[!is.na(pastPerformances$modNote), ], aes(x = spieltag, y = modNote)) +
    #    geom_line()
    
    forecastWithBeta <- try(HoltWinters(timeSeries, gamma = FALSE, l.start = pastPerformances[1, ]$spieltag))
    forecastWOBeta <- HoltWinters(timeSeries, beta = FALSE, gamma = FALSE, l.start = pastPerformances[1, ]$spieltag)
    
    resultWithBeta <- NA
    if(class(forecastWithBeta) != 'try-error') {
        predWithBeta <- forecast.HoltWinters(forecastWithBeta, h = 1)
        resultWithBeta <- predWithBeta$mean[1]
    }
    predWOBeta <- forecast.HoltWinters(forecastWOBeta, h = 1)
    
    c(predWOBeta$mean[1], resultWithBeta)
}

naomitSpielerStats$predModNoteWOBeta <- NA
naomitSpielerStats$predModNoteWithBeta <- NA

for(i in 1:nrow(naomitSpielerStats)) {
    row <- naomitSpielerStats[i, ]
    print(paste(paste(paste('Calculate sId =', row$spielerId), 'gId ='), 
                row$spielId))
    preds <- predModNote(row$spielerId, row$spielId)
    naomitSpielerStats[i, ]$predModNoteWOBeta <- preds[1]
    naomitSpielerStats[i, ]$predModNoteWithBeta <- preds[2]
}

predModNotenWOBeta <- subset(naomitSpielerStats, !is.na(predModNoteWOBeta))
predModNotenWithBeta <- subset(naomitSpielerStats, !is.na(predModNoteWithBeta))

mseTrivial <- sqrt(1 / nrow(naomitSpielerStats) * 
                       sum((naomitSpielerStats$modNote - 0) ^ 2))

mseWOBeta <- sqrt(1 / nrow(predModNotenWOBeta) *                     
                      sum((predModNotenWOBeta$modNote - 
                               predModNotenWOBeta$predModNoteWOBeta) ^ 2))

mseWithBeta <- sqrt(1 / nrow(predModNotenWithBeta) * 
                        sum((predModNotenWithBeta$modNote - 
                                 predModNotenWithBeta$predModNoteWithBeta) ^ 2))