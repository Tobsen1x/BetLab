source('formModel.R', encoding = 'UTF-8')

# Fill spieleToBet Data Set
colNames <- c('Torwart', 'Innenverteidiger', 'Linker Verteidiger', 'Rechter Verteidiger',
              'Defensives Mittelfeld', 'Zentrales Mittelfeld', 'Linkes Mittelfeld',
              'Rechtes Mittelfeld', 'Offensives Mittelfeld', 'Hängende Spitze', 
              'Mittelstürmer', 'Linksaußen', 'Rechtsaußen')
#relPositionen <- c('tw', 'iv', 'fv', 'fv', 'dm', 'mit', 'mit', 'mit', 'om', 'om', 'st', 'fs', 'fs')
relPositionen <- c('tw', 'iv', 'fv', 'fv', 'mit', 'mit', 'mit', 'mit', 'mit', 'mit', 'st', 'st', 'st')
groupedPositions <- as.data.frame(t(relPositionen))
colnames(groupedPositions) <- colNames
groupedPositions

getFeatureName <- function(pos, preis = TRUE, heim = TRUE) {
    if(preis) {
        if(heim) {
            return(paste(pos, 'Preis', 'Heim', sep = ''))
        } else {
            return(paste(pos, 'Preis', 'Ausw', sep = ''))
        }
    } else {
        if(heim) {
            return(paste(pos, 'Form', 'Heim', sep = ''))
        } else {
            return(paste(pos, 'Form', 'Ausw', sep = ''))
        }
    }
}

featureNames <- sapply(unique(relPositionen), getFeatureName)
featureNames <- c(featureNames, sapply(unique(relPositionen), getFeatureName, preis = FALSE))
featureNames <- c(featureNames, sapply(unique(relPositionen), getFeatureName, heim = FALSE))
featureNames <- c(featureNames, sapply(unique(relPositionen), getFeatureName, heim = FALSE, preis = FALSE))
featureNames

extractFeature <- function(feaStats, isPr) {
    if(isPr) {
        return(mean(feaStats$fitPreis))
    } else {
        return(mean(feaStats$preBeta5Form))
    }
}

calcFeatureValue <- function(feature, spId) {
    
    isHome <- grepl('Heim', feature)
    isPreis <- grepl('Preis', feature)
    
    relPos <- character()
    if(isPreis) {
        relPos <- strsplit(feature, 'Preis')[[1]][1]
    } else {
        relPos <- strsplit(feature, 'Form')[[1]][1]
    }
    
    # Extracting relevant Positions
    relPps <- t(groupedPositions[1, ])[, 1] == relPos 
    relTransPositionen <- names(relPps[relPps == TRUE])
    
    # Subsetting to relevant SpielerStats
    featureStats <- subset(relSpielerStats, spielId == spId & heim == isHome & 
                               transPos %in% relTransPositionen)
    extractFeature(featureStats, isPreis)
    
}


for(j in seq(1:length(featureNames))) {
    featureName <- featureNames[j]
    spieleToBet$tmp <- NA
    
    for(i in seq(1:nrow(spieleToBet))) {
        aktSpiel <- spieleToBet[i, ]
        
        featureValue <- calcFeatureValue(featureName, aktSpiel$spielId)
        #print(paste('Feature:', featureName, '| Value:', featureValue))
        spieleToBet[spieleToBet$spielId == aktSpiel$spielId, 'tmp'] <- 
            featureValue
    }
    
    spieleToBet$tmp[is.nan(spieleToBet$tmp)] <- NA
    spieleToBet <- rename(spieleToBet, replace = setNames(featureName, 'tmp'))
}

##############
#spieleToBet <- spieleToBet[, -(10:41)]
#dim(spieleToBet)
#summary(spieleToBet[, 10:41])
#sum(apply(spieleToBet[spieleToBet$spieltag != 1, 10:29],1,function(x)!any(is.na(x)))) / 
#    nrow(spieleToBet[spieleToBet$spieltag != 1,])
#sum(is.na(spieleToBet[,10:41]))
#allNotNa <- spieleToBet[sum(is.na(spieleToBet[,10:41]))]
#sum(is.na(spieleToBet[, 10:41])) == 0
#spieleToBet[1, 10:41]
##############

rm(aktSpiel)
rm(groupedPositions)
rm(colNames)
rm(featureName)
rm(featureNames)
rm(featureValue)
rm(i)
rm(j)
rm(relPositionen)