source('getData.R', encoding = 'UTF-8')

#############   FEATURE EXTRACTION      ######################

# Laden Pos Preis Gewichtung Matrix
posPreisGew <- read.table('gegnerPreisGewichtung.csv', sep = ';', 
                          quote = '', skip = 1)
posPreisGew[, 1] <- NULL
rownames(posPreisGew) <- levels(relSpielerStats$transPos)
colnames(posPreisGew) <- levels(relSpielerStats$transPos)

# Calculates the weighted price of the enemies playing against
getGegnerPreis <- function(spielerId, sId, h, transPos) {
    relStats <- subset(relSpielerStats, spielId == sId & heim != h, 
                       c(spielerId, transPos, fitPreis))
    gewSum <- 0
    preisSum <- 0
    for(i in 1:nrow(relStats)) {
        row <- relStats[i,]
        #Gegner nur relevant wenn transpos gesetzt ist
        if(!is.na(transPos) & !is.na(row$transPos)) {
            gew <- posPreisGew[transPos, row$transPos]
            gewSum <- gewSum + gew
            preisSum <- preisSum + gew * row$fitPreis
        }
    }
    preisSum / gewSum
}

summary(relSpielerStats$transPos)

relSpielerStats <- cbind(relSpielerStats, gegnerPreis = mapply(
    getGegnerPreis, relSpielerStats$spielerId, relSpielerStats$spielId, 
    relSpielerStats$heim, relSpielerStats$transPos))

withPositionSpielerStats <- subset(relSpielerStats, !is.na(transPos))

library(caret)
withPositionSpielerStats$preisDiff <- withPositionSpielerStats$fitPreis - 
    withPositionSpielerStats$gegnerPreis
withPositionSpielerStats$logPreisVerh <- log(withPositionSpielerStats$fitPreis / 
    withPositionSpielerStats$gegnerPreis)

x <- subset(withPositionSpielerStats, select = c(fitPreis, gegnerPreis, preisDiff, logPreisVerh, heim, transPos))
y <- subset(withPositionSpielerStats, select = c(kickerNote))

corMatrix <- cor(x[, -c(5, 6)])
corMatrix

highlyCorrelated <- findCorrelation(corMatrix, cutoff=0.5)
highlyCorrelated

# At least one of preisDiff and logPreisVerh should be removed
set.seed(1)
repCVControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
cvControl <- trainControl(method = 'cv', number = 5)

withNoteSpielerStats <- subset(withPositionSpielerStats, !is.na(kickerNote))

################    LM MODEL    ##################################

lmFit <- train(kickerNote ~ fitPreis + gegnerPreis + heim + transPos,
               data = withNoteSpielerStats, method = 'lm', trControl = repCVControl,
               preProcess = c('center', 'scale'))
lmFit
summary(lmFit)

lmImportance <- varImp(lmFit, scale = FALSE)
lmImportance

########################    RANDOM FOREST MODEL     ###################################

print(paste('Random Forest ModNoten Model Start Time:', Sys.time()))
rfFit <- train(kickerNote ~ fitPreis + gegnerPreis + heim + transPos + preisDiff + logPreisVerh,
               data = withNoteSpielerStats, method = 'rf', 
               trControl = cvControl, tuneGrid = data.frame(mtry = 3), importance = TRUE)
print(paste('End Time:', Sys.time()))
rfFit

rfImportance <- varImp(rfFit, scale = FALSE)
rfImportance

withNoteSpielerStats$lmPredKickerNote <- predict(lmFit, newdata = withNoteSpielerStats)
withNoteSpielerStats$rfPredKickerNote <- predict(rfFit, newdata = withNoteSpielerStats)

summary(withNoteSpielerStats$lmPredKickerNote)
summary(withNoteSpielerStats$rfPredKickerNote)

############################    COMBINED RANDOM FOREST MODEL    ######################

print(paste('Random Forest ModNoten Combined Model Start Time:', Sys.time()))
combFit <- train(kickerNote ~ lmPredKickerNote + rfPredKickerNote, data = withNoteSpielerStats,
                 method = 'rf', trControl = cvControl, tuneGrid = data.frame(mtry = 3), importance = TRUE)
print(paste('End Time:', Sys.time()))
print(combFit)

combImportance <- varImp(combFit, scale = FALSE)
combImportance

withNoteSpielerStats$combPredKickerNote <- predict(combFit, newdata = withNoteSpielerStats)
withNoteSpielerStats$modNote <- withNoteSpielerStats$combPredKickerNote - withNoteSpielerStats$kickerNote

summary(withNoteSpielerStats$combPredKickerNote)
summary(withNoteSpielerStats$modNote)

rm(x)
rm(y)
rm(corMatrix)
rm(highlyCorrelated)


