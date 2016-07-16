featuredMatches <- readRDS('data/featuredMatches/HOMEVISITORS_BL12015-2016_34_222_SBI-0.05_SNPI-0.2_PD100_LFI0.Rds')
modelInput <- reduceFeatures(featuredMatches)

data <- loadTrainingData(toMatchday = 34, seasons = getAllSeasons('2005-2006', '2015-2016'), 
                         leagues = 'BL1', fitPriceImpute = 50000)
odds <- data$odds
stats <- data$stats
matches <- data$matches
# Add booky odds
modelInput <- merge(select(odds, matchId, HomeOdd = HomeVictory, VisitorsOdd = VisitorsVictory, 
                           DrawOdd = Draw), modelInput, by = c('matchId'), all.y = TRUE, sort = FALSE)

customCvContr <- trainControl(method = 'cv' , number = 10)
tuneGrid <- expand.grid(nrounds = c(100,200,300,400),
                    max_depth = c(3),
                    eta = 0.1,
                    gamma = 0,
                    colsample_bytree = 0.8,
                    min_child_weight = 1,
                    lambda = 1,
                    alpha = 0,
                    subsample = 1,
                    scale_pos_weight = 1)


# Just form features
formTuningResult <- tuneModelWrapped(modelInput = modelInput, trControl = customCvContr, 
                                 tuneGrid = tuneGrid, seed = 16450, featuresToInclude = 'form')
describe(formTuningResult)

# Just price features
priceTuningResult <- tuneModelWrapped(modelInput = modelInput, trControl = customCvContr, 
                                     tuneGrid = tuneGrid, seed = 16450, featuresToInclude = 'price')
describe(priceTuningResult)
plotTuningProfile(priceTuningResult)

# Price and form features
allTuningResult <- tuneModelWrapped(modelInput = modelInput, trControl = customCvContr, 
                                     tuneGrid = tuneGrid, seed = 16450, featuresToInclude = 'all')
