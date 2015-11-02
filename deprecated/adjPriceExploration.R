### Exploration
featurePlot(x = select(filteredAdjPriceData, startPrice, avgForm, age), 
            y = filteredAdjPriceData$endPrice,
            between = list(x = 1, y =  1),
            type = c('g', 'p', 'smooth'))

summary(adjPriceModels$lm$finalModel)
adjPriceModels$gbm$finalModel
plot(adjPriceModels$lm$finalModel)

describe(exampleObs)

firstObs <- filter(exampleObs, startPrice == 1000000, avgForm == 2, position == 1)
ggplot(firstObs, aes(x = age, y = lm)) +
    geom_smooth()

ggplot(modPrices, aes(x = age, y = priceLambda)) +
    geom_smooth()
