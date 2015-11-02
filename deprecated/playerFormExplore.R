describe(select(formEnrichedPlayerStats, adjGrade, meanf, expSmoothing))
formExploreData <- formEnrichedPlayerStats %>% filter(
    !is.na(adjGrade), !is.na(meanf), !is.na(expSmoothing))
describe(select(formExploreData, adjGrade, meanf, 
                expSmoothing))
sample_n(formExploreData, size = 10)

means <- rep(mean(formExploreData$adjGrade), length(formExploreData$adjGrade))
accuracy(means, formExploreData$adjGrade)
accuracy(formExploreData$meanf, formExploreData$adjGrade)
accuracy(formExploreData$expSmoothing, formExploreData$adjGrade)