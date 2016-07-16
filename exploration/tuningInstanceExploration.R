results <- loadAllTuningResults()
instance <- cbind(results$metrics, results$featureConfigs, results$xgbConfigs)
describe(select(instance, profitPerc, accuracy, valueDiffPerc))

ordMetrics <- arrange(instance, desc(profitPerc))
head(ordMetrics)
