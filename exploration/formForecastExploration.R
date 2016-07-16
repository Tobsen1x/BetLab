data <- readRDS(file="data/BL1_2005-2015.Rds") 
stats <- data$stats
matches <- data$matches

### Execute form forecast ###
# Calculate form forecasts TODO Adjust!!
#source(file = 'featureEngineering/playerFormForecast.R', echo = FALSE, encoding = 'UTF-8')
#formEnrichedStats <- fillAllPlayerForm(stats, matches, version = (2, 2, 2))
#describe(select(formEnrichedStats, formForecast, purgedGrade))
#saveRDS(formEnrichedStats, file="data/formEnriched/111.Rds")
###

formEnrichedStats <- readRDS(file="data/formEnriched/BL12015-2016_34_122_SBI-0.1_SNPI-0.2_PD60_LFI-0.4.Rds")
describe(select(formEnrichedStats, formForecast, purgedGrade))

lastImputeStats <- filter(formEnrichedStats, !is.na(purgedGrade), formForecast == -0.4)
# Percent of Stats imputed by lastFormImpute
nrow(lastImputeStats) / nrow(filter(formEnrichedStats, !is.na(purgedGrade)))
# mean of purgedGrade
mean(lastImputeStats$purgedGrade)

benchPlayedStats <- filter(formEnrichedStats, !is.na(purgedGrade), formForecast == -0.1, playerAssignment == 'EINGEWECHSELT')
# Percent of Stats imputed by staticBenchImpute
as.numeric(nrow(benchPlayedStats)) / as.numeric(nrow(filter(formEnrichedStats, !is.na(purgedGrade))))
# mean of purgedGrade
mean(benchPlayedStats$purgedGrade)


### Explore differences in formForecast and grade
naFilteredStats <- filter(formEnrichedStats, !is.na(formForecast), !is.na(purgedGrade))
differenceStats <- mutate(naFilteredStats, forecastDiff = purgedGrade - formForecast,
                          forecastDiffAbs = abs(purgedGrade - formForecast))
describe(select(differenceStats, forecastDiff, forecastDiffAbs))

vioplot(differenceStats$forecastDiff, names = 'Forecast Diff', col = 'green')
title('Violin Plot for difference in formForecast and purgedGrade')
vioplot(differenceStats$forecastDiffAbs, names = 'Forecast Diff (abs.)', col = 'green')
title('Violin Plot for the absolute value of the difference in formForecast and purgedGrade')

# correlation
cor(naFilteredStats$formForecast, naFilteredStats$purgedGrade)

seed <- 16450
cvContr <- trainControl(method = 'cv', number = 10)
gradeFormula <- as.formula('purgedGrade ~ formForecast')
lmModel <- train(form = gradeFormula, data = naFilteredStats, method = 'lm',
                 trControl = cvContr)
lmModel