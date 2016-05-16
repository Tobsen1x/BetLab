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

formEnrichedStats <- readRDS(file="data/formEnriched/222_SBI-0.25_SNPI-0.4_PD60")
describe(select(formEnrichedStats, formForecast, purgedGrade))

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