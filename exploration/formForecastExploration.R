data <- readRDS(file="data/BL1_2005-2015.Rds")

stats <- data$stats
matches <- data$matches

### Execute form forecast ###
# Calculate form forecasts
source(file = 'featureEngineering/playerFormForecast.R', echo = FALSE, encoding = 'UTF-8')
formEnrichedStats <- fillAllPlayerForm(stats)
describe(select(formEnrichedStats, formForecast, grade))
# Save this shit
saveRDS(formEnrichedStats, file="data/formEnriched/111.Rds")
###

# Load it man
formEnrichedStats <- readRDS(file="data/formEnriched/111.Rds")

### Explore differences in formForecast and grade
naFilteredStats <- filter(formEnrichedStats, !is.na(formForecast), !is.na(grade))
differenceStats <- mutate(naFilteredStats, forecastDiff = grade - formForecast,
                          forecastDiffAbs = abs(grade - formForecast))
describe(select(differenceStats, forecastDiff, forecastDiffAbs))

vioplot(differenceStats$forecastDiff, names = 'Forecast Diff', col = 'green')
title('Violin Plot for difference in formForecast and grade')
vioplot(differenceStats$forecastDiffAbs, names = 'Forecast Diff (abs.)', col = 'green')
title('Violin Plot for the absolute value of the difference in formForecast and grade')

# correlation
cor(naFilteredStats$formForecast, naFilteredStats$grade)
featurePlot(x = naFilteredStats$formForecast, 
            y = naFilteredStats$grade, 
            plot = "scatter",
            type = c("p", "smooth"),
            span = .5)