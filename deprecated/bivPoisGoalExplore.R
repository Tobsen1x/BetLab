describe(select(chancesEnrMatches, goalsHome, goalsVisitors, 
                homeExpGoals, visitorsExpGoals))

enrMatches %>% ggplot(aes(y = goalDiff, x = (homeExpGoals - visitorsExpGoals))) +
    geom_smooth() +
    coord_fixed(ratio = 1, xlim = c(-3, 3), ylim = c(-3, 3)) +
    scale_x_continuous(name = 'Predicted') +
    scale_y_continuous(name = 'Observed') +
    geom_abline(intercept = 0, slope = 1, size = 0.01, linetype = 'dashed')

# Residuals depending on matchday
enrMatches %>% ggplot(aes(y = abs(goalDiff - (homeExpGoals - visitorsExpGoals)), x = matchday)) +
    geom_smooth() +
    scale_x_continuous(name = 'Matchday') +
    scale_y_continuous(name = 'abs(residual)')