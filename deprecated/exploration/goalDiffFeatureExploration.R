source(file = 'positionFeatureExtraction.R', echo = FALSE, encoding = 'UTF-8')

positions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 'mid', 'off', 'off', 'off', 'off', 'off')
featuredMatches <- extractMatchResultFeatures(playerStats = data$stats,
                                              matches = data$matches,
                                              priceAssignedPositions = positions,
                                              functs = c('min', 'max', 'avg', 'sum'))

featurePlot(x = select(featuredMatches, def_Price_Home_sum, def_Price_Home_avg, 
                       def_Price_Home_min, def_Price_Home_max, def_Price_Visitors_sum, 
                       def_Price_Visitors_avg, def_Price_Visitors_min, def_Price_Visitors_max),
            y = featuredMatches$goalDiff,
            between = list(x = 1, y =  1),
            type = c('g', 'p', 'smooth'))


describe(dplyr::select(featuredMatches, off_Price_Home_avg, off_Price_Visitors_avg))