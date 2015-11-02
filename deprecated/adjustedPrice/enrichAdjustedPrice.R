####### TEST #########
#prices <- data$prices
#playerStats <- stats
#seed <- 1234
#result <- extractAdjPriceModelInput(prices, playerStats, players, seed)

# Adjusted Price enrichment
extractAdjPriceModelInput <- function(prices, playerStats, player) {
    prices <- arrange(prices, playerId, informationDate)
    
    ### Getting Basis Data
    beforePlayer <- prices[1, 'playerId']
    resultInput <- data.frame()
    for(rowIndex in 1:nrow(prices)) {
        actPrice <- prices[rowIndex, ]
        actPlayer <- actPrice['playerId']
        if(rowIndex == 1 | actPlayer != beforePlayer) {
            actInput <- createAdjPriceModelInputDataFrame(actPrice)
        } else {
            if(is.null(actInput)) {
                actInput <- createAdjPriceModelInputDataFrame(actPrice)
            } else {
                actInput$end <- actPrice$informationDate
                actInput$endPrice <- actPrice$price
                actInput$weeksPast <- floor(as.numeric(difftime(as.Date(actInput$end), 
                                                                as.Date(actInput$start), 
                                                                unit="weeks")))
                actInput <- enrichBasisObservation(obs = actInput, 
                    players = player, stats = playerStats)
                resultInput <- rbind(resultInput, actInput)
                actInput <- NULL
            }
        }
        beforePlayer <- actPlayer
    }
    
    return(resultInput)
}

extractAge <- function(playerId, date, players) {
    playerBirth <- players[players$id == playerId, 'geburtstag']
    date <- as.Date(date)
    birth <- as.Date(playerBirth)
    age <- floor(as.numeric(difftime(date, birth, unit="weeks") / 52.25))
    return(age)
}

extractFormParas <- function(id, start, end, stats) {
    start <- as.Date(start)
    end <- as.Date(end)
    stats$matchtime <- as.Date(stats$matchtime)
    relevantStats <- filter(stats, matchtime > start, matchtime < end, playerId == id)
    statsCount <- nrow(relevantStats)
    gradeCount <- nrow(filter(relevantStats, !is.na(kickerGrade)))
    positionStats <- filter(relevantStats, !is.na(transPos))
    avgForm <- mean(positionStats$kickerGrade, na.rm = TRUE)
    if(is.nan(avgForm)) {
        avgForm <- NA
    }
    
    youngestStat <- filter(positionStats, matchtime == max(matchtime, na.rm = TRUE))
    if(nrow(youngestStat) > 0) {
        position <- youngestStat$transPos
    } else {
        position <- NA
    }
    
    resultList <- list(avgForm = avgForm, statsCount = statsCount, 
                       gradeCount = gradeCount, position = position)
    return(resultList)
}

enrichBasisObservation <- function(obs, players, stats) {
    obs$age <- extractAge(obs$playerId, obs$end, players)
    formParas <- extractFormParas(obs$playerId, obs$start, obs$end, stats)
    obs$avgForm <- formParas$avgForm
    obs$gradeCount <- formParas$gradeCount
    obs$statsCount <- formParas$statsCount
    obs$position <- formParas$position
    return(obs)
}

# Create basis observation
createAdjPriceModelInputDataFrame <- function(price) {
    result <- data.frame(playerId = price['playerId'], start = price['informationDate'],
                         end = NA, startPrice = price['price'], endPrice = NA, weeksPast = NA,
                         age = NA, avgForm = NA, 'gradeCount' = NA, statsCount = NA,
                         position = NA)
    result <- rename(result, start = informationDate, startPrice = price)
    return(result)
}

# Extracts data for Age Price Adjustment
extractAdjStatsModelInput <- function(player = players,
                                      playerStats = stats) {
    for(row in 1:nrow(playerStats)) {
        aktStat <- playerStats[row, ]
        playerStats[row, 'birthday'] <- player[player$id == aktStat$playerId, 'geburtstag']
    }
    
    result <- mutate(playerStats, age = floor(as.numeric(difftime(as.Date(matchtime), as.Date(birthday), 
                                                                  unit="weeks") / 52.25)))
    return(result)
}