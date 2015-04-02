enrichDoublePoissonExpChances <- function(matches, minMatchdays) {
    require(dplyr)
    require(magrittr)
    matches$homeExpChances <- NA
    matches$visitorsExpChances <- NA
    allSeasons <- distinct(select(matches, season))[, 1]
    for(sea in allSeasons) {
        seasonMatches <- matches %>% filter(season == sea)
        allMatchdays <- distinct(select(seasonMatches, matchday))[, 1]
        for(mday in allMatchdays) {
            if(mday <= minMatchdays) {
                next
            }
            modelTeams <- fitChancesModel(seasonMatches, sea, mday)
            paras <- extractDoublePoisParas(modelTeams$model, modelTeams$teams)
            
            matchdayMatches <- seasonMatches %>% filter(matchday == mday)
            for(i in seq(1:nrow(matchdayMatches))) {
                aktMatch <- matchdayMatches[i, ]
                expChances <- calculateExpChances(aktMatch, paras)
                matches[matches$matchId == aktMatch$matchId, 'homeExpChances'] <-
                    expChances$expChancesHome
                matches[matches$matchId == aktMatch$matchId, 'visitorsExpChances'] <-
                    expChances$expChancesVisitors
            }
        }
    }
    return(matches)
}

fitChancesModel <- function(matches, sea, mday) {    
    relMatches <- matches %>% filter(season == sea, matchday < mday) %>%
        select(g1 = homeChances, g2 = visitorsChances, team1 = homeTeamId, 
               team2 = visitorsTeamId) %>%
        mutate(team1 = as.factor(team1), team2 = as.factor(team2))
    
    teams <- distinct(select(relMatches, team1))[, 1]
    contrasts(relMatches$team1) <- contr.sum(length(teams)) 
    contrasts(relMatches$team2) <- contr.sum(length(teams)) 
    
    # formula for modeling of lambda1 and lambda2
    form1 <- ~c(team1,team2)+c(team2,team1)
    
    # double poisson model fit
    doublePoisson <- lm.bp( g1~1, g2~1, l1l2=form1, zeroL3=TRUE, data=relMatches)
    
    return(list(model = doublePoisson, teams = teams))
}

extractDoublePoisParas <- function(model, teams) {
    homeEffect <- model$beta1[1] - model$beta2[1]
    
    names(model$beta1) <- gsub("team1..team2", "attack", names(model$beta1))
    names(model$beta1) <- gsub("team2..team1", "defense", names(model$beta1))
    
    ids <- c()
    attacks <- c()
    defenses <- c()
    for(i in teams) {
        id <- i
        attack <- model$beta1[paste('attack', id, sep = '')]
        defense <- model$beta1[paste('defense', id, sep = '')]
        if(is.na(attack)) {
            attack <- -sum(model$beta1[2:length(teams)])
        }
        if(is.na(defense)) {
            defense <- -sum(
                model$beta1[(length(teams) + 1):(2 * length(teams) - 1)])
        }
        ids <- c(ids, id)
        attacks <- c(attacks, attack)
        defenses <- c(defenses, defense)
    }
    teamStrength <- data.frame(teamId = ids, attack = attacks, defense = defenses, 
                               row.names = NULL)
    teamStrength <- teamStrength %>% arrange(as.integer(teamId))
    
    return(list(homeEffect = homeEffect, teamStrength = teamStrength,
                lambda2Intercept = model$beta2[1]))
}

calculateExpChances <- function(match, paras) {
    homeAtt <- paras$teamStrength %>% filter(teamId == match$homeTeamId) %>%
        select(attack)
    homeDef <- paras$teamStrength %>% filter(teamId == match$homeTeamId) %>%
        select(defense)
    visitorsAtt <- paras$teamStrength %>% filter(teamId == match$visitorsTeamId) %>%
        select(attack)
    visitorsDef <- paras$teamStrength %>% filter(teamId == match$visitorsTeamId) %>%
        select(defense)
    
    lambda1 <- exp(paras$lambda2Intercept + paras$homeEffect + 
                       homeAtt + visitorsDef)
    lambda2 <- exp(paras$lambda2Intercept + visitorsAtt + homeDef)
    
    expHome <- lambda1
    expVisitors <- lambda2
    
    return(list(expChancesHome = expHome, expChancesVisitors = expVisitors))
}