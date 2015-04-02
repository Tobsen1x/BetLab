enrichBivPoisExpGoals <- function(matches, minMatchdays) {
    require(dplyr)
    require(magrittr)
    matches$homeExpGoals <- NA
    matches$visitorsExpGoals <- NA
    allSeasons <- distinct(select(matches, season))[, 1]
    for(sea in allSeasons) {
        seasonMatches <- matches %>% filter(season == sea)
        allMatchdays <- distinct(select(seasonMatches, matchday))[, 1]
        for(mday in allMatchdays) {
            if(mday <= minMatchdays) {
                next
            }
            modelTeams <- calculateBivPoisModel(seasonMatches, sea, mday)
            paras <- extractBivPoisParas(modelTeams$model, modelTeams$teams)
            
            matchdayMatches <- seasonMatches %>% filter(matchday == mday)
            for(i in seq(1:nrow(matchdayMatches))) {
                aktMatch <- matchdayMatches[i, ]
                expGoals <- calculateExpGoals(aktMatch, paras)
                matches[matches$matchId == aktMatch$matchId, 'homeExpGoals'] <-
                    expGoals$expGoalsHome
                matches[matches$matchId == aktMatch$matchId, 'visitorsExpGoals'] <-
                    expGoals$expGoalsVisitors
            }
        }
    }
    return(matches)
}

calculateExpGoals <- function(match, paras) {
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
    lambda3 <- exp(paras$lambda3Intercept)
    
    expHome <- (1 - paras$p) * (lambda1 + lambda3) + paras$p * paras$theta
    expVisitors <- (1 - paras$p) * (lambda2 + lambda3) + paras$p * paras$theta
    
    return(list(expGoalsHome = expHome, expGoalsVisitors = expVisitors))
}

calculateBivPoisModel <- function(matches, sea, mday) {
    relMatches <- matches %>% filter(season == sea, matchday < mday) %>%
        select(g1 = goalsHome, g2 = goalsVisitors, team1 = homeTeamId, team2 = visitorsTeamId) %>%
        mutate(team1 = as.factor(team1), team2 = as.factor(team2))
    
    teams <- distinct(select(relMatches, team1))[, 1]
    contrasts(relMatches$team1) <- contr.sum(length(teams)) 
    contrasts(relMatches$team2) <- contr.sum(length(teams)) 
    
    # formula for modeling of lambda1 and lambda2
    form1 <- ~c(team1,team2)+c(team2,team1)
    # diagonal inflated bivariate poisson model fit
    bivpoisGoals <-lm.dibp(g1~1,g2~1, l1l2=form1, data=relMatches, jmax=1)
    
    return(list(model = bivpoisGoals, teams = teams))
}

extractBivPoisParas <- function(model, teams) {
    p <- model$p
    theta <- model$theta
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
    
    return(list(p = p, theta = theta, homeEffect = homeEffect, 
                teamStrength = teamStrength, lambda3Intercept = model$beta3,
                lambda2Intercept = model$beta2[1]))
}