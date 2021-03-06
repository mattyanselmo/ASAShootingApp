# # Function testing
# teamxgoals <- readRDS('AppData/xGoalsByTeam.rds')
# conferences <- read.csv('AppData/teamsbyconferencebyseason.csv')
# date1 = as.Date('2000-01-01')
# date2 = as.Date('9999-12-31')
# season = 2016:2017
# even = F
# pattern = unique(teamxgoals$patternOfPlay.model)
# pergame = T
# advanced = T
# venue = c('Home', 'Away')
# byseasons = T
# plot = T

teamxgoals.func <- function(teamxgoals = teamxgoals, 
                            date1 = as.Date('2000-01-01'), 
                            date2 = as.Date('9999-12-31'),
                            season = 2011:2017,
                            even = F,
                            pattern = unique(teamxgoals$patternOfPlay.model),
                            pergame = F,
                            advanced = F,
                            venue = c('Home', 'Away'),
                            byseasons = T, 
                            confview = T,
                            plot = F){
  
  tempdat <- teamxgoals %>%
    filter(date >= date1 & date <= date2,
           Season %in% season,
           home %in% ifelse(venue == 'Home', 1, 0)) %>%
    group_by_(.dots = c('Team', 'Season')[c(T, byseasons)]) %>%
    mutate(gamesplayed = length(unique(date)),
           Pts = ifelse(is.na(Pts), 0, Pts)) %>%
    ungroup()
  
  ptsdat <- unique(tempdat %>% select_(.dots = c('Team', 'Season', 'date', 'Pts', "xPts", "Comp", "Gini18")[c(T, byseasons, T, T, T, T, T)])) %>%
    group_by_(.dots = c('Team', 'Season')[c(T, byseasons)]) %>%
    summarize(Pts = sum(Pts),
              xPts = sum(xPts),
              Comp = mean(Comp),
              Gini18 = mean(Gini18)) %>%
    ungroup()
  
  tempdat <- tempdat %>%
    filter(evengamestate %in% ifelse(rep(even, 2), c(1, 1), c(0, 1)), 
           patternOfPlay.model %in% pattern)
  
  if(pergame){
    if(advanced){
      if(!plot){
      aggdata <- tempdat %>%
        group_by_(.dots = c('Team', 'Season')[c(T, byseasons)]) %>%
        summarize(Games = gamesplayed[1],
                  ShtF = sum(shots)/Games,
                  ShtA = sum(shotsA)/Games,
                  # `Solo%F` = (sum(shots) - sum(assisted))/sum(shots),
                  # `Solo%A` = (sum(shotsA) - sum(assistedA))/sum(shotsA),
                  GF = sum(goals)/Games,
                  GA = sum(goalsA)/Games,
                  GD = (sum(goals) - sum(goalsA))/Games,
                  xGF = sum(xGF)/Games,
                  xGA = sum(xGA)/Games,
                  xGD = xGF - xGA,
                  `GD-xGD` = GD - xGD,
                  TSR = sum(shots)/sum(shotsA + shots),
                  PDO = round(1000*(sum(goals)/sum(ontarget) + 1 - sum(goalsA)/sum(ontargetA)), 0)) %>%
        ungroup()
      }else{
        aggdata <- tempdat %>%
          group_by_(.dots = c('Team', 'Season')[c(T, byseasons)]) %>%
          summarize(Games = gamesplayed[1],
                    ShtF = sum(shots)/Games,
                    ShtA = sum(shotsA)/Games,
                    GF = sum(goals)/Games,
                    GA = sum(goalsA)/Games,
                    CrossPctF = sum(crossed)/sum(shots),
                    CrossPctA = sum(crossedA)/sum(shotsA),
                    OnTargetF = sum(ontarget)/Games,
                    OnTargetA = sum(ontargetA)/Games,
                    # `Solo%F` = (sum(shots) - sum(assisted))/sum(shots),
                    # `Solo%A` = (sum(shotsA) - sum(assistedA))/sum(shotsA),
                    GF = sum(goals)/Games,
                    GA = sum(goalsA)/Games,
                    GD = (sum(goals) - sum(goalsA))/Games,
                    xGF = sum(xGF)/Games,
                    xGA = sum(xGA)/Games,
                    xGD = xGF - xGA,
                    `GD-xGD` = GD - xGD,
                    TSR = sum(shots)/sum(shotsA + shots),
                    PDO = round(1000*(sum(goals)/sum(ontarget) + 1 - sum(goalsA)/sum(ontargetA)), 0)) %>%
          ungroup()
      }
    }else{
    aggdata <- tempdat %>%
      group_by_(.dots = c('Team', 'Season')[c(T, byseasons)]) %>%
      summarize(Games = gamesplayed[1],
                ShtF = sum(shots)/Games,
                SoTF = sum(ontarget)/Games,
                `SoT%F` = SoTF/ShtF,
                GF = sum(goals)/Games,
                `Finish%F` = sum(goals)/sum(shots),
                ShtA = sum(shotsA)/Games,
                SoTA = sum(ontargetA)/Games,
                `SoT%A` = SoTA/ShtA,
                GA = sum(goalsA)/Games,
                `Finish%A` = sum(goalsA)/sum(shotsA),
                GD = GF - GA)
    }
  }else{
    if(advanced){
      aggdata <- tempdat %>%
        group_by_(.dots = c('Team', 'Season')[c(T, byseasons)]) %>%
        summarize(Games = gamesplayed[1],
                  ShtF = sum(shots),
                  ShtA = sum(shotsA),
                  # `Solo%F` = (sum(shots) - sum(assisted))/sum(shots),
                  # `Solo%A` = (sum(shotsA) - sum(assistedA))/sum(shotsA),
                  GF = sum(goals),
                  GA = sum(goalsA),
                  GD = (sum(goals) - sum(goalsA)),
                  xGF = sum(xGF),
                  xGA = sum(xGA),
                  xGD = xGF - xGA,
                  `GD-xGD` = GD - xGD,
                  TSR = sum(shots)/sum(shotsA + shots),
                  PDO = round(1000*(sum(goals)/sum(ontarget) + 1 - sum(goalsA)/sum(ontargetA)), 0))
    }else{
      aggdata <- tempdat %>%
        group_by_(.dots = c('Team', 'Season')[c(T, byseasons)]) %>%
        summarize(Games = gamesplayed[1],
                  ShtF = sum(shots),
                  SoTF = sum(ontarget),
                  `SoT%F` = SoTF/ShtF,
                  GF = sum(goals),
                  `Finish%F` = sum(goals)/sum(shots),
                  ShtA = sum(shotsA),
                  SoTA = sum(ontargetA),
                  `SoT%A` = SoTA/ShtA,
                  GA = sum(goalsA),
                  `Finish%A` = sum(goalsA)/sum(shotsA),
                  GD = GF - GA)
    }
  }
  
  if(pergame){
    aggdata <- aggdata %>%
      left_join(ptsdat, by = c('Team', 'Season')[c(T, byseasons)]) %>%
      mutate(Pts = Pts/Games,
             xPts = xPts/Games)
  } else{
    aggdata <- aggdata %>%
      left_join(ptsdat, by = c('Team', 'Season')[c(T, byseasons)])
  }
  
  if((length(season) == 1 | format(date1, "%Y") == format(date2, "%Y")) & confview){
  aggdata <- aggdata %>%
    left_join(conferences %>% filter(Season == season) %>% select(-Season), 
              by = c('Team'))
  }
  
  return(aggdata %>%
           ungroup() %>%
           arrange(desc(Pts)))
  
}

# # Function example
# teamxgoals.func(teamxgoals = teamxgoals,
#                 date1 = as.Date('2000-01-01'),
#                 date2 = as.Date('9999-12-31'),
#                 season = 2016:2017,
#                 even = F,
#                 pattern = unique(teamxgoals$patternOfPlay.model),
#                 pergame = T,
#                 advanced = T,
#                 venue = c('Home', 'Away'),
#                 byseasons = T,
#                 plot = F)
