# Test inputs
# library(dplyr)
# playerxgoals <- readRDS('IgnoreList/xGoalsByPlayer.rds') %>%
#   mutate(date = as.Date(date, format = '%m/%d/%Y'))
# minutesPlayed <- readRDS('IgnoreList/MinutesByGameID.rds')
# date1 = as.Date('2000-01-01')
# date2 = as.Date('9999-12-31')
# season = 2015:2017
# shotfilter = 0
# keyfilter = 0
# minfilter = 0
# byseasons = T
# FK = T
# PK = T
# OtherShots = T

shooterxgoals_perminute <- function(playerxgoals = playerxgoals,
                                    minutes_df = minutesPlayed,
                                    date1 = as.Date('2000-01-01'), 
                                    date2 = as.Date('9999-12-31'),
                                    season = 2015:2017,
                                    minfilter = 0,
                                    shotfilter = 0, 
                                    keyfilter = 0,
                                    byseasons = T,
                                    OtherShots = T,
                                    FK = T,
                                    PK = T){
  
  tempdat <- playerxgoals %>%
    filter(date >= date1 & date <= date2,
           Season %in% season,
           type %in% c('Other'[OtherShots], 'FK'[FK], 'PK'[PK]))
  
  tempmins <- minutes_df %>%
    filter(date >= date1 & date <= date2,
           Season %in% season) %>%
    group_by_(.dots = c('player', 'Season')[c(TRUE, byseasons)]) %>%
    summarize(minutes = sum(minutes))
  
  if(byseasons){
    aggdata <- tempdat %>%
      group_by(player, Season) %>%
      summarize(Shots = sum(shots),
                SoT = sum(ontarget),
                Goals = sum(goals),
                xG = sum(xG),
                xPlace = sum(xG_gk) - xG,
                `G-xG` = sum(`G-xG`),
                KeyP = sum(keypasses),
                Assts = sum(assists),
                xA = sum(xA),
                `A-xA` = sum(`A-xA`),
                `xG+xA` = sum(xG + xA)) %>%
      ungroup() %>%
      filter(Shots >= shotfilter,
             KeyP >= keyfilter) %>%
      left_join(tempmins, by = c('player', 'Season')) %>%
      mutate_at(.cols = vars(Shots:`xG+xA`),
                .funs = funs(.*96/minutes)) %>%
      select(Player = player, Season, Min = minutes, Shots:`xG+xA`) %>%
      filter(Min >= minfilter)
  } else{
    aggdata <- tempdat %>%
      group_by(player) %>%
      summarize(Shots = sum(shots),
                SoT = sum(ontarget),
                Goals = sum(goals),
                xG = sum(xG),
                xPlace = sum(xG_gk) - xG,
                `G-xG` = sum(`G-xG`),
                KeyP = sum(keypasses),
                Assts = sum(assists),
                xA = sum(xA),
                `A-xA` = sum(`A-xA`),
                `xG+xA` = sum(xG + xA)) %>%
      ungroup() %>%
      filter(Shots >= shotfilter,
             KeyP >= keyfilter) %>%
      left_join(tempmins, by = c('player')) %>%
      mutate_at(.cols = vars(Shots:`xG+xA`),
                .funs = funs(.*96/minutes)) %>%
      select(Player = player, Min = minutes, Shots:`xG+xA`) %>%
      filter(Min >= minfilter)
  }
  
  return(aggdata %>% 
           filter(!is.na(Player)) %>%
           arrange(desc(`xG+xA`)))
  
}

# shooterxgoals_perminute(playerxgoals = readRDS('IgnoreList/xGoalsByPlayer.rds'),
#                    minutes_df = minutesPlayed,
#                    date1 = as.Date('2000-01-01'),
#                    date2 = as.Date('9999-12-31'),
#                    season = 2017,
#                    shotfilter = 0,
#                    keyfilter = 0,
#                    minfilter = 0,
#                    byseasons = T,
#                    OtherShots = T,
#                    FK = T,
#                    PK = T) -> dt