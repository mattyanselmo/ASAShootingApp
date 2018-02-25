# Test inputs
# library(dplyr)
# playerxgoals <- readRDS('IgnoreList/xGoalsByPlayer.rds') %>%
#   mutate(date = as.Date(date, format = '%m/%d/%Y'))
# minutes_df <- readRDS('IgnoreList/MinutesByGameID.rds')
# date1 = as.Date('2000-01-01')
# date2 = as.Date('9999-12-31')
# season = 2015:2017
# shotfilter = 0
# keyfilter = 0
# minfilter = 0
# byseasons = T
# byteams = T
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
                                    byteams = T,
                                    OtherShots = T,
                                    FK = T,
                                    PK = T){
  
  tempdat <- playerxgoals %>%
    filter(date >= date1 & date <= date2,
           Season %in% season,
           type %in% c('Other'[OtherShots], 'FK'[FK], 'PK'[PK]))
  
  tempmins <- minutes_df %>%
    filter(date >= date1 & date <= date2,
           Season %in% season)
  # Make byteams splits also, copy from non96 shooterxgoals function
  if(byseasons & byteams){
    aggdata <- tempdat %>%
      group_by(player, Team = team, Season) %>%
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
      left_join(tempmins %>%
                  group_by(player, Team = team, Season) %>%
                  summarize(Min = sum(minutes)), 
                by = c('player', "Team", 'Season')) %>%
      mutate_at(.vars = vars(Shots:`xG+xA`),
                .funs = funs(.*96/Min)) %>%
      select(Player = player, Team, Season, Min, Shots:`xG+xA`) %>%
      filter(Min >= minfilter)
    
  } else if(byteams & !byseasons){
    aggdata <- tempdat %>%
      group_by(player, Team = team) %>%
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
      filter(Shots >= shotfilter,
             KeyP >= keyfilter) %>%
        left_join(tempmins %>%
                    group_by(player, Team = team) %>%
                    summarize(Min = sum(minutes)),
                  by = c("player", "Team")) %>%
        select(Player = player, Team, Min, Shots:`xG+xA`) %>%
        mutate_at(.vars = vars(Shots:`xG+xA`),
                  .funs = funs(.*96/Min)) %>%
      filter(Min >= minfilter)
    
  } else if(!byteams & byseasons){
    aggdata <- tempdat %>%
      group_by(player, Season) %>%
      summarize(Team = paste0(na.omit(unique(team)), collapse = ', '),
                Shots = sum(shots),
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
      filter(Shots >= shotfilter,
             KeyP >= keyfilter) %>%
      left_join(tempmins %>%
                  group_by(player, Season) %>%
                  summarize(Min = sum(minutes)),
                by = c("player", "Season")) %>%
      select(Player = player, Season, Min, Shots:`xG+xA`) %>%
      mutate_at(.vars = vars(Shots:`xG+xA`),
                .funs = funs(.*96/Min)) %>%
      filter(Min >= minfilter)
    
  } else{
    aggdata <- tempdat %>%
      group_by(player) %>%
      summarize(Team = paste0(na.omit(unique(team)), collapse = ', '),
                Shots = sum(shots),
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
      filter(Shots >= shotfilter,
             KeyP >= keyfilter) %>%
        left_join(tempmins %>%
                    group_by(player) %>%
                    summarize(Min = sum(minutes)),
                  by = c("player")) %>%
        select(Player = player, Team, Min, Shots:`xG+xA`) %>%
      mutate_at(.vars = vars(Shots:`xG+xA`),
                .funs = funs(.*96/Min)) %>%
      filter(Min >= minfilter)
  }
  
  return(aggdata %>% 
           filter(!is.na(Player)) %>%
           arrange(desc(`xG+xA`)))
  
}

# shooterxgoals_perminute(playerxgoals = readRDS('IgnoreList/xGoalsByPlayer.rds'),
#                    minutes_df = readRDS('IgnoreList/MinutesByGameID.rds'),
#                    date1 = as.Date('2000-01-01'),
#                    date2 = as.Date('9999-12-31'),
#                    season = 2017,
#                    shotfilter = 0,
#                    keyfilter = 0,
#                    minfilter = 0,
#                    byseasons = T,
#                    byteams = T,
#                    OtherShots = T,
#                    FK = T,
#                    PK = T)