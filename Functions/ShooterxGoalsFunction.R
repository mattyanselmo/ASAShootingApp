# library(dplyr)
# playerxgoals <- readRDS('AppData/xGoalsByPlayer.rds') %>%
#   mutate(date = as.Date(date, format = '%m/%d/%Y'))
# minutes_df <- readRDS("AppData/MinutesByGameID_forapp.rds")
# playerpos <- readRDS("AppData/playerpositions_byseason.rds")
# date1 = as.Date('2011-01-01')
# date2 = as.Date('2018-12-31')
# season = 2011:2018
# minfilter = 0
# shotfilter = 0
# keyfilter = 0
# teamfilter = unique(playerxgoals$team)
# byteams = F
# byseasons = T
# FK = T
# PK = T
# OpenPlay = T
# SetPiece = T
# Mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }

shooterxgoals.func <- function(playerxgoals = playerxgoals,
                               minutes_df = minutesPlayed,
                               date1 = as.Date('2000-01-01'), 
                               date2 = as.Date('9999-12-31'),
                               season = 2011:2017,
                               minfilter = 0,
                               shotfilter = 0, 
                               keyfilter = 0,
                               teamfilter = unique(playerxgoals$team),
                               byteams = F,
                               byseasons = T,
                               OpenPlay = T,
                               FK = T,
                               PK = T,
                               SetPiece = T){
  
  tempmins <- minutes_df %>%
    filter(date >= date1 & date <= date2,
           Season %in% season)
  
  tempdat <- playerxgoals %>%
    filter(date >= date1 & date <= date2,
           Season %in% season,
           type %in% c("Open play"[OpenPlay], 'Direct FK'[FK], 'PK'[PK], "Set piece"[SetPiece]),
           team %in% teamfilter)
  
  if(byteams & byseasons){
    aggdata <- tempdat %>%
      group_by(player, Team = team, Season) %>%
      summarize(Pos = season.pos[1],
                Shots = sum(shots),
                SoT = sum(ontarget),
                Dist = sum(shots*meddist, na.rm = T)/sum(shots),
                Solo = sum(unassisted)/Shots,
                Goals = sum(goals),
                xG = sum(xG),
                xPlace = sum(xG_gk) - xG,
                `G-xG` = sum(`G-xG`),
                KeyP = sum(keypasses),
                Dist.key = sum(keypasses*meddist.pass, na.rm = T)/sum(keypasses),
                Assts = sum(assists),
                xA = sum(xA),
                `A-xA` = sum(`A-xA`),
                `xG+xA` = sum(xG + xA),
                PA = sum(PA),
                xPA = sum(xPA),
                Salary = mean(Guaranteed, na.rm = T)) %>%
      filter(Shots >= shotfilter,
             KeyP >= keyfilter) %>%
      ungroup()
    
    if(max(season) >= 2015){
      aggdata <- aggdata %>%
        left_join(tempmins %>%
                    group_by(player, Team = team, Season) %>%
                    summarize(Min = sum(minutes)),
                  by = c("player", "Team", "Season")) %>%
        select(Player = player, Team, Season, Min, Pos, Shots:Salary) %>%
        filter(Min >= minfilter | (rep(minfilter == 0, n()) & is.na(Min))) %>%
        ungroup()
    } else{
      aggdata <- aggdata %>%
        select(Player = player, Team, Season, Shots:Salary)
    }
    
  }else if(byteams & !byseasons){
    aggdata <- tempdat %>%
      group_by(player, Team = team) %>%
      summarize(Pos = Mode(season.pos),
                Shots = sum(shots),
                SoT = sum(ontarget),
                Dist = sum(shots*meddist, na.rm = T)/sum(shots),
                Solo = sum(unassisted)/Shots,
                Goals = sum(goals),
                xG = sum(xG),
                xPlace = sum(xG_gk) - xG,
                `G-xG` = sum(`G-xG`),
                KeyP = sum(keypasses),
                Dist.key = sum(keypasses*meddist.pass, na.rm = T)/sum(keypasses),
                Assts = sum(assists),
                xA = sum(xA),
                `A-xA` = sum(`A-xA`),
                `xG+xA` = sum(xG + xA),
                PA = sum(PA),
                xPA = sum(xPA),
                Salary = mean(Guaranteed, na.rm = T)) %>%
      filter(Shots >= shotfilter,
             KeyP >= keyfilter) %>%
      ungroup()
    
    if(max(season) >= 2015){
      aggdata <- aggdata %>%
        left_join(tempmins %>%
                    group_by(player, Team = team) %>%
                    summarize(Min = sum(minutes)),
                  by = c("player", "Team")) %>%
        select(Player = player, Team, Min, Pos, Shots:Salary) %>%
        filter(Min >= minfilter | (rep(minfilter == 0, n()) & is.na(Min))) %>%
        ungroup()
    } else{
      aggdata <- aggdata %>%
        select(Player = player, Team, Shots:Salary)
    }
    
    
  } else if(!byteams & byseasons){
    aggdata <- tempdat %>%
      group_by(player, Season) %>%
      summarize(Pos = Mode(season.pos),
                Team = paste0(na.omit(unique(team)), collapse = ', '),
                Shots = sum(shots),
                SoT = sum(ontarget),
                Dist = sum(shots*meddist, na.rm = T)/sum(shots),
                Solo = sum(unassisted)/Shots,
                Goals = sum(goals),
                xG = sum(xG),
                xPlace = sum(xG_gk) - xG,
                `G-xG` = sum(`G-xG`),
                KeyP = sum(keypasses),
                Dist.key = sum(keypasses*meddist.pass, na.rm = T)/sum(keypasses),
                Assts = sum(assists),
                xA = sum(xA),
                `A-xA` = sum(`A-xA`),
                `xG+xA` = sum(xG + xA),
                PA = sum(PA),
                xPA = sum(xPA),
                Salary = mean(Guaranteed, na.rm = T)) %>%
      filter(Shots >= shotfilter,
             KeyP >= keyfilter) %>%
      ungroup()
    
    if(max(season) >= 2015){
    aggdata <- aggdata  %>%
      left_join(tempmins %>%
                  group_by(player, Season) %>%
                  summarize(Min = sum(minutes)),
                by = c("player", "Season")) %>%
      select(Player = player, Team, Season, Min, Pos, Shots:Salary) %>%
      filter(Min >= minfilter | (rep(minfilter == 0, n()) & is.na(Min))) %>%
      ungroup()
    } else{
      aggdata <- aggdata %>%
        select(Player = player, Team, Season, Shots:Salary)
    }
    
  } else{
    aggdata <- tempdat %>%
      group_by(player) %>%
      summarize(Pos = Mode(season.pos),
                Team = paste0(na.omit(unique(team)), collapse = ', '),
                Shots = sum(shots),
                SoT = sum(ontarget),
                Dist = sum(shots*meddist, na.rm = T)/sum(shots),
                Solo = sum(unassisted)/Shots,
                Goals = sum(goals),
                xG = sum(xG),
                xPlace = sum(xG_gk) - xG,
                `G-xG` = sum(`G-xG`),
                KeyP = sum(keypasses),
                Dist.key = sum(keypasses*meddist.pass, na.rm = T)/sum(keypasses),
                Assts = sum(assists),
                xA = sum(xA),
                `A-xA` = sum(`A-xA`),
                `xG+xA` = sum(xG + xA),
                PA = sum(PA),
                xPA = sum(xPA),
                Salary = mean(Guaranteed, na.rm = T)) %>%
      filter(Shots >= shotfilter,
             KeyP >= keyfilter) %>%
      ungroup()
    
    if(max(season) >= 2015){
      aggdata <- aggdata %>%
        left_join(tempmins %>%
                    group_by(player) %>%
                    summarize(Min = sum(minutes)),
                  by = c("player")) %>%
        select(Player = player, Team, Min, Pos, Shots:Salary) %>%
        filter(Min >= minfilter | (rep(minfilter == 0, n()) & is.na(Min))) %>%
        ungroup()
    } else{
      aggdata <- aggdata %>%
        select(Player = player, Team, Shots:Salary)
    }
  }
  
  return(aggdata %>% 
           filter(!is.na(Player)) %>%
           arrange(desc(`xG+xA`)))
  
}

# # Function example
# library(dplyr)
# playerpos <- readRDS("AppData/playerpositions_byseason.rds")
# Mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }
# shooterxgoals.func(playerxgoals = readRDS('AppData/xGoalsByPlayer.rds') %>%
#                      mutate(date = as.Date(date, format = '%m/%d/%Y')),
#                    minutes_df <- readRDS("AppData/MinutesByGameID_forapp.rds"),
#                    date1 = as.Date('2011-03-01'),
#                    date2 = as.Date('2018-12-31'),
#                    season = as.numeric(format(as.Date('2011-03-01'), "%Y")):as.numeric(format(as.Date('2018-12-01'), "%Y")),
#                    shotfilter = 0,
#                    keyfilter = 0,
#                    byteams = F,
#                    byseasons = T,
#                    OpenPlay = T,
#                    FK = T,
#                    PK = T,
#                    SetPiece = T) %>%
#   mutate(`xG/shot` = ifelse(Shots > 0, xG/Shots, 0),
#          `xA/pass` = ifelse(KeyP > 0, xA/KeyP, 0),
#          `G-xG/shot` = ifelse(Shots > 0, `G-xG`/Shots, 0),
#          `A-xA/pass` = ifelse(KeyP > 0, `A-xA`/KeyP, 0))
