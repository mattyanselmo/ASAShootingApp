# playerxgoals <- readRDS('IgnoreList/xGoalsByPlayer.rds') %>%
#   mutate(date = as.Date(date, format = '%m/%d/%Y'))
# minutes_df <- readRDS("IgnoreList/MinutesByGameID.rds")
# playerpos <- readRDS("IgnoreList/playerpositions_byseason.rds")
# date1 = as.Date('2000-01-01')
# date2 = as.Date('9999-12-31')
# season = 2014:2015
# minfilter = 0
# shotfilter = 0
# keyfilter = 0
# byteams = F
# byseasons = T
# FK = T
# PK = T
# OtherShots = T
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
                               byteams = F,
                               byseasons = T,
                               OtherShots = T,
                               FK = F,
                               PK = F){
  
  tempmins <- minutes_df %>%
    filter(date >= date1 & date <= date2,
           Season %in% season)
  
  tempdat <- playerxgoals %>%
    filter(date >= date1 & date <= date2,
           Season %in% season,
           type %in% c('Other'[OtherShots], 'FK'[FK], 'PK'[PK]))
  
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
                `xG+xA` = sum(xG + xA)) %>%
      filter(Shots >= shotfilter,
             KeyP >= keyfilter)
    
    if(min(season) >= 2015){
      aggdata <- aggdata %>%
        left_join(tempmins %>%
                    group_by(player, Team = team, Season) %>%
                    summarize(Min = sum(minutes)),
                  by = c("player", "Team", "Season")) %>%
        select(Player = player, Team, Season, Min, Pos, Shots:`xG+xA`) %>%
        filter(Min >= minfilter | (rep(minfilter == 0, n()) & is.na(Min)))
    } else{
      aggdata <- aggdata %>%
        select(Player = player, Team, Season, Shots:`xG+xA`)
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
                `xG+xA` = sum(xG + xA)) %>%
      filter(Shots >= shotfilter,
             KeyP >= keyfilter)
    
    if(min(season) >= 2015){
      aggdata <- aggdata %>%
        left_join(tempmins %>%
                    group_by(player, Team = team) %>%
                    summarize(Min = sum(minutes)),
                  by = c("player", "Team")) %>%
        select(Player = player, Team, Min, Pos, Shots:`xG+xA`) %>%
        filter(Min >= minfilter | (rep(minfilter == 0, n()) & is.na(Min)))
    } else{
      aggdata <- aggdata %>%
        select(Player = player, Team, Shots:`xG+xA`)
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
                `xG+xA` = sum(xG + xA)) %>%
      filter(Shots >= shotfilter,
             KeyP >= keyfilter)
    
    if(min(season) >= 2015){
    aggdata <- aggdata  %>%
      left_join(tempmins %>%
                  group_by(player, Season) %>%
                  summarize(Min = sum(minutes)),
                by = c("player", "Season")) %>%
      select(Player = player, Team, Season, Min, Pos, Shots:`xG+xA`) %>%
      filter(Min >= minfilter | (rep(minfilter == 0, n()) & is.na(Min)))
    } else{
      aggdata <- aggdata %>%
        select(Player = player, Team, Season, Shots:`xG+xA`)
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
                `xG+xA` = sum(xG + xA)) %>%
      filter(Shots >= shotfilter,
             KeyP >= keyfilter)
    
    if(min(season) >= 2015){
      aggdata <- aggdata %>%
        left_join(tempmins %>%
                    group_by(player) %>%
                    summarize(Min = sum(minutes)),
                  by = c("player")) %>%
        select(Player = player, Team, Min, Pos, Shots:`xG+xA`) %>%
        filter(Min >= minfilter | (rep(minfilter == 0, n()) & is.na(Min)))
    } else{
      aggdata <- aggdata %>%
        select(Player = player, Team, Shots:`xG+xA`)
    }
  }
  
  return(aggdata %>% 
           filter(!is.na(Player)) %>%
           arrange(desc(`xG+xA`)))
  
}

# shooterxgoals.func(playerxgoals = readRDS('IgnoreList/xGoalsByPlayer.rds') %>%
#                      mutate(date = as.Date(date, format = '%m/%d/%Y')),
#                    minutes_df <- readRDS("IgnoreList/MinutesByGameID.rds"),
#                    date1 = as.Date('2000-01-01'),
#                    date2 = as.Date('9999-12-31'),
#                    season = 2014:2015,
#                    shotfilter = 0,
#                    keyfilter = 0,
#                    byteams = F,
#                    byseasons = T,
#                    FK = T,
#                    PK = T) %>%
#   mutate(xGperShot = ifelse(Shots > 0, xG/Shots, 0),
#          xAperPass = ifelse(KeyP > 0, xA/KeyP, 0))