playerxgoals <- readRDS('IgnoreList/xGoalsByPlayer.rds') %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y'))
date1 = as.Date('2000-01-01')
date2 = as.Date('9999-12-31')
season = 2011:2017
shotfilter = 0
keyfilter = 0
byteams = T
byseasons = T
FK = T
PK = T
OtherShots = T

shooterxgoals_perminute <- function(playerxgoals = playerxgoals, 
                               date1 = as.Date('2000-01-01'), 
                               date2 = as.Date('9999-12-31'),
                               season = 2011:2017,
                               shotfilter = 0, 
                               keyfilter = 0,
                               byteams = F,
                               byseasons = T,
                               OtherShots = T,
                               FK = F,
                               PK = F){
  
  tempdat <- playerxgoals %>%
    filter(date >= date1 & date <= date2,
           Season %in% season,
           type %in% c('Other'[OtherShots], 'FK'[FK], 'PK'[PK]))
  
  if(byteams & byseasons){
    aggdata <- tempdat %>%
      group_by(player, Team = team, Season) %>%
      summarize(Min = sum(tapply(minutes, gameID, function(x) x[1])),
                Shots = sum(shots)*96/Min,
                SoT = sum(ontarget)*96/Min,
                Dist = sum(shots*meddist, na.rm = T)/sum(shots),
                Solo = sum(unassisted)/Shots,
                Goals = sum(goals)*96/Min,
                xG = sum(xG)*96/Min,
                xPlace = sum(xG_gk)*96/Min - xG,
                `G-xG` = sum(`G-xG`)*96/Min,
                KeyP = sum(keypasses)*96/Min,
                Assts = sum(assists)*96/Min,
                xA = sum(xA)*96/Min,
                `A-xA` = sum(`A-xA`)*96/Min,
                `xG+xA` = sum(xG + xA)) %>%
      filter(Shots >= shotfilter,
             KeyP >= keyfilter)  
    
  }else if(byteams & !byseasons){
    aggdata <- tempdat %>%
      group_by(player, Team = team) %>%
      summarize(Min = sum(tapply(minutes, gameID, function(x) x[1])),
                Shots = sum(shots)*96/Min,
                SoT = sum(ontarget)*96/Min,
                Dist = sum(shots*meddist, na.rm = T)/sum(shots),
                Solo = sum(unassisted)/Shots,
                Goals = sum(goals)*96/Min,
                xG = sum(xG)*96/Min,
                xPlace = sum(xG_gk)*96/Min - xG,
                `G-xG` = sum(`G-xG`)*96/Min,
                KeyP = sum(keypasses)*96/Min,
                Assts = sum(assists)*96/Min,
                xA = sum(xA)*96/Min,
                `A-xA` = sum(`A-xA`)*96/Min,
                `xG+xA` = sum(xG + xA)) %>%
      filter(Shots >= shotfilter,
             KeyP >= keyfilter)
  } else if(!byteams & byseasons){
    aggdata <- tempdat %>%
      group_by(player, Season) %>%
      summarize(Team = paste0(na.omit(unique(team)), collapse = ', '),
                Min = sum(tapply(minutes, gameID, function(x) x[1])),
                Shots = sum(shots)*96/Min,
                SoT = sum(ontarget)*96/Min,
                Dist = sum(shots*meddist, na.rm = T)/sum(shots),
                Solo = sum(unassisted)/Shots,
                Goals = sum(goals)*96/Min,
                xG = sum(xG)*96/Min,
                xPlace = sum(xG_gk)*96/Min - xG,
                `G-xG` = sum(`G-xG`)*96/Min,
                KeyP = sum(keypasses)*96/Min,
                Assts = sum(assists)*96/Min,
                xA = sum(xA)*96/Min,
                `A-xA` = sum(`A-xA`)*96/Min,
                `xG+xA` = sum(xG + xA)) %>%
      filter(Shots >= shotfilter,
             KeyP >= keyfilter)
  } else{
    aggdata <- tempdat %>%
      group_by(player) %>%
      summarize(Team = paste0(na.omit(unique(team)), collapse = ', '),
                Min = sum(tapply(minutes, gameID, function(x) x[1])),
                Shots = sum(shots)*96/Min,
                SoT = sum(ontarget)*96/Min,
                Dist = sum(shots*meddist, na.rm = T)/sum(shots),
                Solo = sum(unassisted)/Shots,
                Goals = sum(goals)*96/Min,
                xG = sum(xG)*96/Min,
                xPlace = sum(xG_gk)*96/Min - xG,
                `G-xG` = sum(`G-xG`)*96/Min,
                KeyP = sum(keypasses)*96/Min,
                Assts = sum(assists)*96/Min,
                xA = sum(xA)*96/Min,
                `A-xA` = sum(`A-xA`)*96/Min,
                `xG+xA` = sum(xG + xA)) %>%
      filter(Shots >= shotfilter,
             KeyP >= keyfilter)
  }
  
  return(aggdata %>% 
           filter(!is.na(player)) %>%
           rename(Player = player) %>%
           arrange(desc(`xG+xA`)))
  
}

# shooterxgoals.func(playerxgoals = readRDS('IgnoreList/xGoalsByPlayer.rds') %>%
#                      mutate(date = as.Date(date, format = '%m/%d/%Y')),
#                    date1 = as.Date('2000-01-01'),
#                    date2 = as.Date('9999-12-31'),
#                    season = 2017,
#                    shotfilter = 0,
#                    keyfilter = 0,
#                    byteams = T,
#                    FK = T,
#                    PK = T) %>%
#   mutate(xGperShot = ifelse(Shots > 0, xG/Shots, 0),
#          xAperPass = ifelse(KeyP > 0, xA/KeyP, 0)) -> dt