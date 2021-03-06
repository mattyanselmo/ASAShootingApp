# library(dplyr)
# keeperxgoals <- readRDS('IgnoreList/xGoalsByKeeper.rds') %>%
#   mutate(date = as.Date(date, format = '%m/%d/%Y'))
# minutes_df <- readRDS("IgnoreList/MinutesByGameID_forapp.rds")
# date1 = as.Date('2000-01-01')
# date2 = as.Date('9999-12-31')
# season = 2015:2017
# shotfilter = 0
# minfilter = 0
# teamfilter = unique(keeperxgoals$team.1)
# byteams = F
# byseasons = T
# OpenPlay = T
# FK = T
# PK = T
# SetPiece = T

keeperxgoals_per96.func <- function(keeperxgoals = keeperxgoals,
                                    minutes_df,
                                    date1 = as.Date('2000-01-01'), 
                                    date2 = as.Date('9999-12-31'),
                                    season = 2011:2017,
                                    shotfilter = 0, 
                                    minfilter = 0,
                                    teamfilter = unique(keeperxgoals$team.1),
                                    byteams = F,
                                    byseasons = T,
                                    OpenPlay = T,
                                    FK = T,
                                    PK = T,
                                    SetPiece = T){
  
  tempmins <- minutes_df %>%
    filter(date >= date1 & date <= date2,
           Season %in% season)
  
  tempdat <- keeperxgoals %>%
    filter(date >= date1 & date <= date2,
           Season %in% season,
           type %in% c('Open play'[OpenPlay], 'Direct FK'[FK], 'PK'[PK], "Set piece"[SetPiece]),
           team.1 %in% teamfilter)
  
  if(byteams & byseasons){
    aggdata <- tempdat %>%
      group_by(goalie, Team = team.1, Season) %>%
      summarize(Shots = sum(shotsfaced),
                Goals = sum(goals),
                Saves = Shots - Goals,
                `Header%` = sum(headers)/sum(shotsfaced),
                Dist = sum(shotsfaced*meddist, na.rm = T)/sum(shotsfaced),
                xG = sum(xG),
                `G-xG` = sum(`G-xG`),
                Comp = mean(Guaranteed, na.rm = T)) %>%
      filter(Shots >= shotfilter)  %>%
      left_join(tempmins %>%
                  group_by(player, Team = team, Season) %>%
                  summarize(Min = sum(minutes)),
                by = c("goalie" = "player", "Team", "Season")) %>%
      filter(Min >= minfilter | (rep(minfilter == 0, n()) & is.na(Min))) %>%
      mutate_at(.vars = vars(Shots:Saves, xG, `G-xG`),
                .funs = funs(.*96/Min))
    
  }else if(byteams & !byseasons){
    aggdata <- tempdat %>%
      group_by(goalie, Team = team.1) %>%
      summarize(Shots = sum(shotsfaced),
                Goals = sum(goals),
                Saves = Shots - Goals,
                `Header%` = sum(headers)/sum(shotsfaced),
                Dist = sum(shotsfaced*meddist, na.rm = T)/sum(shotsfaced),
                xG = sum(xG),
                `G-xG` = sum(`G-xG`),
                Comp = mean(Guaranteed, na.rm = T)) %>%
      filter(Shots >= shotfilter) %>%
      left_join(tempmins %>%
                  group_by(player, Team = team) %>%
                  summarize(Min = sum(minutes)),
                by = c("goalie" = "player", "Team")) %>%
      filter(Min >= minfilter | (rep(minfilter == 0, n()) & is.na(Min))) %>%
      mutate_at(.vars = vars(Shots:Saves, xG, `G-xG`),
                .funs = funs(.*96/Min))
    
  } else if(!byteams & byseasons){
    aggdata <- tempdat %>%
      group_by(goalie, Season) %>%
      summarize(Team = paste0(na.omit(unique(team.1)), collapse = ', '),
                Shots = sum(shotsfaced),
                Goals = sum(goals),
                Saves = Shots - Goals,
                `Header%` = sum(headers)/sum(shotsfaced),
                Dist = sum(shotsfaced*meddist, na.rm = T)/sum(shotsfaced),
                xG = sum(xG),
                `G-xG` = sum(`G-xG`),
                Comp = mean(Guaranteed, na.rm = T)) %>%
      filter(Shots >= shotfilter) %>% 
      left_join(tempmins %>%
                  group_by(player, Season) %>%
                  summarize(Min = sum(minutes)),
                by = c("goalie" = "player", "Season")) %>%
      filter(Min >= minfilter | (rep(minfilter == 0, n()) & is.na(Min))) %>%
      mutate_at(.vars = vars(Shots:Saves, xG, `G-xG`),
                .funs = funs(.*96/Min))
    
  }else{
    aggdata <- tempdat %>%
      group_by(goalie) %>%
      summarize(Team = paste0(na.omit(unique(team.1)), collapse = ', '),
                Shots = sum(shotsfaced),
                Goals = sum(goals),
                Saves = Shots - Goals,
                `Header%` = sum(headers)/sum(shotsfaced),
                Dist = sum(shotsfaced*meddist, na.rm = T)/sum(shotsfaced),
                xG = sum(xG),
                `G-xG` = sum(`G-xG`),
                Comp = mean(Guaranteed, na.rm = T)) %>%
      filter(Shots >= shotfilter) %>%
      left_join(tempmins %>%
                  group_by(player) %>%
                  summarize(Min = sum(minutes)),
                by = c("goalie" = "player")) %>%
      filter(Min >= minfilter | (rep(minfilter == 0, n()) & is.na(Min))) %>%
      mutate_at(.vars = vars(Shots:Saves, xG, `G-xG`),
                .funs = funs(.*96/Min))
    
  }
  
  return(aggdata %>% 
           filter(!is.na(goalie)) %>%
           rename(Keeper = goalie) %>%
           arrange(`G-xG`)) %>%
    select(one_of(c("Keeper", "Team", "Season", "Min", 
                    "Shots", "Goals", "Saves", "Header%", 
                    "Dist", "xG", "G-xG", "Comp"))) %>%
    ungroup()
  
}

# # Function example
# library(dplyr)
# keeperxgoals_per96.func(keeperxgoals = readRDS('IgnoreList/xGoalsByKeeper.rds') %>%
#                           mutate(date = as.Date(date, format = '%m/%d/%Y')),
#                         minutes_df = readRDS("IgnoreList/MinutesByGameID_forapp.rds"),
#                         date1 = as.Date('2015-01-01'),
#                         date2 = as.Date('9999-12-31'),
#                         season = 2018,
#                         shotfilter = 0,
#                         minfilter = 0,
#                         byteams = F,
#                         byseasons = T,
#                         OpenPlay = T,
#                         FK = T,
#                         PK = T,
#                         SetPiece = T)