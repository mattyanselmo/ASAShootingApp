# keeperxgoals <- readRDS('xGoalsByKeeper.rds') %>%
#   mutate(date = as.Date(date, format = '%m/%d/%Y'))
# date1 = as.Date('2000-01-01')
# date2 = as.Date('9999-12-31')
# season = 2011:2017
# shotfilter = 0
# byteams = T
# OtherShots = T
# FK = F
# PK = F

keeperxgoals.func <- function(keeperxgoals = keeperxgoals, 
                               date1 = as.Date('2000-01-01'), 
                               date2 = as.Date('9999-12-31'),
                               season = 2011:2017,
                               shotfilter = 0, 
                               byteams = F,
                               OtherShots = T,
                               FK = F,
                               PK = F){
  
  tempdat <- keeperxgoals %>%
    filter(date >= date1 & date <= date2,
           Season %in% season,
           type %in% c('Other'[OtherShots], 'FK'[FK], 'PK'[PK]))
  
  if(byteams){
    aggdata <- tempdat %>%
      group_by(goalie, Team = team.1) %>%
      summarize(Shots = sum(shotsfaced),
                `Header%` = sum(headers)/sum(shotsfaced),
                Dist = sum(shotsfaced*meddist, na.rm = T)/sum(shotsfaced),
                Goals = sum(goals),
                xG = sum(xG),
                `G-xG` = sum(`G-xG`)) %>%
      filter(Shots >= shotfilter)  
    
  }else{
    aggdata <- tempdat %>%
      group_by(goalie) %>%
      summarize(Team = paste0(na.omit(unique(team.1)), collapse = ', '),
                Shots = sum(shotsfaced),
                `Header%` = sum(headers)/sum(shotsfaced),
                Dist = sum(shotsfaced*meddist, na.rm = T)/sum(shotsfaced),
                Goals = sum(goals),
                xG = sum(xG),
                `G-xG` = sum(`G-xG`)) %>%
      filter(Shots >= shotfilter)
  }
  
  return(aggdata %>% 
           filter(!is.na(goalie)) %>%
           rename(Keeper = goalie) %>%
           arrange(`G-xG`))
  
}

keeperxgoals.func(keeperxgoals = readRDS('xGoalsByKeeper.rds') %>%
                     mutate(date = as.Date(date, format = '%m/%d/%Y')),
                   date1 = as.Date('2000-01-01'),
                   date2 = as.Date('9999-12-31'),
                   season = 2011:2017,
                   shotfilter = 10,
                   byteams = F,
                   OtherShots = T,
                   FK = T,
                   PK = T) -> x