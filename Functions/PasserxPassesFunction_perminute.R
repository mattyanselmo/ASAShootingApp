# Passing breakdown by player


# # Sample inputs:
# playerpassing <- readRDS("AppData/xPassingByPlayer.rds")
# minpasses = 0
# minfilter = 0
# seasonfilter = 2015:2018
# teamfilter = unique(playerpassing$team)
# byteams = F
# byseasons = T
# third.filter = "All"
# pos.filter = c("GK", "CB", "FB/WB", "CM", "CDM", "CAM", "Wing", "F", "sub")

passer.xpasses.p96 <- function(playerpassing,
                           minpasses,
                           minfilter,
                           seasonfilter,
                           teamfilter,
                           date1 = as.Date('2000-01-01'), 
                           date2 = as.Date('9999-12-31'),
                           byteams,
                           byseasons,
                           third.filter = "All", # options = c("All", "Att", "Def", "Mid"),
                           pos.filter = c("GK", "CB", "FB/WB", "CM", "CDM", "CAM", "Wing", "F", "sub")){
 
  playerpassing.temp <- playerpassing %>%
    ungroup() %>%
    filter(Season %in% seasonfilter,
           team %in% teamfilter,
           date >= date1 & date <= date2,
           Position %in% pos.filter) %>%
    group_by_(.dots = c("Player" = "passer", "Season", "team", "third")[c(T, byseasons, byteams, third.filter != "All")]) %>%
    summarize(Team = paste(unique(team), collapse = ","),
              Min = sum(tapply(minutes, gameID, function(x) x[1])),
              Pos = Position[which.max(touches)],
              Passes = sum(N),
              PassPct = sum(successes)/Passes,
              xPassPct = sum(exp)/Passes,
              Score = (PassPct - xPassPct)*Passes,
              Per100 = Score*100/Passes,
              Distance = sum(Distance)/sum(successes),
              Vertical = sum(Vert.Dist)/sum(successes),
              `Touch%` = sum(tapply(touches*touchpct, gameID, function(x) x[1])/
                               sum(tapply(touches, gameID, function(x) x[1]))),
              Comp = mean(Guaranteed, na.rm = T)) %>%
    ungroup() %>%
    select(-one_of("team")) %>%
    filter(Passes >= minpasses, Min >= minfilter)
  
  if(third.filter != "All"){
  playerpassing.temp <- playerpassing.temp %>%
    filter(third %in% third.filter) %>%
    select(-third, -`Touch%`)
  }
  return(playerpassing.temp %>%
           mutate_at(.vars = c("Passes", "Score"),
                     .funs = funs(.*96/Min)) %>%
           arrange(desc(Score)))
  
}

# # Function example:
# library(dplyr)
# passer.xpasses.p96(playerpassing = readRDS("AppData/xPassingByPlayer.rds"),
#                minpasses = 50,
#                minfilter = 0,
#                seasonfilter = 2015:2018,
#                teamfilter = unique(playerpassing$team),
#                byteams = T,
#                byseasons = T,
#                third.filter = "All",
#                pos.filter = c("GK", "CB", "FB/WB", "CM", "CDM", "CAM", "Wing", "F", "sub")) %>% as.data.frame() %>% head()