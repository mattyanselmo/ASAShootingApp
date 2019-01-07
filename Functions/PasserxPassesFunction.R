# Passing breakdown by player


# # Sample inputs:
# playerpassing <- readRDS("IgnoreList/xPassingByPlayer.rds")
# minpasses = 0
# minfilter = 0
# date1 = as.Date('2000-01-01') 
# date2 = as.Date('9999-12-31')
# seasonfilter = 2015:2018
# byteams = F
# byseasons = T
# third.filter = "All"
# pos.filter = c("G", "D", "B", "M", "A", "F", "S")

passer.xpasses <- function(playerpassing,
                           minpasses,
                           minfilter,
                           date1 = as.Date('2000-01-01'), 
                           date2 = as.Date('9999-12-31'),
                           seasonfilter,
                           byteams,
                           byseasons,
                           third.filter = "All", # options = c("All", "Att", "Def", "Mid"),
                           pos.filter = c("G", "D", "B", "M", "A", "F", "S")){
 
  # consider two summaries: for "All" and otherwise
  playerpassing.temp <- playerpassing %>%
    ungroup() %>%
    filter(Season %in% seasonfilter,
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
           arrange(desc(Score)))
  
}

# # Function example:
# passer.xpasses(playerpassing = readRDS("AppData/xPassingByPlayer.rds"),
#                minpasses = 50,
#                minfilter = 0,
#                seasonfilter = 2015:2018,
#                byteams = T,
#                byseasons = T,
#                third.filter = "All",
#                pos.filter = c("G", "D", "B", "M", "A", "F", "S")) %>% as.data.frame() %>% head()
