# Passing breakdown by player

# Sample inputs:
  # playerpassing <- readRDS("IgnoreList/xPassingByPlayer.rds")
  # minpasses = 50
  # seasonfilter = 2015:2016
  # byteams = T
  # byseasons = T
  # third.filter = "Att"
passer.xpasses <- function(playerpassing,
                           minpasses,
                           seasonfilter,
                           byteams,
                           byseasons,
                           third.filter = c("All", "Att", "Def", "Mid")){
 
  
  playerpassing.temp <- playerpassing %>%
    ungroup() %>%
    filter(year %in% seasonfilter) %>%
    group_by_(.dots = c("Player" = "passer", "Season" = "year", "team", "third")[c(T, byseasons, byteams, third.filter != "All")]) %>%
    summarize(Team = paste(unique(team), collapse = ", "),
              Passes = sum(N),
              PassPct = sum(successes)/Passes,
              xPassPct = sum(exp)/Passes,
              Score = (PassPct - xPassPct)*Passes,
              Per100 = Score*100/Passes,
              Distance = sum(Distance)/sum(successes),
              Vertical = sum(Vert.Dist)/sum(successes)) %>%
    ungroup() %>%
    select(-one_of("team")) %>%
    filter(Passes > minpasses)
  
  if(third.filter != "All"){
  playerpassing.temp <- playerpassing.temp %>%
    filter(third %in% third.filter) %>%
    select(-third)
  }
  return(playerpassing.temp)
  
}

# Function example:
# passer.xpasses(playerpassing = readRDS("IgnoreList/xPassingByPlayer.rds"),
#                minpasses = 50,
#                seasonfilter = 2015:2016,
#                byteams = T,
#                byseasons = T,
#                third.filter = "Att") %>% as.data.frame() %>% head()