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
                           third.filter = c("Att", "Def", "Mid")){
 
  playerpassing.temp <- playerpassing %>%
    ungroup() %>%
    filter(year %in% seasonfilter) %>%
    group_by_(.dots = c("Player" = "passer", "Season" = "year", "team")[c(T, byseasons, byteams)]) %>%
    summarize(Team = paste(unique(team), collapse = ", "),
              Totals = "Totals",
              Passes = sum(N),
              PassPct = sum(success)/Passes,
              xPass = sum(exp)/Passes,
              Score = (PassPct - xPass)*Passes,
              Per100 = Score*100/Passes,
              Third = third.filter,
              Passes_ = sum(N[third == third.filter]),
              PassPct_ = sum(success[third == third.filter])/Passes_,
              xPass_ = sum(exp[third == third.filter])/Passes_,
              Score_ = (PassPct_ - xPass_)*Passes_,
              Per100_ = Score_*100/Passes_) %>%
    select(-one_of("team")) %>%
    filter(Passes_ > minpasses)
  names(playerpassing.temp) <- gsub("_", " ", names(playerpassing.temp))
  return(playerpassing.temp)
  
}

# Function example:
passer.xpasses(playerpassing = readRDS("IgnoreList/xPassingByPlayer.rds"),
               minpasses = 50,
               seasonfilter = 2015:2016,
               byteams = T,
               byseasons = T,
               third.filter = "Att") %>% as.data.frame() %>% head()