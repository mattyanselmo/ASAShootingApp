# Aggregate team passing metrics for shiny app

# Sample inputs ####
# offense <- readRDS("IgnoreList/xPassingByTeamOffense.rds")
# defense <- readRDS("IgnoreList/xPassingByTeamDefense.rds")
# season <- 2016:2017
# byseasons <- F
# third.filter <- c("Att")

teampassing.func <- function(offense,
                             defense,
                             season,
                             byseasons,
                             third.filter){
  temp <- offense %>%
    left_join(defense, 
              by = c("year", "team" = "team.1", "third"), 
              suffix = c("f", "a")) %>%
    ungroup() %>%
    filter(year %in% season,
           third %in% third.filter) %>%
    group_by_(.dots = c("team", "year")[c(T, byseasons)]) %>%
    summarize(PassF = sum(Nf),
              PctF = sum(successesf)/PassF,
              xPctF = sum(expf)/PassF,
              ScoreF = (PctF - xPctF)*PassF,
              Per100F = ScoreF*100/PassF,
              VertF = sum(Vert.Distf)/sum(successesf),
              PassA = sum(Na),
              PctA = sum(successesa)/PassA,
              xPctA = sum(expa)/PassA,
              VertA = sum(Vert.Dista)/sum(successesa),
              ScoreA = (PctA - xPctA)*PassA,
              Per100A = ScoreA*100/PassA,
              ScoreDiff = ScoreF - ScoreA,
              VertDiff = VertF - VertA) %>%
    ungroup()
  
  if(byseasons){
    temp <- temp %>%
      rename("Season" = "year")
  }
  
  return(temp %>%
           arrange(desc(ScoreDiff)) %>%
           rename("Team" = "team"))
}

# Function example
library(dplyr)
teampassing.func(offense = readRDS("IgnoreList/xPassingByTeamOffense.rds"),
                 defense = readRDS("IgnoreList/xPassingByTeamDefense.rds"),
                 season = 2016:2017,
                 byseasons = T,
                 third.filter = "Att")
