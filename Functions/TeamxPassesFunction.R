# Aggregate team passing metrics for shiny app

# # Sample inputs ####
# library(dplyr)
# offense <- readRDS("AppData/xPassingByTeamOffense.rds")
# defense <- readRDS("AppData/xPassingByTeamDefense.rds")
# gamesplayed <- readRDS("IgnoreList/GamesPlayed_forTeamPassing.rds")
# season <- 2016:2017
# date1 <- as.Date("2000-01-01")
# date2 <- as.Date("9999-12-31")
# byseasons <- T
# third.filter <- c("Att")
# pergame = T

teampassing.func <- function(offense,
                             defense,
                             date1 = as.Date('2000-01-01'), 
                             date2 = as.Date('9999-12-31'),
                             season = 2011:2018,
                             byseasons,
                             third.filter,
                             pergame = F){
  
  # if(!byseasons){
  #   games_df <- games_df %>%
  #     filter(year %in% season) %>%
  #     group_by(team) %>%
  #     summarize(Games = sum(Games)) %>%
  #     ungroup()
  # }
  
  temp <- offense %>%
    left_join(defense %>% select(-year), 
              by = c("date", "team" = "team.1", "third"), 
              suffix = c("f", "a")) %>%
    ungroup() %>%
    filter(date >= date1 & date <= date2,
           year %in% season,
           third %in% third.filter) %>%
    group_by_(.dots = c("team", "year")[c(T, byseasons)]) %>%
    summarize(Games = length(unique(date)),
              PassF = sum(Nf),
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
              VertDiff = VertF - VertA,
              Gini18 = mean(Gini18f),
              Comp = mean(Compf)) %>%
    ungroup()
  
  if(byseasons){
    temp <- temp %>%
      rename("Season" = "year")
  }
  
  if(pergame){
    return(temp %>%
             mutate(`PassF/g` = PassF/Games,
                    `ScoreF/g` = ScoreF/Games,
                    `PassA/g` = PassA/Games,
                    `ScoreA/g` = ScoreA/Games,
                    `ScoreDiff/g` = ScoreDiff/Games) %>%
             #select(-c(PassF, ScoreF, PassA, ScoreA, ScoreDiff))%>%
             select_(.dots = c("team", "Season"[byseasons], "Games", "`PassF/g`", 
                               "PctF", "xPctF", "`ScoreF/g`", "Per100F", 
                               "VertF", "`PassA/g`", "PctA", "xPctA", 
                               "`ScoreA/g`", "Per100A", "VertA", "`ScoreDiff/g`", 
                               "VertDiff", "Gini18", "Comp")) %>%
             arrange(desc(`ScoreDiff/g`)) %>%
             rename("Team" = "team"))
    
  } else{
    return(temp %>%
             arrange(desc(ScoreDiff)) %>%
             rename("Team" = "team"))
  }
}

# # Function example
# library(dplyr)
# teampassing.func(offense = readRDS("AppData/xPassingByTeamOffense.rds"),
#                  defense = readRDS("AppData/xPassingByTeamDefense.rds"),
#                  date1 = as.Date('2000-01-01'),
#                  date2 = as.Date('9999-12-31'),
#                  season = 2017:2018,
#                  byseasons = T,
#                  third.filter = "Att",
#                  pergame = T)
