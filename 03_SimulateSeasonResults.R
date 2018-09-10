library(dplyr)
# year <- 2018
sched <- readRDS("IgnoreList/RemainingSchedule.rds")
standings <- readRDS("IgnoreList/CurrentStandings.rds")
pred.data <- readRDS("IgnoreList/TeamPredictionsData.rds")
load('IgnoreList/UnivariatePoissonModels.Rdata')
source("03_TeamPredictiveModelFunction.R")

# About 60 seconds per 100 runs
N <- 1000
standings.cum <- data.frame()
for(run in 1:N){
  scores <- matrix(NA, nrow = nrow(sched), ncol = 2)
  standings.temp <- standings
  for(i in 1:nrow(sched)){
    score.mat <- pm.function(pred.data = pred.data,
                             model.home = model.home,
                             model.away = model.away,
              team.home = sched$Home[i],
              team.away = sched$Away[i],
              season = year)$score.mat
  
  cell <- which(cumsum(score.mat) > runif(1))[1]
  scores[i,] <- c(cell %% 11 - 1, (cell - 1) %/% 11)
  
  standings.temp$Pts[standings.temp$Team == sched$Home[i]] <- standings.temp$Pts[standings.temp$Team == sched$Home[i]] + 3*(scores[i,1] > scores[i,2]) + (scores[i,1] == scores[i,2])
  standings.temp$Pts[standings.temp$Team == sched$Away[i]] <- standings.temp$Pts[standings.temp$Team == sched$Away[i]] + 3*(scores[i,1] < scores[i,2]) + (scores[i,1] == scores[i,2])
  
  standings.temp$GF[standings.temp$Team == sched$Home[i]] <- standings.temp$GF[standings.temp$Team == sched$Home[i]] + scores[i,1]
  standings.temp$GF[standings.temp$Team == sched$Away[i]] <- standings.temp$GF[standings.temp$Team == sched$Away[i]] + scores[i,2]
  
  standings.temp$GA[standings.temp$Team == sched$Home[i]] <- standings.temp$GA[standings.temp$Team == sched$Home[i]] + scores[i,2]
  standings.temp$GA[standings.temp$Team == sched$Away[i]] <- standings.temp$GA[standings.temp$Team == sched$Away[i]] + scores[i,1]
  
  standings.temp$Wins[standings.temp$Team == sched$Home[i]] <- standings.temp$Wins[standings.temp$Team == sched$Home[i]] + (scores[i,1] > scores[i,2])
  standings.temp$Wins[standings.temp$Team == sched$Away[i]] <- standings.temp$Wins[standings.temp$Team == sched$Away[i]] + (scores[i,2] > scores[i,1])
  
  standings.temp$Games[standings.temp$Team == sched$Home[i]] <- standings.temp$Games[standings.temp$Team == sched$Home[i]] + 1
  standings.temp$Games[standings.temp$Team == sched$Away[i]] <- standings.temp$Games[standings.temp$Team == sched$Away[i]] + 1
  
}
standings.cum <- bind_rows(standings.cum, standings.temp %>% mutate(Run = run))
gc()
}

results <- standings.cum %>%
  group_by(Run, Conf) %>%
  arrange(desc(Pts), desc(Wins), desc(GF - GA), desc(GF)) %>%
  mutate(Rank = pmin(7, 1:n())) %>%
  group_by(Run) %>%
  arrange(desc(Pts), desc(Wins), desc(GF - GA), desc(GF)) %>%
  mutate(LeagueRank = 1:n()) %>%
  ungroup()

final.pos <- results %>% 
  group_by(Team, Conf) %>%
  summarize(`1` = mean(Rank == 1),
            `2` = mean(Rank == 2),
            `3` = mean(Rank == 3),
            `4` = mean(Rank == 4),
            `5` = mean(Rank == 5),
            `6` = mean(Rank == 6),
            Playoffs = mean(Rank <= 6),
            Shield = mean(LeagueRank == 1),
            Bye = `1` + `2`) %>%
  arrange(desc(`1`), desc(Playoffs)) %>%
  ungroup()

final.pos
saveRDS(final.pos %>% filter(Conf == "west") %>% select(-Conf), "IgnoreList/CurrentSimulationResults_playoffseeding_west.rds")
saveRDS(final.pos %>% 
          filter(Conf == "west") %>% 
          select(-Conf), 
        paste0("IgnoreList/CurrentSimulationResults_playoffseeding_west_week", max.week, "_year", year, ".rds"))

saveRDS(final.pos %>% filter(Conf == "east") %>% select(-Conf), "IgnoreList/CurrentSimulationResults_playoffseeding_east.rds")
saveRDS(final.pos %>% 
          filter(Conf == "east") %>% 
          select(-Conf), 
        paste0("IgnoreList/CurrentSimulationResults_playoffseeding_east_week", max.week, "_year", year, ".rds"))

