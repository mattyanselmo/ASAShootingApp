library(dplyr)
# year <- 2018
sched <- readRDS("IgnoreList/RemainingSchedule.rds")
standings <- readRDS("IgnoreList/CurrentStandings.rds")
pred.data <- readRDS("IgnoreList/TeamPredictionsData.rds")
load('IgnoreList/UnivariatePoissonModels.Rdata')
source("03_TeamPredictiveModelFunction.R")

# About 60 seconds per 100 runs
N <- ifelse(nrow(sched) > 0, 1000, 1)

standings.cum <- data.frame()
for(run in 1:N){
  scores <- matrix(NA, nrow = nrow(sched), ncol = 2)
  standings.temp <- standings
  if(nrow(sched) > 0){
    
    for(i in 1:nrow(sched)){
      score.mat <- pm.function(pred.data = pred.data,
                               model.home = model.home,
                               model.away = model.away,
                               team.home = sched$Home[i],
                               team.away = sched$Away[i],
                               season = year)$score.mat
      
      cell <- which(cumsum(score.mat) > runif(1))[1]
      scores[i,] <- c(cell %% 11 - 1, (cell - 1) %/% 11)
      
      sched[["Win"]][i] <- sum(score.mat[lower.tri(score.mat)])
      sched[["Loss"]][i] <- sum(score.mat[upper.tri(score.mat)])
      sched[["Draw"]][i] <- sum(diag(score.mat))
      
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

final.pos %>% as.data.frame()
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

# Matchup analysis ####
matchup.func <- function(results = results, team1, team2, rank1, rank2){
  temp <- results %>%
    group_by(Run) %>%
    summarize(Matchup = ifelse(Rank[Team == team1] == rank1 & Rank[Team == team2] == rank2, 1, 0)) %>%
    ungroup()
  sum(temp$Matchup)
}

team1 <- "SEA"
team2 <- "RSL"
matchup.func(results, team1, team2, 3, 6)/N
matchup.func(results, team1, team2, 4, 5)/N
matchup.func(results, team1, team2, 5, 4)/N
matchup.func(results, team1, team2, 6, 3)/N

matchups <- matrix(data = 0, 
                   nrow = length(unique(standings$Team)),
                   ncol = length(unique(standings$Team)),
                   dimnames = list(sort(unique(standings$Team)),
                                   sort(unique(standings$Team))))
for(home in standings$Team[standings$Conf == "west"]){
  for(away in standings$Team[standings$Conf == "west"]){
    matchups[rownames(matchups) == home, colnames(matchups) == away] <- matchup.func(results, home, away, 3, 6)/N + matchup.func(results, home, away, 4, 5)/N
  }
}

for(home in standings$Team[standings$Conf == "east"]){
  for(away in standings$Team[standings$Conf == "east"]){
    matchups[rownames(matchups) == home, colnames(matchups) == away] <- matchup.func(results, home, away, 3, 6)/N + matchup.func(results, home, away, 4, 5)/N
  }
}

west <- matchups[rownames(matchups) %in% standings$Team[standings$Conf == "west"], 
         colnames(matchups) %in% standings$Team[standings$Conf == "west"]] %>%
  as.data.frame()
west <- west %>%
  select(which(sapply(west, function(x) sum(x) > 0) | apply(west, 1, function(x) sum(x) > 0))) %>%
  filter(sapply(west, function(x) sum(x) > 0) | apply(west, 1, function(x) sum(x) > 0))
print(west %>%
  mutate(Home = colnames(west)) %>%
  select_(.dots = c("Home", colnames(west))),
  row.names = F)



east <- matchups[rownames(matchups) %in% standings$Team[standings$Conf == "east"], 
                 colnames(matchups) %in% standings$Team[standings$Conf == "east"]] %>%
  as.data.frame()
east <- east %>%
  select(which(sapply(east, function(x) sum(x) > 0) | apply(east, 1, function(x) sum(x) > 0))) %>%
  filter(sapply(east, function(x) sum(x) > 0) | apply(east, 1, function(x) sum(x) > 0))
print(east %>%
        mutate(Home = colnames(east)) %>%
        select_(.dots = c("Home", colnames(east))),
      row.names = F)

