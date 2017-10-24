dat <- readRDS('IgnoreList/AllShotsData2011-2017.rds')
library(dplyr)

# Include games played
# Prorate by game
# Include response variables (Goals, xGoals, outcome)
# Include defensive variables

dat.for <- dat %>%
  mutate(Season = format(date, '%Y')) %>%
  group_by(Season) %>%
  mutate(week = floor((as.numeric(date) - min(as.numeric(date)))/7) + 1) %>%
  group_by(Season, team, date) %>%
  summarize(xGF_team = sum(xGTeam),
            xGF_shooter = sum(xGShooter),
            xGF_gk = sum(xGKeeper),
            GF = sum(result == 'Goal')) %>%
  ungroup() %>% group_by(Season, team) %>%
  arrange(date) %>%
  mutate(xGF_team = lag(cumsum(xGF_team)),
         xGF_shooter = lag(cumsum(xGF_shooter)),
         xGF_gk = lag(cumsum(xGF_gk)),
         GF_season = lag(cumsum(GF)),
         GF_last10 = sapply(sapply(1:n(), function(x) GF[pmax(0, x - 10):(x - 1)]), sum))
  
