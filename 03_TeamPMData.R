dat <- readRDS('IgnoreList/AllShotsData2011-2017.rds')
library(MASS)
library(dplyr)
library(tidyr)
library(gbm)
library(nnet)

dat.for <- dat %>%
  mutate(Season = format(date, '%Y'),
         xG_keeper = ifelse(is.na(xGKeeper), 0, xGKeeper)) %>%
  group_by(Season) %>%
  mutate(week = floor((as.numeric(date) - min(as.numeric(date)))/7) + 1) %>%
  group_by(Season, team, date) %>%
  summarize(week = week[1],
            hteam = hteam[1],
            ateam = ateam[1],
            xGF_team = sum(xGTeam),
            xGF_shooter = sum(xGShooter),
            xGF_gk = sum(xG_keeper),
            GF = sum(result == 'Goal'),
            final = final[1]) %>%
  ungroup() %>% 
  group_by(Season, team) %>%
  arrange(date) %>%
  mutate(xGF_season_team = lag(cumsum(xGF_team)),
         xGF_season_shooter = lag(cumsum(xGF_shooter)),
         xGF_season_gk = lag(cumsum(xGF_gk)),
         xGF_last10_team = sapply(sapply(1:n(), function(x) xGF_team[pmax(0, x - 10):(x - 1)]), mean),
         xGF_last10_gk = sapply(sapply(1:n(), function(x) xGF_gk[pmax(0, x - 10):(x - 1)]), mean),
         xGF_last10_shooter = sapply(sapply(1:n(), function(x) xGF_shooter[pmax(0, x - 10):(x - 1)]), mean),
         GF_season = lag(cumsum(GF)),
         GF_last10 = sapply(sapply(1:n(), function(x) GF[pmax(0, x - 10):(x - 1)]), mean)) %>%
  ungroup()

dat.against <- dat %>%
  mutate(Season = format(date, '%Y'),
         xG_keeper = ifelse(is.na(xGKeeper), 0, xGKeeper)) %>%
  group_by(Season) %>%
  mutate(week = floor((as.numeric(date) - min(as.numeric(date)))/7) + 1) %>%
  group_by(Season, team.1, date) %>%
  summarize(week = week[1],
            xGA_team = sum(xGTeam),
            xGA_shooter = sum(xGShooter),
            xGA_gk = sum(xG_keeper),
            GA = sum(result == 'Goal')) %>%
  ungroup() %>% 
  group_by(Season, team.1) %>%
  arrange(date) %>%
  mutate(xGA_season_team = lag(cumsum(xGA_team)),
         xGA_season_shooter = lag(cumsum(xGA_shooter)),
         xGA_season_gk = lag(cumsum(xGA_gk)),
         xGA_last10_team = sapply(sapply(1:n(), function(x) xGA_team[pmax(0, x - 10):(x - 1)]), mean),
         xGA_last10_gk = sapply(sapply(1:n(), function(x) xGA_gk[pmax(0, x - 10):(x - 1)]), mean),
         xGA_last10_shooter = sapply(sapply(1:n(), function(x) xGA_shooter[pmax(0, x - 10):(x - 1)]), mean),
         GA_season = lag(cumsum(GA)),
         GA_last10 = sapply(sapply(1:n(), function(x) GA[pmax(0, x - 10):(x - 1)]), sum)) %>%
  ungroup()

dat.pred <- dat.for %>%
  full_join(dat.against, by = c('team' = 'team.1', 'Season', 'date', 'week'), suffix = c('_for', '_against'))

dat.pred <- dat.pred %>%
  filter(team == hteam) %>%
  group_by(Season) %>%
  mutate(gamesplayedH = sapply(1:n(), function(x) sum((hteam == hteam[x] | ateam == hteam[x]) & date < date[x])),
         gamesplayedA = sapply(1:n(), function(x) sum((hteam == ateam[x] | ateam == ateam[x]) & date < date[x]))) %>%
  ungroup() %>%
  select(hteam, ateam, date, gamesplayedH, gamesplayedA) %>%
  left_join(dat.pred %>% 
              select(Season, team, date, xGF_team:GA_last10), 
            by = c('hteam' = 'team', 'date')) %>%
  left_join(dat.pred %>%
              select(team, date, xGF_team:GA_last10),
            by = c('ateam' = 'team', 'date'),
            suffix = c('_home', '_away')) %>%
  filter(!is.na(hteam)) %>%
  group_by(Season) %>%
  mutate(week = floor(as.numeric(date - min(date))/7) + 1) %>%
  ungroup()

dat.pred <- dat.pred %>%
  mutate_at(.vars = names(dat.pred)[!(names(dat.pred) %in% c(grep('season', names(dat.pred), value = T),
                                                             grep('last10', names(dat.pred), value = T)))],
            .funs = function(x) ifelse(is.na(x), 0, x)) %>%
  mutate_at(.vars = grep('home', grep('season', names(dat.pred), value = T), value = T),
            .funs = funs(./gamesplayedH)) %>%
  mutate_at(.vars = grep('away', grep('season', names(dat.pred), value = T), value = T),
            .funs = funs(./gamesplayedA)) %>%
  group_by(Season, hteam) %>%
  arrange(date) %>%
  fill_(grep('home', grep('season', names(test), value = T), value = T), .direction = 'up') %>%
  ungroup() %>%
  group_by(Season, ateam) %>%
  arrange(date) %>%
  fill_(grep('away', grep('season', names(test), value = T), value = T), .direction = 'up') %>%
  ungroup() %>%
  mutate(final = final_home,
         outcome = ifelse(final < 0, 0, ifelse(final == 0, 1, 3))) %>%
  select(-c(final_home, final_away)) %>%
  mutate(Season = as.numeric(Season))

saveRDS(dat.pred, 'IgnoreList/TeamPMData.rds')

