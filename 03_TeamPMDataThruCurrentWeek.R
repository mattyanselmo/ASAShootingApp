dat <- readRDS('IgnoreList/AllShotsData2011-2017.rds')
current.season <- year
week.pred <- max.week

dat.for <- dat %>%
  mutate(Season = as.numeric(format(date, '%Y')),
         xG_keeper = ifelse(is.na(xGKeeper), 0, xGKeeper)) %>%
  filter(Season == current.season) %>%
  mutate(week = floor((as.numeric(date) - min(as.numeric(date)))/7) + 1) %>%
  filter(week <= week.pred) %>%
  group_by(team, date) %>%
  summarize(week = week[1],
            hteam = hteam[1],
            ateam = ateam[1],
            xGF_team = sum(xGTeam),
            xGF_shooter = sum(xGShooter),
            xGF_gk = sum(xG_keeper),
            GF = sum(result == 'Goal'),
            final = final[1]) %>%
  ungroup() %>% 
  group_by(team) %>%
  arrange(date) %>%
  summarize(date = max(date), 
            week = max(week),
            xGF_season_team = mean(xGF_team),
            xGF_season_shooter = mean(xGF_shooter),
            xGF_season_gk = mean(xGF_gk),
            xGF_last10_team = mean(xGF_team[pmax(1, n() - 9):n()]),
            xGF_last10_gk = mean(xGF_gk[pmax(1, n() - 9):n()]),
            xGF_last10_shooter = mean(xGF_shooter[pmax(1, n() - 9):n()]),
            GF_season = mean(GF),
            GF_last10 = mean(GF[pmax(1, n() - 9):n()])) %>%
  ungroup()

dat.against <- dat %>%
  mutate(Season = as.numeric(format(date, '%Y')),
         xG_keeper = ifelse(is.na(xGKeeper), 0, xGKeeper)) %>%
  filter(Season == current.season) %>%
  mutate(week = floor((as.numeric(date) - min(as.numeric(date)))/7) + 1) %>%
  filter(week <= week.pred) %>%
  group_by(team.1, date) %>%
  summarize(week = week[1],
            hteam = hteam[1],
            ateam = ateam[1],
            xGA_team = sum(xGTeam),
            xGA_shooter = sum(xGShooter),
            xGA_gk = sum(xG_keeper),
            GA = sum(result == 'Goal'),
            final = final[1]) %>%
  ungroup() %>% 
  group_by(team.1) %>%
  arrange(date) %>%
  summarize(date = max(date), 
            week = max(week),
            xGA_season_team = mean(xGA_team),
            xGA_season_shooter = mean(xGA_shooter),
            xGA_season_gk = mean(xGA_gk),
            xGA_last10_team = mean(xGA_team[pmax(1, n() - 9):n()]),
            xGA_last10_gk = mean(xGA_gk[pmax(1, n() - 9):n()]),
            xGA_last10_shooter = mean(xGA_shooter[pmax(1, n() - 9):n()]),
            GA_season = mean(GA),
            GA_last10 = mean(GA[pmax(1, n() - 9):n()])) %>%
  ungroup()

pred.data <- dat.for %>%
  full_join(dat.against, by = c('team' = 'team.1', 'date', 'week'), suffix = c('_for', '_against'))

saveRDS(pred.data %>% mutate(Season = current.season), paste0('IgnoreList/TeamPredictionsData.rds'))
