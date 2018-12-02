mls <- passing %>% 
  filter(year == 2018) %>% 
  group_by(gameID, team, spread = ifelse(home == 1, pmax(-2, pmin(2, hscore - ascore)), pmax(-2, pmin(2, ascore - hscore)))) %>% 
  mutate(final = ifelse(home == 1, hfinal - afinal, afinal - hfinal)) %>% 
  filter(row_number() == 1) %>% 
  summarize(Opponent = team.1[1], 
            Home = home[1], 
            Date = date[1], 
            Win = ifelse(final > 0, 1, 0), 
            Loss = ifelse(final < 0, 1, 0), 
            Draw = ifelse(final == 0, 1, 0))

mls %>%
  group_by(Home, spread) %>%
  summarize(N = n(), WinPct = mean(Win), LossPct = mean(Loss), DrawPct = mean(Draw))

atl <- passing %>% 
  filter(team == "ATL", year == 2018) %>% 
  mutate(final = ifelse(home == 1, hfinal - afinal, afinal - hfinal)) %>% 
  group_by(spread = ifelse(home == 1, pmax(-2, pmin(2, hscore - ascore)), pmax(-2, pmin(2, ascore - hscore))), 
           gameID) %>% 
  filter(row_number() == 1) %>% 
  summarize(Opponent = team.1[1], 
            Home = home[1], 
            Date = date[1], 
            Win = ifelse(final > 0, 1, 0), 
            Loss = ifelse(final < 0, 1, 0), 
            Draw = ifelse(final == 0, 1, 0))