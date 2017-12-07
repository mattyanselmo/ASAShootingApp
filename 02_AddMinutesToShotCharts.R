playerxgoals <- readRDS('IgnoreList/xGoalsByPlayer.rds')
#shooting <- readRDS('IgnoreList/AllShotsData2011-2017.rds')
minutesPlayed <- bind_rows(lapply(grep('minutes played by game', list.files('IgnoreList/'), value = T),
                                  function(x) read.csv(paste0('IgnoreList/', x), stringsAsFactors = F))) %>%
  select(-X) %>%
  left_join(playerxgoals %>% select(gameID, date) %>% unique(), by = 'gameID') %>%
  left_join(playerxgoals %>% filter(Season > 2014) %>% select(shooter, gameID, team, Season) %>% unique(),
            by = c('gameID', 'player' = 'shooter'))

# Need full join to get minutes for games players don't shoot/record key pass
# There is no record of which team a player plays on if he does not shoot during a game
playerxgoals <- playerxgoals %>%
  full_join(minutesPlayed, by = c('gameID', 'shooter' = 'player', 'date', 'team', 'Season'))

saveRDS(playerxgoals, 'IgnoreList/xGoalsByPlayer.rds')
