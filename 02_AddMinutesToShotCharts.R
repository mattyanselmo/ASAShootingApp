playerxgoals <- readRDS('IgnoreList/xGoalsByPlayer.rds')
#shooting <- readRDS('IgnoreList/AllShotsData2011-2017.rds')
minutesPlayed <- bind_rows(lapply(grep('minutes played by game', list.files('IgnoreList/'), value = T),
                                  function(x) read.csv(paste0('IgnoreList/', x), stringsAsFactors = F)))

# Need full join to get minutes for games players don't shoot/record key pass
playerxgoals <- playerxgoals %>%
  left_join(minutesPlayed, by = c('gameID', 'shooter' = 'player'))

saveRDS(playerxgoals, 'IgnoreList/xGoalsByPlayer.rds')
