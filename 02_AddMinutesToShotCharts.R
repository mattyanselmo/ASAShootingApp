
minutesPlayed <- bind_rows(lapply(grep('minutes played by game', list.files('IgnoreList/'), value = T),
                                  function(x) read.csv(paste0('IgnoreList/', x), stringsAsFactors = F))) %>%
  select(-X) %>%
  left_join(playerxgoals %>% select(gameID, date) %>% unique(), by = 'gameID') %>%
  mutate(Season = as.numeric(format(date, '%Y')))

saveRDS(minutesPlayed, 'IgnoreList/MinutesByGameID.rds')
