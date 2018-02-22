# Produces minutes played per player per game
# Append dates to minutes data
library(dplyr)
library(stringr)
minutesPlayed <- bind_rows(lapply(grep('minutes played by game', list.files('IgnoreList/'), value = T),
                                  function(x) read.csv(paste0('IgnoreList/', x), stringsAsFactors = F))) %>%
  select(-X) %>%
  mutate(player = str_replace_all(player, 
                           c('Kazaishvili' = 'Qazaishvili', 
                             'Jorge Villafaña' = 'Jorge Villafana',
                             "Antonio Mlinar Dalamea" = "Antonio Mlinar Delamea"))) %>%
  left_join(playerxgoals %>% select(gameID, date) %>% unique(), by = 'gameID') %>%
  mutate(Season = as.numeric(format(date, '%Y')))

saveRDS(minutesPlayed, 'IgnoreList/MinutesByGameID.rds')
