# Produces minutes played per player per game
# Append dates to minutes data
library(dplyr)
library(stringr)
library(zoo)

minutesPlayed <- bind_rows(lapply(grep('minutes played by game', list.files('IgnoreList/'), value = T),
                                  function(x) read.csv(paste0('IgnoreList/', x), stringsAsFactors = F))) %>%
  select(-X) %>%
  mutate(player = str_replace_all(player, 
                           c('Kazaishvili' = 'Qazaishvili', 
                             'Jorge Villafaña' = 'Jorge Villafana',
                             "Antonio Mlinar Dalamea" = "Antonio Mlinar Delamea"))) %>%
  left_join(merged.passes %>%
              select(gameID, date) %>% unique(),
              by = "gameID") %>%
  left_join(merged.passes %>% 
              select(gameID, passer, team) %>% unique(), 
            by = c("gameID", "player" = "passer")) %>%
  mutate(Season = as.numeric(format(date, "%Y"))) %>%
  group_by(player) %>%
  arrange(date) %>%
  mutate(team = as.character(na.locf(team, na.rm = F)),
         team = ifelse(is.na(team), "Missing", team)) %>%
  ungroup()

saveRDS(minutesPlayed, 'IgnoreList/MinutesByGameID.rds')
