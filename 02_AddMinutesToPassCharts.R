# Produces minute played per player per season
minutesPlayedPassing <- readRDS('IgnoreList/MinutesByGameID.rds') %>%
  mutate(player = str_replace_all(player, 
                                  c('Kazaishvili' = 'Qazaishvili', 
                                    'Jorge Villafaña' = 'Jorge Villafana',
                                    "Antonio Mlinar Dalamea" = "Antonio Mlinar Delamea"))) %>%
  group_by(player, Season) %>%
  summarize(minutes = sum(minutes))

saveRDS(minutesPlayedPassing, "IgnoreList/MinutesBySeason.rds")
