minutesPlayedPassing <- readRDS('IgnoreList/MinutesByGameID.rds') %>%
  group_by(player, Season) %>%
  summarize(minutes = sum(minutes))

saveRDS(minutesPlayedPassing, "IgnoreList/MinutesBySeason.rds")
