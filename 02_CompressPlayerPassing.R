# Condense player passing data for web app

# Read dataset ####
passing <- readRDS("IgnoreList/AllPassingData.rds")

## balance predictions to actual by zone
passing <- passing %>%
  mutate(third = ifelse(x < 115/3, "Def",
                          ifelse(x < 115*2/3, "Mid", "Att")))

player.stats <- passing %>%
  group_by(passer, year, team, third) %>%
  summarize(N = n(),
            success = sum(success),
            exp = sum(success.pred))

saveRDS(player.stats, "IgnoreList/xPassingByPlayer.rds")
write.csv(player.stats, "IgnoreList/xPassingByPlayer.csv", row.names = F)
