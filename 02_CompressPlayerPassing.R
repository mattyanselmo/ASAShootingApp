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
            successes = sum(success),
            exp = sum(success.pred),
            Position = typical.pos[1],
            Distance = sum(distance[success == 1]),
            Vert.Dist = sum((endX - x)[success == 1]))

saveRDS(player.stats, "IgnoreList/xPassingByPlayer.rds")
write.csv(player.stats, "IgnoreList/xPassingByPlayer.csv", row.names = F)
