# Condense team passing data for web app

# Read dataset ####
merged.passes <- readRDS("IgnoreList/AllPassingData.rds")

## balance predictions to actual by zone
merged.passes <- merged.passes %>%
  mutate(third = ifelse(x < 115/3, "Def",
                        ifelse(x < 115*2/3, "Mid", "Att")))

gamesplayed <- merged.passes %>% 
  group_by(year, team) %>%
  summarize(Games = length(unique(gameID)))

team.stats.offense <- merged.passes %>%
  group_by(year, team, third) %>%
  summarize(N = n(),
            successes = sum(success),
            exp = sum(success.pred),
            Distance = sum(distance[success == 1]),
            Vert.Dist = sum((endX - x)[success == 1]))

team.stats.defense <- merged.passes %>%
  group_by(year, team.1, third) %>%
  summarize(N = n(),
            successes = sum(success),
            exp = sum(success.pred),
            Distance = sum(distance[success == 1]),
            Vert.Dist = sum((endX - x)[success == 1]))

saveRDS(gamesplayed, "IgnoreList/GamesPlayed_forTeamPassing.rds")
saveRDS(team.stats.offense, "IgnoreList/xPassingByTeamOffense.rds")
saveRDS(team.stats.defense, "IgnoreList/xPassingByTeamDefense.rds")

write.csv(team.stats.offense, "IgnoreList/xPassingByTeamOffense.csv", row.names = F)
write.csv(team.stats.defense, "IgnoreList/xPassingByTeamDefense.csv", row.names = F)