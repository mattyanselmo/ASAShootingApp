# Condense player passing data for web app

# Read dataset ####
passing <- readRDS("IgnoreList/AllPassingData.rds")

## balance predictions to actual by zone
passing <- passing %>%
  mutate(section = ifelse(x < 115/3, "Def",
                          ifelse(x < 115*2/3, "Mid", "Att")))

player.stats <- passing %>%
  group_by(passer, year, team) %>%
  summarize(N = n(),
            `Comp%` = sum(success)/N,
            `Exp%` = mean(success.pred),
            Score = `Comp%` - `Exp%`,
            `N(Def)` = sum(section == "Def"),
            `%(Def)` = sum(success[section == "Def"])/`N(Def)`,
            `Exp%(Def)` = mean(success.pred[section == "Def"]),
            `Score(Def)` = `%(Def)` - `Exp%(Def)`,
            `N(Mid)` = sum(section == "Mid"),
            `%(Mid)` = sum(success[section == "Mid"])/`N(Mid)`,
            `Exp%(Mid)` = mean(success.pred[section == "Mid"]),
            `Score(Mid)` = `%(Mid)` - `Exp%(Mid)`,
            `N(Att)` = sum(section == "Att"),
            `%(Att)` = sum(success[section == "Att"])/`N(Att)`,
            `Exp%(Att)` = mean(success.pred[section == "Att"]),
            `Score(Att)` = `%(Att)` - `Exp%(Att)`)

saveRDS(player.stats, "IgnoreList/xPassingByPlayer.rds")