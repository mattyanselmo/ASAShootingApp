# Condense player passing data for web app

# Read dataset ####
library(dplyr)
library(stringr)

# load xGchain combined data, minutes by game id
combined <- readRDS("IgnoreList/xGChain_combineddata.rds")
minutesPlayed_gameID <- readRDS("IgnoreList/MinutesByGameID.rds")
playerpos <- readRDS("IgnoreList/playerpositions_byseason.rds")

# summarize relevant chain data by player and date
playerchaindata <- combined %>%
  group_by(gameID, ChainID) %>%
  mutate(
    #Chain.team = team[action %in% c("pass", "dribble", "shot", "foulsuffered")][1],
    Gamestate0ind = ifelse(round(mean((hscore - ascore)[action %in% c("pass", "dribble", "shot", "foulsuffered")])) == 0, 1, 0)
  ) %>%
  ungroup() %>%
  group_by(team, gameID) %>%
  mutate(num.team.chains = length(na.omit(unique(ChainID[outcome == 1 & action %in% c("pass", "dribble", "shot", "foulsuffered")]))),
         xG.team.buildup.shots = sum(tapply(xG[action %in% c("pass", "dribble", "shot", "foulsuffered") & outcome == 1], 
                                            ChainID[action %in% c("pass", "dribble", "shot", "foulsuffered") & outcome == 1], 
                                            function(x) max(c(0, x))))) %>%
  group_by(player, team, gameID, Gamestate0ind) %>%
  summarize(date = date[1],
            num.team.chains = num.team.chains[1],
            xG.team.buildup.shots = xG.team.buildup.shots[1],
            num.chains = length(na.omit(unique(ChainID[outcome == 1 & action %in% c("pass", "dribble", "shot", "foulsuffered")]))),
            shots.chain = length(na.omit(unique(ChainID[outcome == 1 & action %in% c("pass", "dribble", "shot", "foulsuffered") & xG > 0]))),
            shots = length(na.omit(unique(ChainID[action == "shot"]))),
            keypasses = length(na.omit(unique(ChainID[keyPass == 1]))),
            xG.shooter = sum(xGShooter[action == "shot"], na.rm = T),
            #xG.assister = sum(xGShooter[action == "pass" & keyPass == 1], na.rm = T),
            xG.buildup.noshots = sum(tapply(xG[action %in% c("pass", "dribble", "foulsuffered") & outcome == 1 & keyPass != 1], 
                                            ChainID[action %in% c("pass", "dribble", "foulsuffered") & outcome == 1 & keyPass != 1], 
                                            function(x) max(c(0, x)))),
            xG.buildup.shots = sum(tapply(xG[action %in% c("pass", "dribble", "shot", "foulsuffered") & outcome == 1], 
                                          ChainID[action %in% c("pass", "dribble", "shot", "foulsuffered") & outcome == 1], 
                                          function(x) max(c(0, x))))) %>%
  ungroup()


# join on minutes and positions
playerchaindata <- playerchaindata %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  left_join(playerpos, by = c("player" = "passer", "year", "team")) %>%
  left_join(minutesPlayed_gameID, by = c("player", "team", "gameID")) %>%
  rename(Season = year)

# write file
saveRDS(playerchaindata, "IgnoreList/PlayerxGChainData.rds")
write.csv(playerchaindata, "IgnoreList/PlayerxGChainData.csv", row.names = F)
