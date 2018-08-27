library(dplyr)

passes <- readRDS("IgnoreList/AllPassingData.rds") %>%
  mutate(time = as.character(time),
         time = sapply(strsplit(time, ':'), function(x) as.numeric(x[1]) + as.numeric(x[2])/60),
         x = x*100/115,
         y = y*100/80)

shots <- readRDS("IgnoreList/AllShotsData2011-2017.rds") %>%
  mutate(x = (115 - abs(distance * cos(angle*pi/180)))*100/115,
         y = (40 + distance * sin(angle*pi/180))*100/80) %>%
  filter(as.numeric(year) >= 2015)

playerpos <- readRDS("IgnoreList/playerpositions_byseason.rds")

library(stringr)

teamnames <- read.csv('TeamNameLinks.csv', stringsAsFactors = F) %>%
  select(-one_of("X"))

path <- ifelse(file.exists("C:/Users/Matthias"), 
               "C:/Users/Matthias", 
               "C:/Users/Matthias.Kullowatz")
# year <- 2018

# Add cards someday

temp <- read.csv(paste0(path, "/Dropbox/ASA Blog Data/", year, " Stats/touches.csv"))
write.csv(temp, paste0(path, "/Documents/GitHub/ASAShootingApp_development/IgnoreList/touches ", year, ".csv"))
write.csv(temp, paste0(path, "/Documents/GitHub/ASAShootingApp_master/IgnoreList/touches ", year, ".csv"))

temp <- read.csv(paste0(path, "/Dropbox/ASA Blog Data/", year, " Stats/raw defensive actions.csv"))
write.csv(temp, paste0(path, "/Documents/GitHub/ASAShootingApp_development/IgnoreList/raw defensive actions ", year, ".csv"))
write.csv(temp, paste0(path, "/Documents/GitHub/ASAShootingApp_master/IgnoreList/raw defensive actions ", year, ".csv"))

temp <- read.csv(paste0(path, "/Dropbox/ASA Blog Data/", year, " Stats/aerials.csv"))
write.csv(temp, paste0(path, "/Documents/GitHub/ASAShootingApp_development/IgnoreList/aerials ", year, ".csv"))
write.csv(temp, paste0(path, "/Documents/GitHub/ASAShootingApp_master/IgnoreList/aerials ", year, ".csv"))

temp <- read.csv(paste0(path, "/Dropbox/ASA Blog Data/", year, " Stats/dribbles.csv"))
write.csv(temp, paste0(path, "/Documents/GitHub/ASAShootingApp_development/IgnoreList/dribbles ", year, ".csv"))
write.csv(temp, paste0(path, "/Documents/GitHub/ASAShootingApp_master/IgnoreList/dribbles ", year, ".csv"))
rm(temp)

temp <- read.csv(paste0(path, "/Dropbox/ASA Blog Data/", year, " Stats/raw fouls suffered.csv"))
write.csv(temp, paste0(path, "/Documents/GitHub/ASAShootingApp_development/IgnoreList/raw fouls suffered ", year, ".csv"))
write.csv(temp, paste0(path, "/Documents/GitHub/ASAShootingApp_master/IgnoreList/raw fouls suffered ", year, ".csv"))

temp <- read.csv(paste0(path, "/Dropbox/ASA Blog Data/", year, " Stats/raw fouls committed.csv"))
write.csv(temp, paste0(path, "/Documents/GitHub/ASAShootingApp_development/IgnoreList/raw fouls committed ", year, ".csv"))
write.csv(temp, paste0(path, "/Documents/GitHub/ASAShootingApp_master/IgnoreList/raw fouls committed ", year, ".csv"))

rm(temp)
gc()


# Defensive actions
defacts <- bind_rows(lapply(grep('defensive actions', list.files("IgnoreList/"), value = T),
                            function(x) read.csv(paste0("IgnoreList/", x), stringsAsFactors = F))) %>%
  mutate(time = sapply(strsplit(time, ':'), function(x) as.numeric(x[1]) + as.numeric(x[2])/60))

# Aerials ####
aerials <- bind_rows(lapply(grep('aerials', list.files("IgnoreList/"), value = T),
                            function(x) read.csv(paste0("IgnoreList/", x), stringsAsFactors = F))) %>%
                              mutate(time = sapply(strsplit(time, ':'), function(x) as.numeric(x[1]) + as.numeric(x[2])/60)) %>%
                              select(-one_of("X"))

# Dribbles ####
dribbles <- bind_rows(lapply(grep("dribbles", list.files("IgnoreList/"), value = T),
                             function(x) read.csv(paste0("IgnoreList/", x), stringsAsFactors = F))) %>%
  mutate(time = sapply(strsplit(time, ':'), function(x) as.numeric(x[1]) + as.numeric(x[2])/60)) %>%
  select(-one_of("X"))

# Fouls
foulsC <- bind_rows(lapply(grep("fouls committed", list.files("IgnoreList/"), value = T),
                                       function(x) read.csv(paste0("IgnoreList/", x), stringsAsFactors = F))) %>%
  mutate(time = sapply(strsplit(time, ':'), function(x) as.numeric(x[1]) + as.numeric(x[2])/60)) %>%
  select(-one_of("X"))

foulsS <- bind_rows(lapply(grep("fouls suffered", list.files("IgnoreList/"), value = T),
                           function(x) read.csv(paste0("IgnoreList/", x), stringsAsFactors = F))) %>%
  mutate(
    player = ifelse(is.na(player), playerFouled, player),
    time = sapply(strsplit(time, ':'), function(x) as.numeric(x[1]) + as.numeric(x[2])/60)) %>%
  select(-one_of(c("X")))

# LOOK FOR ####
# How many successful tackles result in change of possession?
# How about interceptions?

# Select which defensive actions interrupt possession

# Combine data ####
combined <- bind_rows(shots %>% mutate(action = "shot",
                                       outcome = 1) %>% select(-year),
                      passes %>% mutate(action = "pass",
                                        player = passer) %>% select(-year),
                      dribbles %>% mutate(action = "dribble",
                                          date = as.Date(date, "%m/%d/%Y")),
                      # aerials %>% mutate(action = "aerial",
                      #                    date = as.Date(date, "%m/%d/%Y")),
                      foulsS %>% mutate(action = "foulsuffered",
                                        date = as.Date(date, "%m/%d/%Y"),
                                        outcome = 1),
                      defacts %>% mutate(date = as.Date(date, "%m/%d/%Y"))) %>%
  mutate(outcome = ifelse(is.na(outcome), success, outcome),
         player = ifelse(action == "shot", shooter, player)) %>%
  select(-success, -shooter) %>%
  left_join(teamnames, by = c("team" = "FullName")) %>%
  mutate(team = ifelse(is.na(Abbr), team, Abbr)) %>%
  select(-Abbr) %>%
  left_join(teamnames, by = c("team.1" = "FullName")) %>%
  mutate(team.1 = ifelse(is.na(Abbr), team.1, Abbr)) %>%
  select(-Abbr)

combined <- combined %>%
  group_by(gameID) %>%
  arrange(half, time) %>%
  mutate(RecentPossTeam = c("First", unlist(sapply(1:n(), function(x) team[pmax(0, max(which(action %in% c("pass", "dribble", "shot", "foulsuffered") & row_number() < x)))]))),
         ChainChange = ifelse(action %in% c("pass", "dribble", "shot", "foulsuffered") & team != RecentPossTeam, 1, 0),
         ChainChange = ifelse(row_number() == 1 | lag(half) != half, 1, ChainChange),
         ChainID = cumsum(ChainChange)) %>%
  group_by(gameID, ChainID) %>%
  mutate(pattern = ifelse(sum(patternOfPlay.model == "Penalty", na.rm = T) > 0, "Penalty",
                          ifelse(sum(patternOfPlay.model %in% c("Corner", "Set piece", "Free kick"), na.rm = T) > 0, "Set Piece",
                                 ifelse(mean(is.na(patternOfPlay.model)) == 1, "None", "Regular"))),
         xG = 1 - prod(1 - na.omit(c(xGShooter[patternOfPlay.model != "Penalty"], 
                    0.2*(patternOfPlay.model == "Penalty")))),
         xGs = sum(xGShooter, na.rm = T),
         G = sum(result == "Goal", na.rm = T),
         Vertical = 120*(max(x[action %in% c("pass", "dribble", "shot", "foulsuffered")]) - x[action %in% c("pass", "dribble", "shot", "foulsuffered")][1])/100,
         Horizontal = 80*(max(y[action %in% c("pass", "dribble", "shot", "foulsuffered")]) - y[action %in% c("pass", "dribble", "shot", "foulsuffered")][1])/100,
         TotalTime = max(time[action %in% c("pass", "dribble", "shot", "foulsuffered")]) - min(time[action %in% c("pass", "dribble", "shot", "foulsuffered")])) %>% 
  ungroup()

saveRDS(combined %>% 
          arrange(gameID, half, time) %>%
          select(action, date, time, half, gameID, team, 
                 goalie, team.1, passer, player, recipient, result, 
                 x, y, endX, endY, angle, xPass = success.pred, hscore, ascore, hfinal, 
                 afinal, outcome, keyPass, assist, ChainChange, 
                 ChainID, xG, xGs, G, xGShooter, Vertical, Horizontal, pattern,
                 TotalTime), "IgnoreList/xGChain_combineddata.rds")
set.seed(1)
write.csv(combined %>% 
            filter(gameID %in% sample(unique(gameID), 4, replace = F)) %>%
            arrange(gameID, half, time) %>%
            select(action, date, time, half, gameID, team, 
                   goalie, team.1, passer, player, recipient, result, 
                   x, y, endX, endY, angle, xPass = success.pred, hscore, ascore, hfinal, 
                   afinal, outcome, keyPass, assist, ChainChange, 
                   ChainID, xG, xGs, G, xGShooter, Vertical, Horizontal, pattern,
                   TotalTime), "IgnoreList/xGChain_combineddata_sample.csv", row.names = F)

for(year in 2015:2018){
  write.csv(combined %>% 
              mutate(Season = as.numeric(format(date, "%Y"))) %>%
              filter(Season == year) %>%
              arrange(gameID, half, time) %>%
              select(action, date, time, half, gameID, team, 
                     goalie, team.1, passer, player, recipient, result, 
                     x, y, endX, endY, angle, xPass = success.pred, hscore, ascore, hfinal, 
                     afinal, outcome, keyPass, assist, ChainChange, 
                     ChainID, xG, xGs, G, xGShooter, Vertical, Horizontal, pattern,
                     TotalTime), 
            file = paste0("IgnoreList/xGChain_combineddata", year, ".csv"), 
            row.names = F)
}
