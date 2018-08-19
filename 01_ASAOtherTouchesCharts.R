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
  mutate(player = ifelse(is.na(player), playerFouled, player),
         time = sapply(strsplit(time, ':'), function(x) as.numeric(x[1]) + as.numeric(x[2])/60)) %>%
  select(-one_of(c("playerFouled", "X")))

# ERROR: looks like there is a challenge issue for chainID=118 gameID=1043908. 
# Goes from a HOU poessesion to SJE with a challenge right in the middle

# Combine data ####
combined <- bind_rows(shots %>% mutate(action = "shot") %>% select(-year),
                      passes %>% mutate(action = "pass") %>% select(-year),
                      # dribbles %>% mutate(action = "dribble",
                      #                     date = as.Date(date, "%m/%d/%Y")),
                      # aerials %>% mutate(action = "aerial",
                      #                    date = as.Date(date, "%m/%d/%Y")),
                      foulsS %>% mutate(action = "foulsuffered",
                                        date = as.Date(date, "%m/%d/%Y")),
                      defacts %>% mutate(date = as.Date(date, "%m/%d/%Y"))) %>%
  mutate(outcome = ifelse(is.na(outcome), success, outcome),
         player = ifelse(action == "shot", shooter, player)) %>%
  select(-success, shooter) %>%
  left_join(teamnames, by = c("team" = "FullName")) %>%
  mutate(team = ifelse(is.na(Abbr), team, Abbr)) %>%
  select(-Abbr) %>%
  left_join(teamnames, by = c("team.1" = "FullName")) %>%
  mutate(team.1 = ifelse(is.na(Abbr), team.1, Abbr)) %>%
  select(-Abbr)

combined <- combined %>%
  group_by(gameID) %>%
  arrange(time) %>%
  mutate(ChainChange = ifelse(action == "challenge", 0,
                              ifelse(lag(team) != team & lag(action) != "challenge", 1,
                                     ifelse(lag(team) != team & lag(action) == "challenge",
                                            ifelse(lag(team, 2) != team, 1, 0), 0))),
         ChainChange = ifelse(row_number() == 1, 1, ChainChange),
         ChainID = cumsum(ChainChange)) %>%
  group_by(gameID, ChainID) %>%
  mutate(xG = 1 - prod(1 - na.omit(c(xGShooter[patternOfPlay.model != "Penalty"], 
                    0.2*(patternOfPlay.model == "Penalty")))),
         G = sum(result == "Goal", na.rm = T),
         Vertical = 120*(max(x) - min(x))/100,
         TotalTime = max(time) - min(time),
         VerticalTime = time[which.max(x)] - time[which.min(x)]) %>% 
  ungroup()

saveRDS(combined, "xGChain_combineddata.rds")
