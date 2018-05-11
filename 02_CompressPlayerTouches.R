library(dplyr)
library(stringr)

teamnames <- read.csv('TeamNameLinks.csv', stringsAsFactors = F) %>%
  select(-one_of("X"))

if(file.exists('C:/Users/Matthias')){
  temp <- read.csv(paste0("C:/Users/Matthias/Dropbox/ASA Blog Data/", year, " Stats/touches.csv"))
  write.csv(temp, paste0("C:/Users/Matthias/Documents/GitHub/ASAShootingApp_development/IgnoreList/touches ", year, ".csv"))
  write.csv(temp, paste0("C:/Users/Matthias/Documents/GitHub/ASAShootingApp_master/IgnoreList/touches ", year, ".csv"))
  
  temp <- read.csv(paste0("C:/Users/Matthias/Dropbox/ASA Blog Data/", year, " Stats/raw defensive actions.csv"))
  write.csv(temp, paste0("C:/Users/Matthias/Documents/GitHub/ASAShootingApp_development/IgnoreList/raw defensive actions ", year, ".csv"))
  write.csv(temp, paste0("C:/Users/Matthias/Documents/GitHub/ASAShootingApp_master/IgnoreList/raw defensive actions ", year, ".csv"))
  
  temp <- read.csv(paste0("C:/Users/Matthias/Dropbox/ASA Blog Data/", year, " Stats/aerials.csv"))
  write.csv(temp, paste0("C:/Users/Matthias/Documents/GitHub/ASAShootingApp_development/IgnoreList/aerials ", year, ".csv"))
  write.csv(temp, paste0("C:/Users/Matthias/Documents/GitHub/ASAShootingApp_master/IgnoreList/aerials ", year, ".csv"))
  
  temp <- read.csv(paste0("C:/Users/Matthias/Dropbox/ASA Blog Data/", year, " Stats/dribbles.csv"))
  write.csv(temp, paste0("C:/Users/Matthias/Documents/GitHub/ASAShootingApp_development/IgnoreList/dribbles ", year, ".csv"))
  write.csv(temp, paste0("C:/Users/Matthias/Documents/GitHub/ASAShootingApp_master/IgnoreList/dribbles ", year, ".csv"))
  rm(temp)
  gc()
} else if(file.exists('C:/Users/Matthias.Kullowatz')){
  temp <- read.csv(paste0("C:/Users/Matthias.Kullowatz/Dropbox/ASA Blog Data/", year, " Stats/touches.csv"))
  write.csv(temp, paste0("C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_development/IgnoreList/touches ", year, ".csv"))
  write.csv(temp, paste0("C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_master/IgnoreList/touches ", year, ".csv"))
  
  temp <- read.csv(paste0("C:/Users/Matthias.Kullowatz/Dropbox/ASA Blog Data/", year, " Stats/raw defensive actions.csv"))
  write.csv(temp, paste0("C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_development/IgnoreList/raw defensive actions ", year, ".csv"))
  write.csv(temp, paste0("C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_master/IgnoreList/raw defensive actions ", year, ".csv"))
  
  temp <- read.csv(paste0("C:/Users/Matthias.Kullowatz/Dropbox/ASA Blog Data/", year, " Stats/aerials.csv"))
  write.csv(temp, paste0("C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_development/IgnoreList/aerials ", year, ".csv"))
  write.csv(temp, paste0("C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_master/IgnoreList/aerials ", year, ".csv"))
  
  temp <- read.csv(paste0("C:/Users/Matthias.Kullowatz/Dropbox/ASA Blog Data/", year, " Stats/dribbles.csv"))
  write.csv(temp, paste0("C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_development/IgnoreList/dribbles ", year, ".csv"))
  write.csv(temp, paste0("C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_master/IgnoreList/dribbles ", year, ".csv"))
  
  rm(temp)
  gc()
}

# Total touches ####
touches <- bind_rows(lapply(grep('touches', list.files("IgnoreList/"), value = T),
                            function(x) read.csv(paste0("IgnoreList/", x), stringsAsFactors = F)))
# already summarized by gameID

# Defensive actions
defacts <- bind_rows(lapply(grep('defensive actions', list.files("IgnoreList/"), value = T),
                            function(x) read.csv(paste0("IgnoreList/", x), stringsAsFactors = F))) %>%
  group_by(player, gameID, third = ifelse(x < 33.3, "def", ifelse(x < 66.7, "mid", "att"))) %>%
  summarize(bs = sum(action == "block"),
            bc = sum(action == "blockedCross"),
            challenges = sum(action == "challenge"),
            interceptions = sum(action == "interceptions"),
            tackleAttempts = sum(action == "tackle"),
            tackleSuccessess = sum(action == "tackle" & outcome == 1)) %>%
  ungroup()

# Aerials ####
aerials <- bind_rows(lapply(grep('aerials', list.files("IgnoreList/"), value = T),
                            function(x) read.csv(paste0("IgnoreList/", x), stringsAsFactors = F))) %>%
  group_by(player, gameID, third = ifelse(x < 33.3, "def", ifelse(x < 66.7, "mid", "att"))) %>%
  summarize(aerialN = n(),
            aerialSuccesses = sum(success)) %>%
  ungroup()

# Dribbles ####
dribbles <- bind_rows(lapply(grep("dribbles", list.files("IgnoreList/"), value = T),
                             function(x) read.csv(paste0("IgnoreList/", x), stringsAsFactors = F))) %>%
  group_by(player, gameID, third = ifelse(x < 33.3, "def", ifelse(x < 66.7, "mid", "att"))) %>%
  summarize(dribbleN = n(),
            dribbleSuccesses = sum(success)) %>%
  ungroup()


# Minutes played ####
minutesPlayed_gameID <- readRDS("IgnoreList/MinutesByGameID.rds") %>%
  left_join(unique(touches %>%
                     select(gameID, date)) %>%
              mutate(date = as.Date(date, "%m/%d/%Y"),
                     Season = as.numeric(format(date, "%Y"))),
            by = "gameID")


alldat <- minutesPlayed_gameID %>%
  left_join(touches %>%
              select(player, gameID, touches, averageX, averageY), by = c("gameID", "player")) %>%
  mutate(touches = ifelse(is.na(touches), 0, touches)) %>%
  group_by(gameID, team) %>%
  mutate(TeamTouches = sum(touches)) %>%
  ungroup()

alldat <- alldat[rep(1:nrow(alldat), 3),] %>%
  group_by(player, gameID) %>%
  mutate(third = c("def", "mid", "att")) %>%
  ungroup()

nontouches <- defacts %>%
  full_join(aerials, by = c("player", "gameID", "third")) %>%
  full_join(dribbles, by = c("player", "gameID", "third")) %>%
  mutate_at(.vars = c("aerialN", "aerialSuccesses", "dribbleN", "dribbleSuccesses"),
            .funs = function(x) ifelse(is.na(x), 0, x))


alldat <- alldat %>%
  left_join(nontouches, by = c("player", "gameID")) %>%
  mutate_at(.vars = names(nontouches)[sapply(nontouches, class) %in% c("numeric", "integer")],
            .funs = function(x) ifelse(is.na(x), 0, x)) %>%
  mutate(player = str_replace_all(player, 
                                  c('Kazaishvili' = 'Qazaishvili', 
                                    'Jorge Villafaña' = 'Jorge Villafana',
                                    "Antonio Mlinar Dalamea" = "Antonio Mlinar Delamea",
                                    "Ismael Tajouri-Shradi" = "Ismael Tajouri")),
         player = ifelse(row_number() %in% grep("Boniek", player), "Oscar Boniek Garcia", 
                         ifelse(player == "Eddie Johnson" & team == "Portland", "Eddie Johnson (no, not that one)", player))) %>%
  left_join(teamnames, by = c('team' = 'FullName')) %>%
  mutate(team = Abbr) %>%
  select(-Abbr)

saveRDS(minutesPlayed_gameID, "IgnoreList/MinutesByGameID_forapp.rds")
saveRDS(alldat, "IgnoreList/TouchesAndMore.rds")