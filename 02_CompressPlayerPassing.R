# Condense player passing data for web app

# Read dataset ####
library(dplyr)
library(stringr)
merged.passes <- readRDS("IgnoreList/AllPassingData.rds")
teamnames <- read.csv('TeamNameLinks.csv', stringsAsFactors = F)

if(file.exists('C:/Users/Matthias')){
  temp <- read.csv(paste0("C:/Users/Matthias/Dropbox/ASA Blog Data/", year, " Stats/touches.csv"))
  write.csv(temp, paste0("C:/Users/Matthias/Documents/GitHub/ASAShootingApp_development/IgnoreList/touches ", year, ".csv"))
  write.csv(temp, paste0("C:/Users/Matthias/Documents/GitHub/ASAShootingApp_master/IgnoreList/touches ", year, ".csv"))
  rm(temp)
  gc()
} else if(file.exists('C:/Users/Matthias.Kullowatz')){
  temp <- read.csv(paste0("C:/Users/Matthias.Kullowatz/Dropbox/ASA Blog Data/", year, " Stats/touches.csv"))
  write.csv(temp, paste0("C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_development/IgnoreList/touches ", year, ".csv"))
  write.csv(temp, paste0("C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_master/IgnoreList/touches ", year, ".csv"))
  rm(temp)
  gc()
}

touches <- bind_rows(lapply(grep('touches', list.files("IgnoreList/"), value = T),
                            function(x) read.csv(paste0("IgnoreList/", x), stringsAsFactors = F))) %>%
  mutate(player = str_replace_all(player, 
                                  c('Kazaishvili' = 'Qazaishvili', 
                                    'Jorge Villafaña' = 'Jorge Villafana',
                                    "Antonio Mlinar Dalamea" = "Antonio Mlinar Delamea")),
         player = ifelse(row_number() %in% grep("Boniek", player), "Oscar Boniek Garcia", player)) %>%
  left_join(teamnames, by = c('team' = 'FullName')) %>%
  mutate(team = Abbr) %>%
  select(-Abbr)

#minutesPlayed_season <- readRDS("IgnoreList/MinutesBySeason.rds")
minutesPlayed_gameID <- readRDS("IgnoreList/MinutesByGameID.rds")
minutesPlayed_gameID <- minutesPlayed_gameID %>%
  left_join(unique(touches %>% 
                     select(gameID, date)) %>%
                     mutate(date = as.Date(date, "%m/%d/%Y"),
                            Season = as.numeric(format(date, "%Y"))),
            by = "gameID")

saveRDS(minutesPlayed_gameID, "IgnoreList/MinutesByGameID.rds")

touches <- minutesPlayed_gameID %>%
  select(gameID, player, minutes, team, date, Season) %>%
  left_join(touches %>%
              select(player, gameID, touches), by = c("gameID", "player")) %>%
  mutate(touches = ifelse(is.na(touches), 0, touches)) %>%
  group_by(gameID, team) %>%
  mutate(TeamTouches = sum(touches)) %>%
  ungroup() %>%
  group_by(player, Season, team) %>%
  summarize(touchpct = sum(touches)/sum((minutes*TeamTouches)/96),
            minutes = sum(minutes),
            touches = sum(touches)) %>%
  filter(team != "Missing")
  
## balance predictions to actual by zone
merged.passes <- merged.passes %>%
  mutate(third = ifelse(x < 115/3, "Def",
                          ifelse(x < 115*2/3, "Mid", "Att"))) %>%
  group_by(passer, year, team, third) %>%
  summarize(N = n(),
            successes = sum(success),
            exp = sum(success.pred),
            Position = typical.pos[1],
            Distance = sum(distance[success == 1]),
            Vert.Dist = sum((endX - x)[success == 1])) %>%
  ungroup()

player.stats <- merged.passes %>% 
  left_join(touches, by = c("passer" = "player", "year" = "Season", "team"))

saveRDS(player.stats, "IgnoreList/xPassingByPlayer.rds")
write.csv(player.stats, "IgnoreList/xPassingByPlayer.csv", row.names = F)
