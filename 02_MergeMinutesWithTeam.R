# Produces minutes played per player per game
# Append dates to minutes data

# merged.passes <- readRDS("IgnoreList/AllPassingData.rds")

library(dplyr)
library(stringr)
library(zoo)
teamnames <- read.csv('TeamNameLinks.csv', stringsAsFactors = F) %>%
  select(-one_of("X"))


if(file.exists('C:/Users/Matthias')){
  temp <- read.csv(paste0("C:/Users/Matthias/Dropbox/ASA Blog Data/", year, " Stats/minutes played by game.csv"))
  write.csv(temp %>% select(-one_of("X")), paste0("C:/Users/Matthias/Documents/GitHub/ASAShootingApp_development/IgnoreList/minutes played by game ", year, ".csv"))
  write.csv(temp %>% select(-one_of("X")), paste0("C:/Users/Matthias/Documents/GitHub/ASAShootingApp_master/IgnoreList/minutes played by game ", year, ".csv"))
  rm(temp)
  gc()
} else if(file.exists('C:/Users/Matthias.Kullowatz')){
  temp <- read.csv(paste0("C:/Users/Matthias.Kullowatz/Dropbox/ASA Blog Data/", year, " Stats/minutes played by game.csv"))
  write.csv(temp %>% select(-one_of("X")), paste0("C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_development/IgnoreList/minutes played by game ", year, ".csv"))
  write.csv(temp %>% select(-one_of("X")), paste0("C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_master/IgnoreList/minutes played by game ", year, ".csv"))
  rm(temp)
  gc()
}

minutesPlayed <- bind_rows(lapply(grep('minutes played by game', list.files('IgnoreList/'), value = T),
                                  function(x) read.csv(paste0('IgnoreList/', x), stringsAsFactors = F))) %>%
  select(-one_of(c("X", "X.1")))

# minutesPlayed <- minutesPlayed %>%
#   left_join(teamnames, by = c('team' = 'FullName')) %>%
#   mutate(team = Abbr) %>%
#   select(-Abbr)

minutesPlayed_gameID <- minutesPlayed %>%
  mutate(player = str_replace_all(player, 
                                  c('Kazaishvili' = 'Qazaishvili', 
                                    'Jorge Villafaña' = 'Jorge Villafana',
                                    "Antonio Mlinar Dalamea" = "Antonio Mlinar Delamea")),
         player = ifelse(row_number() %in% grep("Boniek", player), "Oscar Boniek Garcia", player)) %>%
  left_join(teamnames, by = c('team' = 'FullName')) %>%
  mutate(team = Abbr) %>%
  select(-Abbr)

saveRDS(minutesPlayed_gameID, 'IgnoreList/MinutesByGameID.rds')

# minutesPlayed_gameID <- minutesPlayed %>%
#   select(-X) %>%
#   mutate(player = str_replace_all(player, 
#                                   c('Kazaishvili' = 'Qazaishvili', 
#                                     'Jorge Villafaña' = 'Jorge Villafana',
#                                     "Antonio Mlinar Dalamea" = "Antonio Mlinar Delamea")),
#          player = ifelse(row_number() %in% grep("Boniek", player), "Oscar Boniek Garcia", player)) %>%
#   left_join(merged.passes %>%
#               select(gameID, date) %>% unique(),
#             by = "gameID") %>%
#   left_join(merged.passes %>% 
#               select(gameID, passer, team) %>% unique(), 
#             by = c("gameID", "player" = "passer")) %>%
#   mutate(Season = as.numeric(format(date, "%Y"))) %>%
#   group_by(player, Season) %>%
#   arrange(date) %>%
#   mutate(team = as.character(na.locf(team, na.rm = F)),
#          team = ifelse(is.na(team), "Missing", team)) %>%
#   ungroup()

# By Season
# minutesPlayed_season <- minutesPlayed_gameID %>%
#   group_by(player, Season, team) %>%
#   summarize(minutes = sum(minutes))
# 
# saveRDS(minutesPlayed_season, 'IgnoreList/MinutesBySeason.rds')
