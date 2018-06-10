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
                                    "Antonio Mlinar Dalamea" = "Antonio Mlinar Delamea",
                                    "Ismael Tajouri-Shradi" = "Ismael Tajouri")),
         player = ifelse(row_number() %in% grep("Boniek", player), "Oscar Boniek Garcia", 
                         ifelse(player == "Eddie Johnson" & team == "Portland", "Eddie Johnson (no, not that one)", player))) %>%
  left_join(teamnames, by = c('team' = 'FullName')) %>%
  mutate(team = Abbr) %>%
  select(-Abbr)

saveRDS(minutesPlayed_gameID, 'IgnoreList/MinutesByGameID.rds')

