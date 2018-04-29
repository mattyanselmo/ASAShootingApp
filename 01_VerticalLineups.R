#Kevin Minkus
#convert horizontal lineups file to be vertical, for easy merging with passing data

library(dplyr)
library(stringr)

if(file.exists('C:/Users/Matthias')){
  temp <- read.csv(paste0("C:/Users/Matthias/Dropbox/ASA Blog Data/", year, " Stats/Starting Lineups.csv"))
  write.csv(temp, paste0("C:/Users/Matthias/Documents/GitHub/ASAShootingApp_development/IgnoreList/Starting Lineups ", year, ".csv"))
  write.csv(temp, paste0("C:/Users/Matthias/Documents/GitHub/ASAShootingApp_master/IgnoreList/Starting Lineups ", year, ".csv"))
  rm(temp)
  gc()
} else if(file.exists('C:/Users/Matthias.Kullowatz')){
  temp <- read.csv(paste0("C:/Users/Matthias.Kullowatz/Dropbox/ASA Blog Data/", year, " Stats/Starting Lineups.csv"))
  write.csv(temp, paste0("C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_development/IgnoreList/Starting Lineups ", year, ".csv"))
  write.csv(temp, paste0("C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_master/IgnoreList/Starting Lineups ", year, ".csv"))
  rm(temp)
  gc()
}

lineups <- bind_rows(lapply(paste0("IgnoreList/", grep('Starting Lineups [[:digit:]]', list.files("IgnoreList/"), value = T)), 
                            function(x) read.csv(x, stringsAsFactors = F) %>% 
                              mutate(formation = as.character(formation)) %>% 
                              select(-one_of("X"))))

# store the info that will be used in every row
infoVars <- c('gameID','team','home','formation')
gameInfo <- lineups[,infoVars]

# get just the player info
playerInfo <- lineups[,!names(lineups) %in% infoVars]
players <- as.vector(unlist(t(playerInfo),use.names=FALSE))

# store the names of each position
positions <- colnames(lineups)[5:22]

#this gives us 18 rows for each game-team combo
eighteenGameInfo <- gameInfo[rep(seq_len(nrow(gameInfo)),each=18),]
#now append each position to it
eighteenGameInfo$position <- rep(positions,nrow(gameInfo))
#now append the players
eighteenGameInfo$players <- players

eighteenGameInfo <- eighteenGameInfo %>%
  mutate(players = str_replace_all(players, 
                                   c('Kazaishvili' = 'Qazaishvili', 
                                     'Jorge Villafaña' = 'Jorge Villafana',
                                     "Antonio Mlinar Dalamea" = "Antonio Mlinar Delamea",
                                     "Ismael Tajouri-Shradi" = "Ismael Tajouri")),
         players = ifelse(row_number() %in% grep("Boniek", players), "Oscar Boniek Garcia", 
                          ifelse(players == "Eddie Johnson" & team == "Portland", "Eddie Johnson (no, not that one)", players)))

write.csv(eighteenGameInfo, 'IgnoreList/vertical starting lineups.csv', row.names = F)
