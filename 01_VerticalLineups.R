#Kevin Minkus
#convert horizontal lineups file to be vertical, for easy merging with passing data

library(dplyr)

lineups <- bind_rows(lapply(paste0("IgnoreList/", grep('Starting Lineups', list.files("IgnoreList/"), value = T)), 
                 function(x) read.csv(x, stringsAsFactors = F) %>% mutate(formation = as.character(formation))))

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

write.csv(eighteenGameInfo, 'IgnoreList/vertical starting lineups.csv', row.names = F)
