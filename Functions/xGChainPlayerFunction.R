# Filter player chain data for app

# # Test inputs
# library(dplyr)
# playerchaindata <- readRDS("IgnoreList/PlayerxGChainData.rds")
# date1 = as.Date('2000-01-01')
# date2 = as.Date('9999-12-31')
# season.filter = 2015:2017
# min.filter = 0
# team.filter = unique(playerchaindata$team)
# byseasons = T
# byteams = T
# perminute = F
# Mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }


xgchain.function <- function(
 playerchaindata = playerchaindata,
 date1,
 date2,
 season.filter,
 min.filter,
 team.filter = unique(playerchaindata$team),
 byseasons,
 byteams,
 #gamestateind,
 perminute = F
){
  temp <- playerchaindata %>%
    filter(date >= date1 & date <= date2,
           Season %in% season.filter,
           team %in% team.filter
           #,
           #Gamestate0ind %in% gamestateind
           )
 
  if(perminute){ 
  aggdata <- temp %>%
    group_by_(.dots = c("Player" = "player", "Season", "team")[c(T, byseasons, byteams)]) %>%
    summarize(Team = paste0(unique(team), collapse = ","),
              Games = length(unique(gameID)),
              Minutes = sum(minutes[!duplicated(gameID)]),
              Pos = Mode(season.pos),
              `NumChains/96` = sum(num.chains)*96/Minutes,
              `TeamChain%` = sum(num.chains)*Games*96/(sum(num.team.chains)*Minutes),
              `ChainShot%` = sum(shots.chain)/sum(num.chains),
              `PlayerShot%` = sum(shots)/sum(num.chains),
              `PlayerKP%` = sum(keypasses)/sum(num.chains),
              `xB/96` = sum(xG.buildup.noshots)*96/Minutes,
              `xGChain/96` = sum(xG.buildup.shots)*96/Minutes,
              `xB%` = `xB/96`/`xGChain/96`,
              `xB% (0)` = sum(xG.buildup.noshots[Gamestate0ind == 1])/sum(xG.buildup.shots[Gamestate0ind == 1]),
              Comp = mean(Guaranteed, na.rm = T)) %>%
    select(-one_of("team")) %>%
    arrange(desc(`xB/96`))
  } else{
    aggdata <- temp %>%
      group_by_(.dots = c("Player" = "player", "Season", "team")[c(T, byseasons, byteams)]) %>%
      summarize(Team = paste0(unique(team), collapse = ","),
                Games = length(unique(gameID)),
                Minutes = sum(minutes[!duplicated(gameID)]),
                Pos = Mode(season.pos),
                `NumChains` = sum(num.chains),
                `TeamChain%` = sum(num.chains)*Games*96/(sum(num.team.chains)*Minutes),
                `ChainShot%` = sum(shots.chain)/sum(num.chains),
                `PlayerShot%` = sum(shots)/sum(num.chains),
                `PlayerKP%` = sum(keypasses)/sum(num.chains),
                `xB` = sum(xG.buildup.noshots),
                `xGChain` = sum(xG.buildup.shots),
                `xB%` = `xB`/`xGChain`,
                `xB% (0)` = sum(xG.buildup.noshots[Gamestate0ind == 1])/sum(xG.buildup.shots[Gamestate0ind == 1]),
                Comp = mean(Guaranteed, na.rm = T)) %>%
      select(-one_of("team")) %>%
      arrange(desc(`xB`))
  }
  
  return(aggdata %>% 
           filter(Minutes >= min.filter))
}

# # Function example
# library(dplyr)
# Mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }
# xgchain.function(playerchaindata = playerchaindata,
#                  date1 = "2000-01-01",
#                  date2 = "9999-12-31",
#                  season.filter = 2015:2016,
#                  min.filter = 500,
#                  team.filter = unique(playerchaindata$team),
#                  byseasons = T,
#                  byteams = T)