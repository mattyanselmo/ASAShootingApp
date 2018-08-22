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
 byteams
){
  temp <- playerchaindata %>%
    filter(date >= date1 & date <= date2,
           Season %in% season.filter,
           team %in% team.filter)
  
  aggdata <- temp %>%
    group_by_(.dots = c("Player" = "player", "Season", "team")[c(T, byseasons, byteams)]) %>%
    summarize(Team = paste0(unique(team), collapse = ","),
              Games = length(unique(gameID)),
              Minutes = sum(minutes),
              Pos = Mode(season.pos),
              `NumChains/96` = sum(num.chains)*96/Minutes,
              `TeamChain%` = sum(num.chains)*Games*96/(sum(num.team.chains)*Minutes),
              `ChainShot%` = sum(shots.chain)/sum(num.chains),
              `xGB/96` = sum(xG.buildup.noshots)*96/Minutes,
              `xGChain/96` = sum(xG.buildup.shots)*96/Minutes,
              `xGB%` = `xGB/96`/`xGChain/96`) %>%
    select(-one_of("team"))
  
  return(aggdata %>% 
           filter(Minutes >= min.filter) %>%
           arrange(desc(`xGB/96`)))
}

# # Function example
# xgchain.function(playerchaindata = playerchaindata,
#                  date1 = "2000-01-01",
#                  date2 = "9999-12-31",
#                  season.filter = 2015:2016,
#                  min.filter = 500,
#                  team.filter = unique(playerchaindata$team),
#                  byseasons = T,
#                  byteams = T)