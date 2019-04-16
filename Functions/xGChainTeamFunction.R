# Summarize team chain data across games and seasons

library(dplyr)
teamchaindata.game <- readRDS("AppData/TeamxGChainData.rds")

teamchains.func <- function(teamchaindata = teamchaindata.game, 
                            date1 = as.Date('2000-01-01'), 
                            date2 = as.Date('9999-12-31'),
                            season = 2011:2017,
                            even = F,
                            pattern = unique(teamxgoals$patternOfPlay.model),
                            pergame = F,
                            advanced = F,
                            venue = c('Home', 'Away'),
                            byseasons = T, 
                            confview = T,
                            plot = F){
  
  
}

DO STUFF!
  
%>%
  group_by(team) %>%
  summarize(Games = length(unique(gameID)),
            ChainsF = sum(num.chains_f),
            ChainsA = sum(num.chains_a))
chains.game = Chains/Games,
passes.chain = sum(num.passes)/Chains,
dribbles.chain = sum(num.dribbles)/Chains,
counters.game = sum(num.counters)/Games,
width.chain = sum(total.width)/Chains,
width.median = mean(med.width),
start.def.pct = sum(start.def)/Chains,
start.mid.pct = sum(start.mid)/Chains,
start.att.pct = sum(start.att)/Chains,
avg.xStart = sum(num.chains*avg.xStart)/sum(num.chains),
med.xStart = sum(num.chains*med.xStart)/sum(num.chains)
) %>%
  data.frame()