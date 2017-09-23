xG.by.game <- function(shooting){
  shooting %>% group_by(date, hteam) %>%
    summarize(ateam = ateam[1], 
              HxG = sum(xGTeam[hteam == team]),
              AxG = sum(xGTeam[ateam == team]),
              xGD = HxG - AxG,
              GD = final[1])
  
}