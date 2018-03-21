xG.by.game <- function(shooting){
  shooting %>% group_by(date, hteam) %>%
    summarize(ateam = ateam[1], 
              HG = sum((result == "Goal")[hteam == team]),
              HxGt = sum(xGTeam[hteam == team]),
              HxGp = sum(xGShooter[hteam == team]),
              AG = sum((result == "Goal")[ateam == team]),
              AxGt = sum(xGTeam[ateam == team]),
              AxGp = sum(xGShooter[ateam == team]),
              GD = final[1],
              xGDt = HxGt - AxGt,
              xGDp = HxGp - AxGp)
}