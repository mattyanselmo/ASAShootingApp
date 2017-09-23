team.xgoal.adj <- function(df, posstime){
  df %>% 
    group_by(team, date) %>%
    arrange(time) %>%
    mutate(Poss.Num = sapply(1:n(), function(x) x - sum(which(diff(time) < posstime) < x))) %>%
    group_by(team, date, Poss.Num) %>%
    mutate(xGTeam = xGTeam*(1 - prod(1 - xGTeam))/sum(xGTeam)) %>%
    ungroup()
}