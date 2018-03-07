team.xgoal.adj <- function(df, posstime){
  if(is.numeric(df$time)){
    df %>% 
      group_by(team, date) %>%
      arrange(time) %>%
      mutate(Poss.Num = sapply(1:n(), function(x) x - sum(which(diff(time) < posstime) < x))) %>%
      group_by(team, date, Poss.Num) %>%
      mutate(xGTeam = xGTeam*(1 - prod(1 - xGTeam))/sum(xGTeam)) %>%
      ungroup() 
  } else{
    df %>% 
      group_by(team, date) %>%
      mutate(time2 = sapply(strsplit(time, ':'), function(x) as.numeric(x[1]) + as.numeric(x[2])/60)) %>%
      arrange(time2) %>%
      mutate(Poss.Num = sapply(1:n(), function(x) x - sum(which(diff(time2) < posstime) < x))) %>%
      group_by(team, date, Poss.Num) %>%
      mutate(xGTeam = xGTeam*(1 - prod(1 - xGTeam))/sum(xGTeam)) %>%
      select(-time2) %>%
      ungroup()
  }
}