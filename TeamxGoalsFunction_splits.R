# # Function testing
# teamxgoals <- readRDS('IgnoreList/xGoalsByTeam.rds')
# season = 2011:2017
# game_split = 17
# even = F
# pattern = "All"

teamshootingsplits.func <- function(teamxgoals = teamxgoals, 
                            game_split = 17,
                            season = 2011:2017,
                            even = F,
                            pattern = "All"){
  
  tempdat <- teamxgoals %>%
    filter(Season %in% season) %>%
    group_by(Team, Season) %>%
    arrange(date) %>%
    mutate(gamesplayed = cumsum(date != c(-1, lag(date)[-1])),
           Pts = ifelse(is.na(Pts), 0, Pts)) %>%
    ungroup()
  
  ptsdat <- unique(tempdat %>% 
                     select(Team, Season, date, gamesplayed, Pts)) %>%
    group_by(Team, Season, split = ifelse(gamesplayed <= game_split, 1, 2)) %>%
    summarize(Pts = sum(Pts)) %>%
    ungroup()
  
  tempdat <- tempdat %>%
    filter(evengamestate %in% ifelse(rep(even, 2), c(1, 1), c(0, 1)))
  
  if(pattern != 'All'){
    tempdat <- tempdat %>% 
      filter(patternOfPlay.model == pattern)
  }

  aggdata <- tempdat %>%
    group_by(Team, Season, split = ifelse(gamesplayed <= game_split, 1, 2)) %>%
    summarize(Games = ifelse(split[1] == 1, game_split, max(gamesplayed) - game_split),
              ShtF = sum(shots)/Games,
              ShtA = sum(shotsA)/Games,
              SoTF = sum(ontarget)/Games,
              SoTA = sum(ontargetA)/Games,
              `SoT%F` = SoTF/ShtF,
              `SoT%A` = SoTA/ShtA,
              CrossPctF = sum(crossed)/sum(shots),
              CrossPctA = sum(crossedA)/sum(shotsA),
              `Solo%F` = (sum(shots) - sum(assisted))/sum(shots),
              `Solo%A` = (sum(shotsA) - sum(assistedA))/sum(shotsA),
              `Finish%F` = sum(goals)/sum(shots),
              `Finish%A` = sum(goalsA)/sum(shotsA),
              GF = sum(goals)/Games,
              GA = sum(goalsA)/Games,
              GD = (sum(goals) - sum(goalsA))/Games,
              xGF = sum(xGF)/Games,
              xGA = sum(xGA)/Games,
              xGD = xGF - xGA,
              `GD-xGD` = GD - xGD,
              TSR = sum(shots)/sum(shotsA + shots),
              PDO = 1000*(sum(goals)/sum(shots) + 1 - sum(goalsA)/sum(shotsA))) %>%
    ungroup() %>%
    left_join(ptsdat, by = c('Team', 'Season', "split")) %>%
    mutate(Pts = Pts/Games)
  
  aggdata <- aggdata %>%
    filter(split == 1) %>%
    select(-split) %>%
    left_join(aggdata %>%
                filter(split == 2) %>%
                select(-split),
              by = c("Team", "Season"),
              suffix = c(" (before split)", " (after split)"))
  
  return(aggdata)
  
}

# # Function example
# teamshootingsplits.func(teamxgoals = teamxgoals,
#                         game_split = 17,
#                         season = 2011:2017,
#                         even = F,
#                         pattern = "All")
