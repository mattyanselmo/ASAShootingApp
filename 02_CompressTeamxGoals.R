shooting <- readRDS('IgnoreList/AllShotsData2011-2017.rds')
library(dplyr)
source('xGoalByGameFunction.R')
xGByGame <- xG.by.game(shooting) %>%
  mutate(Season = format(date, '%Y')) %>%
  select(Date = date, Season, Home = hteam, HxG, Away = ateam, AxG, xGD, GD)

shooting <- shooting %>%
  mutate(playerdiff = ifelse(team == hteam, hplayers - aplayers, aplayers - hplayers),
         evengamestate = ifelse(hscore == ascore & playerdiff == 0, 1, 0))

teamxgoalsF <- shooting %>%
  group_by(Team = team, date, evengamestate, patternOfPlay.model) %>%
  summarize(home = ifelse(hteam[1] == Team[1], 1, 0),
            shots = n(),
            assisted = sum(assisted),
            ontarget = sum(result %in% c('Goal', 'Saved', 'Blocked off line')),
            goals = sum(result == 'Goal'),
            crossed = sum(cross), 
            through = sum(through),
            xGF = sum(xGTeam))

Pts <- shooting %>% 
  group_by(Team = team, date) %>% 
  summarize(Pts = ifelse(team[1] == hteam[1], 3*(final[1] > 0) + (final[1] == 0),
                         3*(final[1] < 0) + (final[1] == 0)))

teamxgoalsA <- shooting %>%
  group_by(Team = team.1, date, evengamestate, patternOfPlay.model) %>%
  summarize(home = ifelse(hteam[1] == Team[1], 1, 0),
            shotsA = n(),
            ontargetA = sum(result %in% c('Goal', 'Saved', 'Blocked off line')),
            goalsA = sum(result == 'Goal'),
            assistedA = sum(assisted),
            crossedA = sum(cross), 
            throughA = sum(through),
            xGA = sum(xGTeam))

teamxgoals <- teamxgoalsF %>%
  full_join(teamxgoalsA, by = c('Team', 'date', 'evengamestate', 'patternOfPlay.model', 'home')) %>%
  ungroup() %>%
  mutate_at(.funs = funs(((function(x) {ifelse(is.na(x), 0, as.numeric(x))})(.))), 
            .cols = vars(-c(Team, date, evengamestate, patternOfPlay.model))) %>%
  filter(!is.na(date)) %>%
  mutate(Season = as.numeric(format(date, '%Y'))) %>%
  left_join(Pts, by = c('Team', 'date'))

saveRDS(teamxgoals, file = 'IgnoreList/xGoalsByTeam.rds')
saveRDS(xGByGame, file = 'IgnoreList/xGoalsByTeam_byGame.rds')
