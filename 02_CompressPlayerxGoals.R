shooting <- readRDS('IgnoreList/AllShotsData2011-2017.rds')
library(dplyr)

shooting <- shooting %>%
  mutate(playerdiff = ifelse(team == hteam, hplayers - aplayers, aplayers - hplayers),
         evengamestate = ifelse(hscore == ascore & playerdiff == 0, 1, 0))

shooterxgoals <- shooting %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>%
  group_by(date, team, shooter, type = ifelse(patternOfPlay == 'Penalty', 'PK',
                                              ifelse(patternOfPlay == 'Free kick', 'FK', 'Other'))) %>%
  summarize(gameID = gameID[1],
            shots = n(),
            ontarget = sum(result %in% c('Goal', 'Saved', 'Blocked off line')),
            unassisted = shots - sum(assisted),
            meddist = median(distance),
            headers = sum(bodypart == 'Head'),
            goals = sum(result == 'Goal'),
            xG = sum(xGShooter),
            xG_gk = sum(xGKeeper, na.rm = T),
            `G-xG` = goals - xG)

passerxgoals <- shooting %>%
  filter(!is.na(passer)) %>%
  group_by(date, team, passer, type = ifelse(patternOfPlay == 'Penalty', 'PK',
                                             ifelse(patternOfPlay == 'Free kick', 'FK', 'Other'))) %>%
  summarize(gameID = gameID[1],
            keypasses = n(),
            ontarget.pass = sum(result %in% c('Goal', 'Saved', 'Blocked off line')),
            meddist.pass = median(distance),
            headers.pass = sum(bodypart == 'Head'),
            assists = sum(result == 'Goal'),
            xA = sum(xGShooter),
            `A-xA` = assists - xA)

playerxgoals <- shooterxgoals %>%
            full_join(passerxgoals, 
                      by = c('date', 'team', 'shooter' = 'passer', 'type'), 
                      suffix = c('_shooter', '_passer')) %>%
  ungroup() %>%
  mutate(player = shooter) %>%
  mutate_at(.funs = funs(((function(x) {ifelse(is.na(x), 0, as.numeric(x))})(.))), 
            .vars = vars(-c(shooter, type, meddist, meddist.pass, player, date, team))) %>%
  filter(!is.na(date)) %>%
  mutate(Season = as.numeric(format(date, '%Y')),
         gameID = ifelse(Season >= 2015,
                         ifelse(gameID_shooter == 0, gameID_passer, gameID_shooter), 0)) %>%
  select(-c(gameID_passer, gameID_shooter))



saveRDS(playerxgoals, file = 'IgnoreList/xGoalsByPlayer.rds')
