shooting <- readRDS('IgnoreList/AllShotsData2011-2017.rds')
library(dplyr)

shooting <- shooting %>%
  mutate(playerdiff = ifelse(team == hteam, hplayers - aplayers, aplayers - hplayers),
         evengamestate = ifelse(hscore == ascore & playerdiff == 0, 1, 0))

shooterxgoals <- shooting %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>%
  group_by(date, team, shooter, type = ifelse(patternOfPlay == 'Penalty', 'PK',
                                              ifelse(patternOfPlay == 'Free kick', 'FK', 'Other'))) %>%
  summarize(shots = n(),
            ontarget = sum(result %in% c('Goal', 'Saved', 'Blocked off line')),
            unassisted = shots - sum(assisted),
            meddist = median(distance),
            headers = sum(bodypart == 'Head'),
            goals = sum(result == 'Goal'),
            xG = sum(xGShooter),
            `G-xG` = goals - xG)

passerxgoals <- shooting %>%
  filter(!is.na(passer)) %>%
  group_by(date, team, passer, type = ifelse(patternOfPlay == 'Penalty', 'PK',
                                             ifelse(patternOfPlay == 'Free kick', 'FK', 'Other'))) %>%
  summarize(keypasses = n(),
            ontarget.pass = sum(result %in% c('Goal', 'Saved', 'Blocked off line')),
            meddist.pass = median(distance),
            headers.pass = sum(bodypart == 'Head'),
            assists = sum(result == 'Goal'),
            xA = sum(xGShooter),
            `A-xA` = assists - xA)

playerxgoals <- shooterxgoals %>%
            full_join(passerxgoals, by = c('date', 'team', 'shooter' = 'passer', 'type')) %>%
  ungroup() %>%
  mutate(player = shooter) %>%
  mutate_at(.funs = funs(((function(x) {ifelse(is.na(x), 0, as.numeric(x))})(.))), .vars = vars(-c(shooter, type, meddist, meddist.pass, player, date, team))) %>%
  ungroup() %>%
  filter(!is.na(date)) %>%
  mutate(Season = as.numeric(format(date, '%Y')))

saveRDS(playerxgoals, file = 'IgnoreList/xGoalsByPlayer.rds')
