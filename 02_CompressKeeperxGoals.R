shooting <- readRDS('IgnoreList/AllShotsData2011-2017.rds')
library(dplyr)

shooting <- shooting %>%
  filter(result %in% c('Goal', 'Saved')) %>%
  mutate(playerdiff = ifelse(team == ateam, hplayers - aplayers, aplayers - hplayers),
         evengamestate = ifelse(hscore == ascore & playerdiff == 0, 1, 0))

keeperxgoals <- shooting %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>%
  group_by(date, team.1, goalie, type = ifelse(patternOfPlay == 'Penalty', 'PK',
                                              ifelse(patternOfPlay == 'Free kick', 'FK', 'Other'))) %>%
  summarize(shotsfaced = n(),
            meddist = median(distance),
            headers = sum(bodypart == 'Head'),
            goals = sum(result == 'Goal'),
            xG = sum(xGKeeper),
            `G-xG` = goals - xG) %>%
  ungroup() %>%
  mutate(Season = as.numeric(format(date, '%Y')))

saveRDS(keeperxgoals, file = 'IgnoreList/xGoalsByKeeper.rds')
