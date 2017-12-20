# File to create shot tables for Shiny app

library(dplyr)
teamnames <- read.csv('TeamNameLinks.csv', stringsAsFactors = F)
#load in the requisite data

if(file.exists('C:/Users/Matthias')){
  temp <- read.csv('C:/Users/Matthias/Dropbox/ASA Blog Data/2017 Stats/shots with xG.csv')
  write.csv(temp, 'C:/Users/Matthias/Documents/GitHub/ASAShootingApp_development/IgnoreList/shots with xG.csv')
  rm(temp)
  gc()
} else if(file.exists('C:/Users/Matthias.Kullowatz')){
  temp <- read.csv('C:/Users/Matthias.Kullowatz/Dropbox/ASA Blog Data/2017 Stats/shots with xG.csv')
  write.csv(temp, 'C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_development/IgnoreList/shots with xG.csv')
  rm(temp)
  gc()
}

shooting15 <- bind_rows(lapply(paste0('IgnoreList/', grep('shots with xG', list.files('IgnoreList'), value = T)), 
                               function(x) read.csv(x, stringsAsFactors = F) %>% select(-X))) %>%
  left_join(teamnames, by = c('team' = 'FullName')) %>%
  left_join(teamnames, by = c('team.1' = 'FullName')) %>%
  mutate(team = Abbr.x,
         team.1 = Abbr.y) %>%
  select(-c(Abbr.x, Abbr.y)) %>%
  left_join(teamnames, by = c('hteam' = 'FullName')) %>%
  left_join(teamnames, by = c('ateam' = 'FullName')) %>%
  mutate(hteam = Abbr.x,
         ateam = Abbr.y) %>%
  select(-c(Abbr.x, Abbr.y)) %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y'),
         year = format(date, '%Y'),
         time = sapply(strsplit(time, ':'), function(x) as.numeric(x[1]) + as.numeric(x[2])/60))

shooting14 <- bind_rows(lapply(paste0('IgnoreList/', grep('shotdata with xgoals', list.files("IgnoreList"), value = T)), 
                             function(x) read.csv(x, stringsAsFactors = F))) %>%
  left_join(teamnames, by = c('Team' = 'FullName')) %>%
  left_join(teamnames, by = c('Team.1' = 'FullName')) %>%
  mutate(team = Abbr.x,
         team.1 = Abbr.y) %>%
  select(-c(Abbr.x, Abbr.y)) %>%
  left_join(teamnames, by = c('hteam' = 'FullName')) %>%
  left_join(teamnames, by = c('ateam' = 'FullName')) %>%
  mutate(hteam = Abbr.x,
         ateam = Abbr.y) %>%
  select(-c(Abbr.x, Abbr.y)) %>%
  mutate(Date = as.Date(Date, origin = '1899-12-30'),
         year = format(Date, '%Y'),
         Time = floor(Time) + (Time - floor(Time))/0.6)

shooting14 <- shooting14 %>%
  mutate(gameID = 0) %>%
  select(date = Date, time = Time, half = Half, shooter = Shooter, gameID,
         team, goalie = Keeper, team.1, passer = Passer, assisted = Assisted,
         through = Through, cross = Cross, distance = Distance, angle = Angle,
         available = Available, keepreach = KeepReach, dive = Dive, gmlocz = GMLocZ,
         bodypart = Body.Part, result = Result, hteam, ateam, hplayers = PlayerH, 
         aplayers = PlayerA, year, hscore, ascore, final, patternOfPlay = Pattern.of.Play)

shooting <- bind_rows(shooting14,
                      shooting15 %>% select(one_of(names(shooting14)))) %>%
  mutate(patternOfPlay.model = ifelse(patternOfPlay == 'Throw in', 'Regular', patternOfPlay),
         distance = ifelse(patternOfPlay == 'Penalty', 
                           mean(distance[patternOfPlay == 'Penalty']), 
                           distance),
         available = ifelse(patternOfPlay == 'Penalty', 
                            8, 
                            available))

## Need to correct scores in 2015 - 2017, see example of teams with negative goals scored

xgoal.model <- glm(result == 'Goal' ~ 
                     I(log(distance)) +
                     patternOfPlay.model +
                     available +
                     I((available - mean(available))^2) +
                     I(bodypart == 'Head') +
                     through +
                     cross,
                   data = shooting %>% 
                     filter(year <= 2016),
                   family = binomial)

xgoal.model.keeper <- glm(result == 'Goal' ~
                            I(patternOfPlay == 'Penalty'):(keepreach +
                                                             gmlocz +
                                                             I((gmlocz - 3.5)^2))+
                            I(log(distance)) +
                            I(bodypart == 'Head') +
                            cross +
                            through +
                            patternOfPlay.model,
                          data = shooting %>%
                            filter(year <= 2016,
                                   result == 'Goal' | result == 'Saved'),
                          family = binomial)

# Tested interaction between free kicks and distance: worse fit on holdout 2015 - 2017

shooting[['xGShooter']] <- predict(xgoal.model, shooting, type = 'response')
shooting[['xGTeam']] <- predict(xgoal.model, shooting %>%
                                  mutate(patternOfPlay.model = ifelse(patternOfPlay.model == 'Penalty', 
                                                                      'Regular', patternOfPlay.model)), 
                                type = 'response')
shooting[['xGKeeper']][shooting$result %in% c('Goal', 'Saved')] <- predict(xgoal.model.keeper, 
                                                                           shooting[shooting$result %in% c('Goal', 'Saved'),], 
                                                                           type = 'response')

source('TeamxGoalAdjustmentFunction.R')
shooting <- team.xgoal.adj(shooting, 5/60)

saveRDS(shooting, 'IgnoreList/AllShotsData2011-2017.rds')

# library(xtable)
# model1.coef <- summary(xgoal.model)$coef
# save(xgoal.model, xgoal.model.keeper, file = paste0('UpdatedModels_', Sys.Date()))
