# File to create shot tables for Shiny app

library(dplyr)
library(stringr)
teamnames <- read.csv('TeamNameLinks.csv', stringsAsFactors = F)
#load in the requisite data

if(file.exists('C:/Users/Matthias')){
  temp <- read.csv(paste0("C:/Users/Matthias/Dropbox/ASA Blog Data/", year, " Stats/shots with xG.csv"))
  write.csv(temp, paste0("C:/Users/Matthias/Documents/GitHub/ASAShootingApp_development/IgnoreList/shots with xG ", year, ".csv"))
  write.csv(temp, paste0("C:/Users/Matthias/Documents/GitHub/ASAShootingApp_master/IgnoreList/shots with xG ", year, ".csv"))
  rm(temp)
  gc()
} else if(file.exists('C:/Users/Matthias.Kullowatz')){
  temp <- read.csv(paste0("C:/Users/Matthias.Kullowatz/Dropbox/ASA Blog Data/", year, " Stats/shots with xG.csv"))
  write.csv(temp, paste0("C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_development/IgnoreList/shots with xG ", year, ".csv"))
  write.csv(temp, paste0("C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_master/IgnoreList/shots with xG ", year, ".csv"))
  rm(temp)
  gc()
}

shooting15 <- bind_rows(lapply(paste0('IgnoreList/', grep('shots with xG', list.files('IgnoreList'), value = T)), 
                               function(x) read.csv(x, stringsAsFactors = F) %>% select(-one_of("X")))) %>%
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
  mutate(date = as.Date(date, format = ifelse(row_number() %in% grep("/", date), "%m/%d/%Y", "%Y-%m-%d")),
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
                           12, 
                           distance),
         available = ifelse(patternOfPlay == 'Penalty', 
                            8, 
                            ifelse(is.na(available),
                                   0, 
                                   available)))

xgoal.model <- glm(result == 'Goal' ~ 
                     patternOfPlay.model +
                     as.factor(year) +
                     I(log(distance)) +
                     available +
                     I((available - mean(available, na.rm = T))^2) +
                     I(bodypart == 'Head') +
                     through +
                     cross,
                   data = shooting %>% 
                     filter(as.numeric(as.character(year)) <= 2017),
                   family = binomial)

xgoal.model.keeper <- glm(result == 'Goal' ~
                            I(patternOfPlay == 'Penalty'):(keepreach +
                                                             gmlocz +
                                                             I((gmlocz - 3.5)^2))+
                            I(log(distance)) +
                            I(bodypart == 'Head') +
                            cross +
                            through +
                            patternOfPlay.model +
                            as.factor(year),
                          data = shooting %>%
                            filter(year <= 2017,
                                   result == 'Goal' | result == 'Saved'),
                          family = binomial)

# Save once per year...
# save(xgoal.model, xgoal.model.keeper, file = paste0('IgnoreList/UpdatedModels_', Sys.Date(), '.Rdata'))
     
# Note: Tested interaction between free kicks and distance: worse fit on holdout 2015 - 2017

shooting[['xGShooter']] <- predict(xgoal.model, 
                                   shooting %>%
                                     mutate(year = ifelse(as.numeric(year) == max(as.numeric(year)), 
                                                          as.character(max(as.numeric(year)) - 1), 
                                                          year)), 
                                   type = 'response')
shooting[['xGTeam']] <- predict(xgoal.model, shooting %>%
                                  mutate(patternOfPlay.model = ifelse(patternOfPlay.model == 'Penalty', 
                                                                      'Regular', patternOfPlay.model),
                                         year = ifelse(as.numeric(year) == max(as.numeric(year)), 
                                                       as.character(max(as.numeric(year)) - 1), 
                                                       year)), 
                                type = 'response')
shooting[['xGKeeper']][shooting$result %in% c('Goal', 'Saved')] <- predict(xgoal.model.keeper, 
                                                                           shooting[shooting$result %in% c('Goal', 'Saved'),] %>%
                                                                             mutate(year = ifelse(as.numeric(year) == max(as.numeric(year)), 
                                                                                                  as.character(max(as.numeric(year)) - 1), 
                                                                                                  year)), 
                                                                           type = 'response')
source('TeamxGoalAdjustmentFunction.R')
shooting <- team.xgoal.adj(shooting, 5/60)

shooting <- shooting %>%
  mutate(shooter = str_replace_all(shooter, 
                                   c('Kazaishvili' = 'Qazaishvili', 
                                     'Jorge Villafaña' = 'Jorge Villafana',
                                     "Antonio Mlinar Dalamea" = "Antonio Mlinar Delamea")),
         shooter = ifelse(row_number() %in% grep("Boniek", shooter), "Oscar Boniek Garcia", 
                          ifelse(shooter == "Eddie Johnson" & year == 2011, "Eddie Johnson (no, not that one)", shooter)),
         passer = str_replace_all(passer, 
                                  c('Kazaishvili' = 'Qazaishvili', 
                                    'Jorge Villafaña' = 'Jorge Villafana',
                                    "Antonio Mlinar Dalamea" = "Antonio Mlinar Delamea")),
         passer = ifelse(row_number() %in% grep("Boniek", passer), "Oscar Boniek Garcia", 
                         ifelse(passer == "Eddie Johnson" & year == 2011, "Eddie Johnson (no, not that one)", passer)))
         
"Boniek Garcia" = "Oscar Boniek Garcia"
saveRDS(shooting, 'IgnoreList/AllShotsData2011-2017.rds')

# library(xtable)
# model1.coef <- summary(xgoal.model)$coef
# save(xgoal.model, xgoal.model.keeper, file = paste0('UpdatedModels_', Sys.Date()))
