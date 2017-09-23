# File to create shot tables for Shiny app

library(dplyr)

#load in the requisite data
shooting <- bind_rows(lapply(grep('shots with xG', list.files(), value = T), 
                             function(x) read.csv(x, stringsAsFactors = F)))
minutesPlayed <- bind_rows(lapply(grep('minutes played by game', list.files(), value = T),
                                  function(x) read.csv(x, stringsAsFactors = F)))
touches <- bind_rows(lapply(grep('touches', list.files(), value = T),
                            function(x) read.csv(x, stringsAsFactors = F)))

# All shots ####
alldata <- shooting %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>%
  group_by(gameID, shooter) %>%
  summarize(date = date[1],
            Team = team[1],
            shots = n(),
            unassisted = shots - sum(assisted),
            meddist = median(distance),
            headers = sum(bodypart == 'Head'),
            goals = sum(result == 'Goal'),
            xG = sum(xGvalueP),
            GminusXG = goals - xG) %>%
  full_join(shooting %>%
              group_by(gameID, passer) %>%
              summarize(keypasses = n(),
                        meddist.pass = median(distance),
                        headers.pass = sum(bodypart == 'Head'),
                        assists = sum(result == 'Goal'),
                        xA = sum(xGvalueP),
                        AminusXA = assists - xA),
            by = c('gameID', 'shooter' = 'passer')) %>%
  mutate(player = shooter) %>%
  full_join(minutesPlayed, 
            by = c('gameID', 'player')) %>%
  full_join(touches %>%
              select(gameID, player, touches), 
            by = c('gameID', 'player')) %>%
  mutate_each(funs(((function(x) {ifelse(is.na(x), 0, as.numeric(x))})(.))), -c(gameID:Team, meddist, meddist.pass, player, X)) %>%
  ungroup() %>%
  group_by(gameID) %>%
  mutate(date = as.Date(ifelse(is.na(date), date[!is.na(date)][1], date), origin = '1970-01-01')) %>%
  ungroup()

# No penalties ####
nopkdata <- shooting %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>%
  filter(patternOfPlay != 'Penalty') %>%
  group_by(gameID, shooter) %>%
  summarize(date = date[1],
            Team = team[1],
            shots = n(),
            unassisted = shots - sum(assisted),
            meddist = median(distance),
            headers = sum(bodypart == 'Head'),
            goals = sum(result == 'Goal'),
            xG = sum(xGvalueP),
            GminusXG = goals - xG) %>%
  full_join(shooting %>%
              group_by(gameID, passer) %>%
              summarize(keypasses = n(),
                        meddist.pass = median(distance),
                        headers.pass = sum(bodypart == 'Head'),
                        assists = sum(result == 'Goal'),
                        xA = sum(xGvalueP),
                        AminusXA = assists - xA),
            by = c('gameID', 'shooter' = 'passer')) %>%
  mutate(player = shooter) %>%
  full_join(minutesPlayed, 
            by = c('gameID', 'player')) %>%
  full_join(touches %>%
              select(gameID, player, touches), 
            by = c('gameID', 'player')) %>%
  mutate_each(funs(((function(x) {ifelse(is.na(x), 0, as.numeric(x))})(.))), -c(gameID:Team, meddist, meddist.pass, player, X)) %>%
  ungroup() %>%
  group_by(gameID) %>%
  mutate(date = as.Date(ifelse(is.na(date), date[!is.na(date)][1], date), origin = '1970-01-01')) %>%
  ungroup()

# No Free kicks ####
nofkdata <- shooting %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>%
  filter(patternOfPlay != 'Free kick') %>%
  group_by(gameID, shooter) %>%
  summarize(date = date[1],
            Team = team[1],
            shots = n(),
            unassisted = shots - sum(assisted),
            meddist = median(distance),
            headers = sum(bodypart == 'Head'),
            goals = sum(result == 'Goal'),
            xG = sum(xGvalueP),
            GminusXG = goals - xG) %>%
  full_join(shooting %>%
              group_by(gameID, passer) %>%
              summarize(keypasses = n(),
                        meddist.pass = median(distance),
                        headers.pass = sum(bodypart == 'Head'),
                        assists = sum(result == 'Goal'),
                        xA = sum(xGvalueP),
                        AminusXA = assists - xA),
            by = c('gameID', 'shooter' = 'passer')) %>%
  mutate(player = shooter) %>%
  full_join(minutesPlayed, 
            by = c('gameID', 'player')) %>%
  full_join(touches %>%
              select(gameID, player, touches), 
            by = c('gameID', 'player')) %>%
  mutate_each(funs(((function(x) {ifelse(is.na(x), 0, as.numeric(x))})(.))), -c(gameID:Team, meddist, meddist.pass, player, X)) %>%
  ungroup() %>%
  group_by(gameID) %>%
  mutate(date = as.Date(ifelse(is.na(date), date[!is.na(date)][1], date), origin = '1970-01-01')) %>%
  ungroup()

# No penalties or Free kicks ####
nofkpkdata <- shooting %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>%
  filter(patternOfPlay != 'Free kick' & patternOfPlay != 'Penalty') %>%
  group_by(gameID, shooter) %>%
  summarize(date = date[1],
            Team = team[1],
            shots = n(),
            unassisted = shots - sum(assisted),
            meddist = median(distance),
            headers = sum(bodypart == 'Head'),
            goals = sum(result == 'Goal'),
            xG = sum(xGvalueP),
            GminusXG = goals - xG) %>%
  full_join(shooting %>%
              group_by(gameID, passer) %>%
              summarize(keypasses = n(),
                        meddist.pass = median(distance),
                        headers.pass = sum(bodypart == 'Head'),
                        assists = sum(result == 'Goal'),
                        xA = sum(xGvalueP),
                        AminusXA = assists - xA),
            by = c('gameID', 'shooter' = 'passer')) %>%
  mutate(player = shooter) %>%
  full_join(minutesPlayed, 
            by = c('gameID', 'player')) %>%
  full_join(touches %>%
              select(gameID, player, touches), 
            by = c('gameID', 'player')) %>%
  mutate_each(funs(((function(x) {ifelse(is.na(x), 0, as.numeric(x))})(.))), -c(gameID:Team, meddist, meddist.pass, player, X)) %>%
  ungroup() %>%
  group_by(gameID) %>%
  mutate(date = as.Date(ifelse(is.na(date), date[!is.na(date)][1], date), origin = '1970-01-01')) %>%
  ungroup()

write.csv(alldata, 'AllShootingData.csv')
write.csv(nopkdata, 'NoPKData.csv')
write.csv(nofkdata, 'NoFKData.csv')
write.csv(nofkpkdata, 'NoFKPKData.csv')


#now write our total stats out
write.csv(withMinutes,'player xg.csv')
##########################################################################

# now handle goalie charts ####################
#we've already got player minutes

saves <- group_by(shooting,goalie) %>%
  summarise(saves = sum(result == 'Saved'),goals = sum(result == 'Goal'), sog = saves + goals, xGA = sum(xGvalueGK), gMinusxG = goals - xGA)

totalGKStats <- merge(saves,playerMin,by.x=c('goalie'),by.y=c('player'))

write.csv(totalGKStats,'goalie xg.csv')

#########################################################

# Now get passing ratio numbers for each team
#passes <- data.frame(read.csv('raw passes.csv'))
#passPercFor <- group_by(passes, team) %>%
#   summarize(finalThirdPer = sum((x > 66.7 | endX > 66.7) & success == 1)/sum(success == 1))

#passPercAgainst <- group_by(passes, team.1) %>%
#    summarize(finalThirdAPer = sum((x > 66.7 | endX > 66.7) & success == 1)/sum(success == 1))


#write.csv(passPerc, 'passing_percentages.csv')x


# Now handle the team pages
teamStatsFor <- group_by(shooting, team) %>%
  summarise(gp = length(unique(gameID)), xGF = sum(xGvalueP), gf = sum(result == 'Goal'))

teamStatsAgainst <- group_by(shooting, team.1) %>%
  summarise(xGA = sum(xGvalueP), ga = sum(result == 'Goal'))

teamStats <- merge(teamStatsFor, teamStatsAgainst, by.x = 'team', by.y = 'team.1')

teamStats <- mutate(teamStats, xGD = xGF - xGA, gd = gf - ga, gdMinusxGD = gd - xGD)[, c('team','gp','xGF','xGA','xGD','gf','ga','gd','gdMinusxGD')]

write.csv(teamStats, 'team_table.csv')

# Now handle the game xG pages

homeStats <- group_by(shooting, gameID) %>%
  summarise(hg = sum(ifelse(team == hteam, result == 'Goal', 0)), hxg = sum(ifelse(team == hteam, xGvalueP, 0)))

awayStats <- group_by(shooting, gameID) %>%
  summarise(ag = sum(ifelse(team == ateam, result == 'Goal', 0)), axg = sum(ifelse(team == ateam, xGvalueP, 0)))

stats <- merge(homeStats, awayStats, by.x = 'gameID', by.y = 'gameID')
statsWithTeams <- unique(merge(stats, shooting, by.x = 'gameID', by.y = 'gameID')[, c('date', 'hteam', 'hg', 'hxg', 'ateam', 'ag', 'axg')])

statsWithTeams$xGD <- statsWithTeams$hxg - statsWithTeams$axg
write.csv(statsWithTeams, 'by_game_xg.csv')
