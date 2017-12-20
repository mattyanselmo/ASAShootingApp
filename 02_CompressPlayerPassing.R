# File to create shot tables for Shiny app

library(dplyr)

#load in the requisite data
shooting <- bind_rows(lapply(grep('shots with xG', list.files(), value = T), 
                             function(x) read.csv(x, stringsAsFactors = F)))
minutesPlayed <- bind_rows(lapply(grep('minutes played by game', list.files(), value = T),
                                  function(x) read.csv(x, stringsAsFactors = F)))
touches <- bind_rows(lapply(grep('touches', list.files(), value = T),
                            function(x) read.csv(x, stringsAsFactors = F)))

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
