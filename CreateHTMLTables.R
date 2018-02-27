library(dplyr)
library(xtable)

# Player xGoals ####
dt_xgoals <- lapply(2011:max(playerxgoals$Season),
                    FUN = function(x){
                      dt_total <- shooterxgoals.func(playerxgoals,
                                                     date1 = as.Date('2000-01-01'),
                                                     date2 = as.Date('9999-12-31'),
                                                     season = x,
                                                     minfilter = 0,
                                                     shotfilter = 0,
                                                     keyfilter = 0,
                                                     byteams = F,
                                                     byseasons = T,
                                                     OtherShots = T,
                                                     FK = T,
                                                     PK = T) %>% 
                        select(-c(Dist, Dist.key)) %>%
                        mutate(`G+A` = Goals + Assts) %>%
                        rename(Sht = Shots,G = Goals, KP = KeyP, A = Assts) %>%
                        select(Player:xG, `G-xG`:`xG+xA`, xPlace)
                      
                      dt_per96 <- shooterxgoals_perminute(playerxgoals,
                                                          minutes_df = minutesPlayed,
                                                          date1 = as.Date('2000-01-01'),
                                                          date2 = as.Date('9999-12-31'),
                                                          season = x,
                                                          shotfilter = 0,
                                                          keyfilter = 0,
                                                          minfilter = 0,
                                                          byseasons = T,
                                                          byteams = F,
                                                          OtherShots = T,
                                                          FK = T,
                                                          PK = T) %>%
                        mutate(`G+A` = Goals + Assts) %>%
                        rename(Sht = Shots,G = Goals, KP = KeyP, A = Assts) %>%
                        select(Player:xG, `G-xG`:`xG+xA`, xPlace) 
                      
                      namesFL <- as.data.frame(do.call("rbind", strsplit(sub(" ", ";", dt_total$Player), ";"))) %>%
                        mutate_all(.funs = as.character)
                      names(namesFL) <- c("First", "Last")
                      namesFL <- namesFL %>%
                        mutate(First = ifelse(First == Last, "", First))
                      
                      if(x >= 2015){
                        output <- data.frame(namesFL, dt_total %>% 
                                               left_join(dt_per96 %>%
                                                           select(-Team),
                                                         by = c("Player", "Season", "Min"),
                                                         suffix = c("", "p96")) %>%
                                               ungroup() %>%
                                               select(-Player), check.names = F)
                        output <- xtable(output, 
                                         digits = c(0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 2, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2))
                        write.table(print.xtable(output, 
                                                 type = "html",
                                                 include.rownames = F,
                                                 print.results = F), 
                                    file = paste0("C:/Users/Matthias.Kullowatz/Dropbox/ASA Blog Data/HTMLOutputs/Player_xGoals_", x, ".txt"),
                                    row.names = F,
                                    quote = F)
                      } else{
                        output <- data.frame(namesFL, dt_total %>%
                                               ungroup() %>%
                                               select(-Player, -Min), check.names = F)
                        
                        output <- xtable(output, 
                                         digits = c(0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 2, 0, 0, 2, 2, 2, 2))
                        write.table(print.xtable(output, 
                                                 type = "html",
                                                 include.rownames = F,
                                                 print.results = F),
                                    file = paste0("C:/Users/Matthias.Kullowatz/Dropbox/ASA Blog Data/HTMLOutputs/Player_xGoals_", x, ".txt"),
                                    row.names = F,
                                    quote = F)
                      }
                      
                    })


# Team xGoals ####
dt_xG_team <- lapply(2011:max(teamxgoals$Season),
                     FUN = function(x){
                       dt_total_bas <- teamxgoals.func(teamxgoals = teamxgoals,
                                                       date1 = as.Date('2000-01-01'),
                                                       date2 = as.Date('9999-12-31'),
                                                       season = x,
                                                       even = F,
                                                       pattern = 'All',
                                                       pergame = F,
                                                       advanced = F,
                                                       venue = c('Home', 'Away'),
                                                       byseasons = T,
                                                       plot = F) %>%
                         select(Team, Games, )
                       dt_total_adv <- teamxgoals.func(teamxgoals = teamxgoals,
                                       date1 = as.Date('2000-01-01'),
                                       date2 = as.Date('9999-12-31'),
                                       season = x,
                                       even = F,
                                       pattern = 'All',
                                       pergame = F,
                                       advanced = T,
                                       venue = c('Home', 'Away'),
                                       byseasons = T,
                                       plot = F) %>%
                         select(Team, Games, )
                       
                       dt_total <- teamxgoals.func(teamxgoals = teamxgoals,
                                                   date1 = as.Date('2000-01-01'),
                                                   date2 = as.Date('9999-12-31'),
                                                   season = x,
                                                   even = F,
                                                   pattern = 'All',
                                                   pergame = T,
                                                   advanced = T,
                                                   venue = c('Home', 'Away'),
                                                   byseasons = T,
                                                   plot = F)
                     })
