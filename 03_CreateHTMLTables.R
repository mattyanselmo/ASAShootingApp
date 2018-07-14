library(dplyr)
library(xtable)
library(formattable)

path <- ifelse(file.exists("C:/Users/Matthias"), "C:/Users/Matthias", "C:/Users/Matthias.Kullowatz")

playerxgoals <- readRDS('IgnoreList/xGoalsByPlayer.rds')
minutesPlayed <- readRDS('IgnoreList/MinutesByGameID_forapp.rds')
teamxgoals <- readRDS('IgnoreList/xGoalsByTeam.rds')
xgbygame <- readRDS('IgnoreList/xGoalsByTeam_byGame.rds')
keeperxgoals <- readRDS('IgnoreList/xGoalsByKeeper.rds')
conferences <- read.csv('teamsbyconferencebyseason.csv')
teamlinks <- read.csv("TeamNameLinks_reverse.csv")
glossary <- read.csv('Glossary.csv')

playerpassing <- readRDS("IgnoreList/xPassingByPlayer.rds")
teampassing.offense <- readRDS("IgnoreList/xPassingByTeamOffense.rds")
teampassing.defense <- readRDS("IgnoreList/xPassingByTeamDefense.rds")

source('ShooterxGoalsFunction.R')
source('ShooterxGoalsFunction_perminute.R')
source('TeamxGoalsFunction.R')
source('KeeperxGoalsFunction.R')
source("PasserxPassesFunction.R")
source("TeamxPassesFunction.R")
source("xGoalByGameFunction.R")

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

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
                        select(Player:xG, `G-xG`:`xG+xA`, xPlace) %>%
                        select(-Season)
                      
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
                        select(Player:xG, `G-xG`:`xG+xA`, xPlace) %>%
                        select(-Season)
                      
                      namesFL <- as.data.frame(do.call("rbind", strsplit(sub(" ", ";", dt_total$Player), ";"))) %>%
                        mutate_all(.funs = as.character)
                      names(namesFL) <- c("First", "Last")
                      namesFL <- namesFL %>%
                        mutate(First = ifelse(First == Last, "", First))
                      
                      if(x >= 2015){
                        output <- data.frame(namesFL, dt_total %>% 
                                               left_join(dt_per96 %>%
                                                           select(-Team, -Pos),
                                                         by = c("Player", "Min"),
                                                         suffix = c("", "p96")) %>%
                                               ungroup() %>%
                                               select(-Player), check.names = F)
                        write.csv(output, paste0(path, "/Dropbox/ASA Blog Data/HTMLOutputs/Player_xGoals_", x, ".csv"))
                        output <- xtable(output, 
                                         digits = c(0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 2, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
                                         align = rep("c", ncol(output) + 1))
                        write.table(gsub("\n", "",
                                         gsub(" <", "<", 
                                              gsub("> ", ">", 
                                                   gsub(' align="center"', "",
                                                        gsub("<table border=1>", '<script>
  $(document).ready(function() {
                                                             $("#myTable").tablesorter();
                      });
                                                             </script><TABLE border=1 id="myTable" class="tablesorter" style="white-space:nowrap;"><thead>',
                                                             gsub(" xPlacep96 </th>  </tr>", " xPlacep96 </th>  </tr> </thead> <tbody>",
                                                                  print.xtable(output, 
                                                                               type = "html",
                                                                               include.rownames = F,
                                                                               print.results = F))))))), 
                                    file = paste0(path, "/Dropbox/ASA Blog Data/HTMLOutputs/Player_xGoals_", x, ".txt"),
                                    row.names = F,
                                    quote = F)
                      } else{
                        output <- data.frame(namesFL, dt_total %>%
                                               ungroup() %>%
                                               select(-Player), check.names = F)
                        write.csv(output, paste0(path, "/Dropbox/ASA Blog Data/HTMLOutputs/Player_xGoals_", x, ".csv"))
                        output <- xtable(output, 
                                         digits = c(0, 0, 0, 0, 0, 0, 2, 0, 2, 2, 0, 0, 2, 2, 2, 2),
                                         align = rep("c", ncol(output) + 1))
                        write.table(gsub("\n", "",
                                         gsub(" <", "<", 
                                              gsub("> ", ">", 
                                                        gsub("<table border=1>", '<script>
  $(document).ready(function() {
                                                             $("#myTable").tablesorter();
                      });
                                                             </script><TABLE border=1 id="myTable" class="tablesorter" style="white-space:nowrap;"><thead>',
                                                             gsub(" xPlacep96 </th>  </tr>", " xPlacep96 </th>  </tr> </thead> <tbody>",
                                                                  print.xtable(output, 
                                                                               type = "html",
                                                                               include.rownames = F,
                                                                               print.results = F)))))),
                                    file = paste0(path, "/Dropbox/ASA Blog Data/HTMLOutputs/Player_xGoals_", x, ".txt"),
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
                         select(Team, GP = Games, ShtF, ShtA, GF, GA, GD, Pts)
                       
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
                         select(Team, xGF, xGA, xGD, TSR, PDO, Conf)
                       
                       dt_per96_bas <- teamxgoals.func(teamxgoals = teamxgoals,
                                                       date1 = as.Date('2000-01-01'),
                                                       date2 = as.Date('9999-12-31'),
                                                       season = x,
                                                       even = F,
                                                       pattern = 'All',
                                                       pergame = T,
                                                       advanced = F,
                                                       venue = c('Home', 'Away'),
                                                       byseasons = T,
                                                       plot = F) %>%
                         select(Team, ShtF, ShtA, GF, GA, GD)
                       
                       dt_per96_adv <- teamxgoals.func(teamxgoals = teamxgoals,
                                                       date1 = as.Date('2000-01-01'),
                                                       date2 = as.Date('9999-12-31'),
                                                       season = x,
                                                       even = F,
                                                       pattern = 'All',
                                                       pergame = T,
                                                       advanced = T,
                                                       venue = c('Home', 'Away'),
                                                       byseasons = T,
                                                       plot = F) %>%
                         select(Team, xGF, xGA, xGD)
                       
                       output <- dt_total_bas %>%
                         left_join(dt_total_adv, "Team") %>%
                         left_join(dt_per96_bas, "Team", suffix = c("", "/g")) %>%
                         left_join(dt_per96_adv, "Team", suffix = c("", "/g")) %>%
                         ungroup() %>%
                         mutate(`GD-xGD` = GD - xGD) %>%
                         select(Team, Conf, GP:GD, xGF:xGD, `GD-xGD`, `ShtF/g`:`xGD/g`) %>%
                         left_join(teamlinks, by = c("Team" = "Abbr")) %>%
                         mutate(Team = FullName,
                                Conf = ifelse(Conf == "east", "Eastern", "Western")) %>%
                         select(-FullName) %>% 
                         arrange(desc(xGD))
                       write.csv(output, paste0(path, "/Dropbox/ASA Blog Data/HTMLOutputs/Team_xGoals_", x, ".csv"))
                       output <- xtable(output, 
                                        digits = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
                                        align = rep("center", ncol(output) + 1))
                       write.table(gsub("\n", "",
                                        gsub(" <", "<", 
                                             gsub("> ", ">", 
                                                  gsub("<table border=1>", '<script>
  $(document).ready(function() {
                                                             $("#myTable").tablesorter();
                      });
                                                             </script><TABLE border=1 id="myTable" class="tablesorter" style="white-space:nowrap;"><thead>',
                                                       gsub(" xGD/g </th>  </tr>", " xGD/g </th>  </tr> </thead> <tbody>",
                                                            print.xtable(output, 
                                                                         type = "html",
                                                                         include.rownames = F,
                                                                         print.results = F)))))),
                                   file = paste0(path, "/Dropbox/ASA Blog Data/HTMLOutputs/Team_xGoals_", x, ".txt"),
                                   row.names = F,
                                   quote = F)
                     })

# Player xPassing ####

lapply(2015:max(playerpassing$year),
       FUN = function(x){
         options(scipen = 100)
         all <- passer.xpasses(playerpassing,
                               minpasses = 0,
                               seasonfilter = x,
                               minfilter = 0,
                               byteams = F,
                               byseasons = T,
                               third.filter = "All") %>%
           mutate(Per100 = Per100/100) %>%
           rename(`%` = PassPct, `Exp%` = xPassPct, `%Diff` = Per100)
         
         att <- passer.xpasses(playerpassing,
                               minpasses = 0,
                               seasonfilter = x,
                               minfilter = 0,
                               byteams = F,
                               byseasons = T,
                               third.filter = "Att") %>%
           mutate(Per100 = Per100/100) %>%
           rename(`%` = PassPct, `Exp%` = xPassPct, `%Diff` = Per100)
         
         mid <- passer.xpasses(playerpassing,
                               minpasses = 0,
                               seasonfilter = x,
                               minfilter = 0,
                               byteams = F,
                               byseasons = T,
                               third.filter = "Mid") %>%
           mutate(Per100 = Per100/100) %>%
           rename(`%` = PassPct, `Exp%` = xPassPct, `%Diff` = Per100)
         
         def <- passer.xpasses(playerpassing,
                               minpasses = 0,
                               seasonfilter = x,
                               minfilter = 0,
                               byteams = F,
                               byseasons = T,
                               third.filter = "Def") %>%
           mutate(Per100 = Per100/100) %>%
           rename(`%` = PassPct, `Exp%` = xPassPct, `%Diff` = Per100)
         
         dt <- all %>%
           select(-c(Season, Score, Distance)) %>%
           select(Player:Pos, `Touch%`, Passes:Vertical) %>%
           full_join(att %>%
                       select(-c(Season, Team, Min, Pos, Score, Distance)), 
                     by = c("Player"), 
                     suffix = c("_all", "_att")) %>%
           full_join(mid %>%
                       select(-c(Season, Team, Min, Pos, Score, Distance)), 
                     by = c("Player")) %>%
           full_join(def %>%
                       select(-c(Season, Team, Min, Pos, Score, Distance)), 
                     by = c("Player"), 
                     suffix = c("_mid", "_def"))
         dt <- dt %>%
           mutate_at(.vars = setdiff(names(dt), c("Player", "Team", "Pos")),
                     .funs = function(x) ifelse(is.na(x), 0, x)) %>%
           mutate_at(.vars = grep("%", names(dt), value = T),
                     .funs = function(x) as.character(percent(x, digits = 1))) %>%
           arrange(desc(`%Diff_all`))
         
         
         namesFL <- as.data.frame(do.call("rbind", strsplit(sub(" ", ";", dt$Player), ";")))
         names(namesFL) <- c("First", "Last")
         
         dt <- data.frame(namesFL, dt %>% select(-Player), check.names = F)
         write.csv(dt, paste0(path, "/Dropbox/ASA Blog Data/HTMLOutputs/Player_xPasses_", x, ".csv"))
         output <- print.xtable(xtable(dt, 
                                       digits = c(0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 2, 0, 3, 3, 3, 2, 0, 3, 3, 3, 2, 0, 3, 3, 3, 2)),
                                type = "html",
                                include.rownames = F,
                                print.results = F)
         # replace column tags and include script
         write.table(gsub("\n", "",
                          gsub(" <", "<", 
                               gsub("> ", ">", 
                                    gsub(' align="right"', "",
                                         gsub("</table>", "</tbody> </table>",
                                              gsub("Vertical </th>  </tr>", "Vertical </th> </tr> </thead> <tbody>",
                                                   gsub("_all|_def|_mid|_att", "",
                                                        gsub("<table border=1>", '<script>
$(document).ready(function() {
$("#myTable").tablesorter();
  });
</script>
<TABLE border=1 id="myTable" class="tablesorter" style="white-space:nowrap;"><thead>
<TR><TH colspan="6" class="sorter-false"></TH><TH colspan="5" class="sorter-false">All Passes</TH><TH colspan="5" class="sorter-false">Attacking Third</TH><TH colspan="5" class="sorter-false">Middle Third</TH><TH colspan="5" class="sorter-false">Defensive Third</TH></TR>
',
                                                             output)))))))),
                     file = paste0(path, "/Dropbox/ASA Blog Data/HTMLOutputs/Player_xPasses_", x, ".txt"),
                     row.names = F,
                     quote = F)
       })

# Keeper xGoals ####
# First	Last	Team	Mins	Saves	GA	SOG	xGA	GA-xGA	GA-xGAp96
lapply(2011:max(keeperxgoals$Season),
       FUN = function(x){
         dt <- keeperxgoals.func(keeperxgoals %>%
                                   mutate(date = as.Date(date, format = '%m/%d/%Y')),
                                 minutes_df = minutesPlayed,
                                 date1 = as.Date('2000-01-01'),
                                 date2 = as.Date('9999-12-31'),
                                 season = x,
                                 shotfilter = 0,
                                 byteams = F,
                                 byseasons = T,
                                 OtherShots = T,
                                 FK = T,
                                 PK = T) %>%
           select(-Season) %>%
           select_(.dots = c("Keeper", "Team", "Min", "Saves", "Goals", "Shots", "`Header%`", "Dist", "xG", "`G-xG`")[c(T, T, x >= 2015, rep(T, 7))]) %>%
           rename(SOG = Shots, GA = Goals, xGA = xG, `GA-xGA` = `G-xG`) %>%
           ungroup()
         
         namesFL <- as.data.frame(do.call("rbind", strsplit(sub(" ", ";", dt$Keeper), ";")))
         names(namesFL) <- c("First", "Last")
         dt <- data.frame(namesFL, dt %>% select(-Keeper), check.names = F)
         write.csv(dt, paste0(path, "/Dropbox/ASA Blog Data/HTMLOutputs/Keeper_xGoals_", x, ".csv"))
         
         if(x >= 2015){
         output <- xtable(dt, 
                          digits = c(0, 0, 0, 0, 0, 0, 0, 0, 3, 1, 2, 2),
                          align = rep("c", ncol(dt) + 1))
         } else{
         output <- xtable(dt, 
                          digits = c(0, 0, 0, 0, 0, 0, 0, 3, 1, 2, 2),
                          align = rep("c", ncol(dt) + 1))
         }
         write.table(gsub("\n", "",
                          gsub(" <", "<", 
                               gsub("> ", ">", 
                                    gsub("<table border=1>", '<script>
  $(document).ready(function() {
                                         $("#myTable").tablesorter();
       });
                                         </script><TABLE border=1 id="myTable" class="tablesorter" style="white-space:nowrap;"><thead>',
                                         gsub(" GA-xGA </th>  </tr>", " GA-xGA </th>  </tr> </thead> <tbody>",
                                              print.xtable(output, 
                                                           type = "html",
                                                           include.rownames = F,
                                                           print.results = F)))))),
                     file = paste0(path, "/Dropbox/ASA Blog Data/HTMLOutputs/Keeper_xGoals_", x, ".txt"),
                     row.names = F,
                     quote = F)
         
       })

# xG by game ####
lapply(2011:max(xgbygame$Season),
       FUN = function(x){
         dt <- xgbygame %>%
           filter(Season == x) %>%
           arrange(desc(Date)) %>%
           select(-Season) %>%
           left_join(teamlinks, by = c("Home" = "Abbr")) %>%
           left_join(teamlinks, by = c("Away" = "Abbr"), suffix = c("_home", "_away")) %>%
           mutate(Home = FullName_home,
                  Away = FullName_away,
                  Date = format(Date, "%m/%d/%Y")) %>%
           select(-FullName_home, -FullName_away)
         write.csv(dt, paste0(path, "/Dropbox/ASA Blog Data/HTMLOutputs/xGoalsByGame_", x, ".csv"))
         output <- xtable(dt, 
                          digits = c(0, 0, 0, 0, 2, 2, 0, 0, 2, 2, 0, 2, 2),
                          align = rep("c", ncol(dt) + 1))
         write.table(gsub("\n", "",
                          gsub(" <", "<", 
                               gsub("> ", ">", 
                                    gsub("<table border=1>", '<script>
  $(document).ready(function() {
                                         $("#myTable").tablesorter();
       });
                                         </script><TABLE border=1 id="myTable" class="tablesorter" style="white-space:nowrap;"><thead>',
                                         gsub(" xGDp </th>  </tr>", " xGDp </th>  </tr> </thead> <tbody>",
                                              print.xtable(output, 
                                                           type = "html",
                                                           include.rownames = F,
                                                           print.results = F)))))),
                     file = paste0(path, "/Dropbox/ASA Blog Data/HTMLOutputs/xGoalsByGame_", x, ".txt"),
                     row.names = F,
                     quote = F)
         
       })
