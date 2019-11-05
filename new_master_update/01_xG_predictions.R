## Building out xG on new events coming into server.
## Imagining that this will predict the xG values on the new events
# and then store those values in a new table in the server that has
# eventID, attacking teamID, defending teamID, playerID, and keeperID based
# on conversations with Tyler and Matthias that with the server, the db 
# structure may break up tables into smaller chunks that are then pulled
# in to generate the tables in the app

## Necessary Libraries --------------------------------------------------------
library(tidyverse)
library(stringr)
library(tidyquant)
library(DBI)
library(dbplyr)
library(RPostgres)

## Filtering newly scraped data to shots to predict xG on ---------------------
new_shots <- filter(events_new, type_id %in% c(13, 14, 15, 16))

## Load old data from server to build model -----------------------------------
# Maybe this is all done earlier in the week and the query is actually to just 
# pull in the previously determined model
# Connect to server
conn <- dbConnect(Postgres(),
                  user = getOption("asa_user"),
                  password = getOption("asa_password"),
                  host = getOption("asa_host"),
                  port = 25060,
                  dbname = getOption("asa_db_name"),
                  sslmode = "require")

# SQL query for shots data to build model 
old_shots <- dbGetQuery(conn, "SELECT e1.*,
                                      games.date_time_et,
                                      games.home_team_score AS hfinal,
                                      games.away_team_score AS afinal,
                                      games.home_team_id AS hteam_id,
                                      games.away_team_id AS ateam_id,
                                      e2.throughball AS assist_throughball,
                                      e2.cross AS assist_cross,
                                      e2.player_id AS passer_id
                                FROM mls.events e1
                                LEFT JOIN mls.games
                                      USING(game_id)
                                LEFT JOIN mls.events e2
                                ON e1.game_id = e2.game_id
                                      AND e1.team_id = e2.team_id
                                      AND e1.related_event_id = e2.event_id
                                      WHERE season_name = (SELECT MAX(season_name)
                                FROM mls.games)
                                      AND e1.type_id IN (13, 14, 15, 16)
                                      AND e1.own_goal IS NULL
                                LIMIT 400")

all_shots <- rbind(new_shots, old_shots)
# new data is missing the following columns: date_time_et,
# hfinal, afinal, hteam_id, ateam_id, assist_throughball,
# assist_cross, passer_id

# Isolate relevant extra variables ---------------------------
## Maybe unnecessary
shots_non_null_vars <- all_shots %>%
  summarize_all(~mean(is.na(.))) %>%
  select(-(game_id:outcome_name),
         -(date_time_et:ateam_id),
         -goal_mouth_y, -goal_mouth_z, -big_chance, -related_event_id,
         -assisted, -assist_cross, -assist_throughball, -passer_id,
         -head, -left_foot, -right_foot, -other_body_part, -blocked,
         -regular_play, -set_piece, -fast_break, -from_corner,
         -direct_freekick, -penalty, -throwin_set_piece) %>%
  gather() %>%
  filter(value < 1) %>%
  arrange(key)

# Join and reshape -------------------------------------------
## Maybe unnecessary if can condense the goalie information
## into a simpler query to get the information 
half_starts_gk <- dbGetQuery(conn, "SELECT DISTINCT events.game_id, events.period_id,
                                                 events.team_id,
                             MIN(events.minute) AS minute,
                             MIN(events.second) AS second
                             FROM mls.events
                             LEFT JOIN mls.games
                             USING(game_id)
                             WHERE season_name = (SELECT MAX(season_name)
                             FROM mls.games)
                             AND period_id IN (1, 2, 3, 4)
                             GROUP BY events.game_id, events.period_id, events.team_id") %>%
  mutate(type_id = 32)

half_ends_gk <- dbGetQuery(conn, "SELECT DISTINCT events.game_id, events.period_id,
                           events.team_id,
                           MAX(events.minute) AS minute,
                           MAX(events.second) AS second
                           FROM mls.events
                           LEFT JOIN mls.games
                           USING(game_id)
                           WHERE season_name = (SELECT MAX(season_name)
                           FROM mls.games)
                           AND period_id IN (1, 2, 3, 4)
                           GROUP BY events.game_id, events.period_id, events.team_id") %>%
  mutate(type_id = 30)

half_starts_ends_gk <- rbind(half_starts_gk, half_ends_gk)

goalkeeper_changes_raw <- dbGetQuery(conn, "SELECT DISTINCT formations.game_id, formations.team_id,
                                     formations.player_id AS goalie_id,
                                     formations.start_period AS period_id,
                                     formations.start_minute AS minute,
                                     formations.start_second AS second
                                     FROM mls.formations
                                     LEFT JOIN mls.games
                                     USING(game_id)
                                     WHERE season_name = (SELECT MAX(season_name)
                                     FROM mls.games)
                                     AND position = 'GK'") %>%
  mutate(type_id = 999)

goalkeeper_changes <- plyr::rbind.fill(goalkeeper_changes_raw, half_starts_ends_gk) %>%
  arrange(game_id, team_id, period_id, minute, second, type_id) %>%
  distinct() %>%
  group_by(game_id, team_id) %>%
  mutate(start_second = minute * 60 + second,
         end_second = ifelse(lead(type_id) == 30,
                             lead(minute) * 60 + lead(second),
                             lead(minute) * 60 + lead(second) - 1)) %>%
  fill(goalie_id, .direction = "down") %>%
  ungroup() %>%
  filter(type_id != 30,
         end_second != -1) %>%
  left_join(players %>% select(goalie_id = player_id, goalie = player_name), "goalie_id") %>%
  select(game_id, period_id, team_id, start_second, end_second, goalie_id, goalie)

## Function to add distance, available, and keepreach columns -----------------
shotloc <- function(data, fwidth = 80, flength = 115){
  # Getting initial values of X and Y
  X <- data[1]
  Y <- data[2]
  gmX <- data[3]
  X <- (100 - X)*flength/100
  Y <- (Y-50)*fwidth/100
  
  # Calculating distance to goal and 'available'? 
  distance <- sqrt(X^2 + Y^2)
  theta <- atan(Y/X)*180/pi
  slope1 <- -abs(Y)/X
  slope2 <- X/(abs(Y) + 4)
  xpoint1 <- -4*(slope1 + slope2)/(slope2 - slope1)
  ypoint1 <- flength - slope2*(xpoint1 + 4)
  available <- sqrt((4-xpoint1)^2 + (flength-ypoint1)^2)
  
  # Calculating keepreach
  ang1 <- ifelse(Y == -4, pi/2, ifelse(Y < -4, pi + atan(X/(4+Y)), atan(X/(4+Y))))
  ang2 <- ifelse(Y == 4, pi/2, ifelse(Y < 4, atan(X/(4-Y)), pi + atan(X/(4-Y))))
  angshot <- apply(data.frame(ang1, pi-ang2), 1, mean, na.rm = T)
  slope3 <- -1/tan(angshot) ##slope from near post perpendicular to shot angle
  slope4 <- ifelse(Y > 0, X/(Y+4), X/(Y-4)) ##slope to far post
  xpoint2 <- ifelse(Y == 0, -4, sign(Y)*4*(slope3 + slope4)/(slope3 - slope4)) 
  ypoint2 <- ifelse(angshot > pi/2, flength - (4-xpoint2)*abs(slope3), flength - (xpoint2 + 4)*abs(slope4))
  xmid <- (xpoint2 + sign(Y)*4)/2
  ymid <- (ypoint2 + flength)/2
  GmX <- (gmX - 50)*fwidth/100
  xshot <- array(dim = length(X))
  yshot <- array(dim = length(X))
  xshot[GmX == Y & is.na(GmX) == F] <- GmX[GmX == Y & is.na(GmX) == F]
  xshot[GmX != Y & is.na(GmX) == F] <- c(ifelse(Y < GmX, (4*slope3 - GmX*X/abs(Y-GmX))/(-X/abs(Y-GmX)-slope3),(-4*slope3+GmX*abs(X/(Y-GmX)))/(-slope3 + X/(Y-GmX))))[GmX != Y & is.na(GmX) == F]
  yshot[GmX == Y & is.na(GmX) == F] <- (-abs(slope3)*abs(abs(xshot) - 4) + flength)[GmX == Y & is.na(GmX) == F]
  yshot[GmX != Y & is.na(GmX) == F] <- ifelse(Y < 0, flength - slope3*(xshot+4), flength - slope3*(xshot-4))[GmX != Y & is.na(GmX) == F]
  keepreach <- sqrt((xmid - xshot)^2 + (ymid - yshot)^2)
  dive.dir <- sign(xshot - xmid)
  dive.dir[is.na(GmX) == T] <- 0
  
  return(c(distance, theta, available, keepreach, dive.dir))
}

output <- apply(cbind(all_shots$x, all_shots$y, all_shots$goal_mouth_y),1,shotloc)
all_shots$distance <- output[seq(1,length(output),5)]
all_shots$angle <- output[seq(2,length(output),5)]
all_shots$available <- output[seq(3,length(output),5)]
all_shots$keepreach <- output[seq(4,length(output),5)]
all_shots$dive <- output[seq(5,length(output),5)]

# shots$gmlocz <- 8*shots$gmlocz/37.5

## Transforming shot dataframe into old format that fits modeling -------------
shots <- all_shots %>%
  left_join(dbGetQuery(conn, "SELECT player_id, player_name FROM mls.players") %>%
              rename(shooter = player_name), "player_id") %>%
  left_join(dbGetQuery(conn, "SELECT player_id, player_name FROM mls.players") %>%
              rename(passer_id = player_id, passer = player_name), "passer_id") %>%
  left_join(dbGetQuery(conn, "SELECT team_id, team_short_name FROM mls.teams") %>% 
              rename(hteam = team_short_name), c("hteam_id" = "team_id")) %>%
  left_join(dbGetQuery(conn, "SELECT team_id, team_short_name FROM mls.teams") %>% 
              rename(ateam = team_short_name), c("ateam_id" = "team_id")) %>%
  left_join(dbGetQuery(conn, "SELECT team_id, team_short_name FROM mls.teams") %>% 
              rename(team = team_short_name), "team_id") %>%
  mutate(date = format(as.Date(date_time_et), "%m/%d/%Y"),
         time = sprintf("%02s:%02s", minute, second),
         team.1_id = ifelse(hteam_id != team_id, hteam_id, ateam_id),
         team.1 = ifelse(hteam != team, hteam, ateam),
         hfinal = as.numeric(gsub("\\s+.*$", "", hfinal)),
         afinal = as.numeric(gsub("\\s+.*$", "", afinal)),
         final = hfinal - afinal,
         cum_second = minute * 60 + second,
         bodypart = case_when(head == TRUE ~ "Head",
                              left_foot == TRUE ~ "Left foot",
                              right_foot == TRUE ~ "Right foot",
                              other_body_part == TRUE ~ "Other"),
         patternOfPlay = case_when(regular_play == TRUE ~ "Regular",
                                   set_piece == TRUE ~ "Set piece",
                                   fast_break == TRUE ~ "Fastbreak",
                                   from_corner == TRUE ~ "Corner",
                                   direct_freekick == TRUE ~ "Free kick",
                                   penalty == TRUE ~ "Penalty",
                                   throwin_set_piece == TRUE ~ "Throw in"),
         result = case_when(type_id == 13 ~ "Miss",
                            type_id == 14 ~ "Post",
                            type_id == 15 & blocked == TRUE ~ "Blocked",
                            type_id == 15 & is.na(blocked) ~ "Miss",
                            type_id == 16 ~ "Goal"),
         year = lubridate::year(date)) %>%
  # Maybe unnecessary now
  # left_join(dbGetQuery(conn, "SELECT DISTINCT events.game_id, events.team_id, events.period_id,
  #                                                      events.type_id, events.minute, events.second,
  #                      events.own_goal
  #                      FROM mls.events
  #                      LEFT JOIN mls.games
  #                      USING(game_id)
  #                      WHERE season_name = (SELECT MAX(season_name)
  #                      FROM mls.games)
  #                      AND type_id = 16"), c("game_id", "period_id")) %>%
  # filter(cum_second >= start_second,
  #        cum_second <= end_second) %>%
  # select(-start_second, -end_second) %>%
  # left_join(player_changes, c("game_id", "period_id")) %>%
  # filter(cum_second >= start_second,
  #        cum_second <= end_second) %>%
  # select(-start_second, -end_second) %>%
  left_join(goalkeeper_changes, c("game_id", "period_id",
                                  "team.1_id" = "team_id")) %>%
  filter(cum_second >= start_second,
         cum_second <= end_second) %>%
  select(date, time, half = period_id, shooter, gameID, team, team.1, passer, 
         assisted, dive = diving_save, bodypart, gmlocy = goal_mouth_y,
         angle, through = assist_throughball, cross = assist_cross, 
         ateam, final, patternOfPlay, year, gmlocz = goal_mouth_z, result,
         distance, keepreach, available, hteam, goalie_id, goalie,
         hscore, ascore) %>%
  mutate(patternOfPlay.model = ifelse(patternOfPlay == 'Throw in', 
                                      'Regular', patternOfPlay),
         distance = ifelse(patternOfPlay == 'Penalty', 
                           12, 
                           distance),
         available = ifelse(patternOfPlay == 'Penalty', 
                            8, 
                            ifelse(is.na(available),
                                   0, 
                                   available)))

## Build xG xGKeeper models ---------------------------------------------------
xgoal.model <- glm(result == 'Goal' ~ 
                     patternOfPlay.model +
                     as.factor(year) +
                     I(log(distance)) +
                     available +
                     I((available - mean(available, na.rm = T))^2) +
                     I(bodypart == 'Head') +
                     through +
                     cross,
                   data = shots,
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
                          data = shots %>%
                            filter(result == 'Goal' | result == 'Saved'),
                          family = binomial)

# Note: Tested interaction between free kicks and distance: worse fit on holdout 2015 - 2017

## ??????
## Not sure what this is? -----------------------------------------------------
# Append predictions ####
# Weight current season against previous season
season.progress <- (shots %>%
                      filter(year == max(year)) %>%
                      group_by(team) %>%
                      summarize(games = length(unique(gameID))) %>%
                      ungroup() %>%
                      summarize(progress = mean(games)/34) %>%
                      select(progress))$progress

## Predicting xG on new data --------------------------------------------------
shots[['xGShooter1']] <- predict(xgoal.model, 
                                    shots %>%
                                      mutate(year = ifelse(as.numeric(year) == max(as.numeric(year)), 
                                                           as.character(max(as.numeric(year)) - 1), 
                                                           year)), 
                                    type = 'response')
shots[['xGShooter2']] <- predict(xgoal.model, 
                                    shots, 
                                    type = 'response')

shots[['xGTeam1']] <- predict(xgoal.model, 
                                 shots %>%
                                   mutate(patternOfPlay.model = ifelse(patternOfPlay.model == 'Penalty', 
                                                                       'Regular', patternOfPlay.model),
                                          year = ifelse(as.numeric(year) == max(as.numeric(year)), 
                                                        as.character(max(as.numeric(year)) - 1), 
                                                        year)), 
                                 type = 'response')
shots[['xGTeam2']] <- predict(xgoal.model, 
                                 shots %>%
                                   mutate(patternOfPlay.model = ifelse(patternOfPlay.model == 'Penalty', 
                                                                       'Regular', patternOfPlay.model)), 
                                 type = 'response')

shots[['xGKeeper1']][shots$result %in% c('Goal', 'Saved')] <- predict(xgoal.model.keeper, 
                                                                      shots %>%
                                                                        filter(result == 'Goal' | result == 'Saved') %>%
                                                                        mutate(year = ifelse(as.numeric(year) == max(as.numeric(year)),
                                                                                             as.character(max(as.numeric(year)) - 1),
                                                                                             year)), 
                                                                            type = 'response')
shots[['xGKeeper2']][shots$result %in% c('Goal', 'Saved')] <- predict(xgoal.model.keeper, 
                                                                      shots %>%
                                                                        filter(result == 'Goal' | result == 'Saved'),
                                                                      type = 'response')

## ?????????????
## not sure what this is ------------------------------------------------------
shots <- shots %>%
  mutate(xGShooter = season.progress*xGShooter2 + (1 - season.progress)*xGShooter1,
         xGTeam = season.progress*xGTeam2 + (1 - season.progress)*xGTeam1,
         xGKeeper = season.progress*xGKeeper2 + (1 - season.progress)*xGKeeper1) %>%
  select(-c(xGShooter1, xGShooter2, xGTeam1, xGTeam2, xGKeeper1, xGKeeper2))

## Add new xG data to xG tables in server -------------------------------------
