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

# SQL query for shots data to build model - currently limited to 
# 300 obs to get around time out issue and for testing purposes
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

# Potential function to transform new format of shots data to old format for
# modeling
old_shot_format <- function(dataframe, dbConnection){
  # Isolate relevant extra variables ---------------------------
  shots_non_null_vars <- dataframe %>%
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
  shots <- dataframe %>%
    left_join(dbGetQuery(dbConnection, "SELECT player_id, player_name FROM mls.players") %>%
                rename(shooter = player_name), "player_id") %>%
    left_join(dbGetQuery(dbConnection, "SELECT player_id, player_name FROM mls.players") %>%
                rename(passer_id = player_id, passer = player_name), "passer_id") %>%
    left_join(dbGetQuery(dbConnection, "SELECT team_id, team_short_name FROM mls.teams") %>% 
                rename(hteam = team_short_name), c("hteam_id" = "team_id")) %>%
    left_join(dbGetQuery(dbConnection, "SELECT team_id, team_short_name FROM mls.teams") %>% 
                rename(ateam = team_short_name), c("ateam_id" = "team_id")) %>%
    left_join(dbGetQuery(dbConnection, "SELECT team_id, team_short_name FROM mls.teams") %>% 
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
                              type_id == 16 ~ "Goal")) %>%
    left_join(score_changes, c("game_id", "period_id")) %>%
    filter(cum_second >= start_second,
           cum_second <= end_second) %>%
    select(-start_second, -end_second) %>%
    left_join(player_changes, c("game_id", "period_id")) %>%
    filter(cum_second >= start_second,
           cum_second <= end_second) %>%
    select(-start_second, -end_second) %>%
    left_join(goalkeeper_changes, c("game_id", "period_id",
                                    "team.1_id" = "team_id")) %>%
    filter(cum_second >= start_second,
           cum_second <= end_second) %>%
    select(date, gameID = game_id, eventID = id, event = event_id, half = period_id,
           time, minute, second, shooter_id = player_id, shooter, team_id, team, goalie_id,
           goalie, team.1_id, team.1, passer_id, passer, assisted, passID = related_event_id,
           through = assist_throughball, cross = assist_cross, x, y,
           gmlocy = goal_mouth_y, gmlocz = goal_mouth_z, bodypart, result, patternOfPlay,
           hteam_id, hteam, ateam_id, ateam, hscore, ascore, hplayers, aplayers,
           hfinal, afinal, final, bigchance = big_chance, shots_non_null_vars$key) %>%
    mutate_if(is.logical, ~as.numeric(ifelse(is.na(.), FALSE, .))) %>%
    arrange(date, gameID, half, minute, second, team_id)
}

# Isolate relevant extra variables ---------------------------
shots_non_null_vars <- old_shots %>%
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
##### Maybe unnecessary ######
# half_starts <- dbGetQuery(conn, "SELECT DISTINCT events.game_id, events.period_id,
#                                                  MIN(events.minute) AS minute,
#                           MIN(events.second) AS second
#                           FROM mls.events
#                           LEFT JOIN mls.games
#                           USING(game_id)
#                           WHERE season_name = (SELECT MAX(season_name)
#                           FROM mls.games)
#                           AND period_id IN (1, 2, 3, 4)
#                           GROUP BY events.game_id, events.period_id") %>%
#   mutate(type_id = 32)
# 
# half_ends <- dbGetQuery(conn, "SELECT DISTINCT events.game_id, events.period_id,
#                         MAX(events.minute) AS minute,
#                         MAX(events.second) AS second
#                         FROM mls.events
#                         LEFT JOIN mls.games
#                         USING(game_id)
#                         WHERE season_name = (SELECT MAX(season_name)
#                         FROM mls.games)
#                         AND period_id IN (1, 2, 3, 4)
#                         GROUP BY events.game_id, events.period_id") %>%
#   mutate(type_id = 30)
# 
# half_starts_ends <- rbind(half_starts, half_ends)
# 
# player_changes_raw <- dbGetQuery(conn, "SELECT DISTINCT events.game_id, events.team_id, events.period_id,
#                                                         events.type_id, events.minute, events.second
#                                  FROM mls.events
#                                  LEFT JOIN mls.games
#                                  USING(game_id)
#                                  WHERE season_name = (SELECT MAX(season_name)
#                                  FROM mls.games)
#                                  AND type_id = 17
#                                  AND (red = TRUE OR second_yellow = TRUE)")
# 
# player_changes <- plyr::rbind.fill(player_changes_raw, half_starts_ends) %>%
#   arrange(game_id, period_id, minute, second) %>%
#   left_join(games %>% select(game_id, home_team_id, away_team_id), "game_id") %>%
#   mutate(hplayers = case_when(team_id == home_team_id & type_id == 17 ~ -1,
#                               type_id == 32 & period_id == 1 ~ 11,
#                               TRUE ~ 0),
#          aplayers = case_when(team_id == away_team_id & type_id == 17 ~ -1,
#                               type_id == 32 & period_id == 1 ~ 11,
#                               TRUE ~ 0)) %>%
#   distinct() %>%
#   group_by(game_id) %>%
#   mutate(cum_hplayers = cumsum(hplayers),
#          cum_aplayers = cumsum(aplayers),
#          start_second = minute * 60 + second,
#          end_second = ifelse(lead(type_id) == 30,
#                              lead(minute) * 60 + lead(second),
#                              lead(minute) * 60 + lead(second) - 1)) %>%
#   ungroup() %>%
#   filter(type_id != 30) %>%
#   select(game_id, period_id, start_second, end_second,
#          hplayers = cum_hplayers, aplayers = cum_aplayers)
##### Maybe unnecessary ######

shots <- old_shots %>%
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
  # left_join(goalkeeper_changes, c("game_id", "period_id",
  #                                 "team.1_id" = "team_id")) %>%
  # filter(cum_second >= start_second,
  #        cum_second <= end_second) %>%
  select(date, time, half = period_id, shooter, gameID, team, team.1, passer, 
         assisted, dive = diving_save, bodypart,
         angle, through = assist_throughball, cross = assist_cross, 
         ateam, final, patternOfPlay, year, gmlocz = goal_mouth_z, result, hteam,
         distance = Distance, goalie = Keeper, available = Available, 
         keepreach = KeepReach, hplayers = PlayerH,
         aplayers = PlayerA, hscore, ascore) %>%
  # select(date, gameID = game_id, eventID = id, event = event_id, half = period_id,
  #        time, minute, second, shooter_id = player_id, shooter, team_id, team, goalie_id,
  #        goalie, team.1_id, team.1, passer_id, passer, assisted, passID = related_event_id,
  #        through = assist_throughball, cross = assist_cross, x, y,
  #        gmlocy = goal_mouth_y, gmlocz = goal_mouth_z, bodypart, result, patternOfPlay,
  #        hteam_id, hteam, ateam_id, ateam, hscore, ascore, hplayers, aplayers,
  #        hfinal, afinal, final, bigchance = big_chance, shots_non_null_vars$key) %>%
  # mutate_if(is.logical, ~as.numeric(ifelse(is.na(.), FALSE, .))) %>%
  # arrange(date, gameID, half, minute, second, team_id)
  mutate(patternOfPlay.model = ifelse(patternOfPlay == 'Throw in', 'Regular', patternOfPlay),
         distance = ifelse(patternOfPlay == 'Penalty', 
                           12, 
                           distance),
         available = ifelse(patternOfPlay == 'Penalty', 
                            8, 
                            ifelse(is.na(available),
                                   0, 
                                   available)))

# This should just be a single SQL query
# shooting15 <- bind_rows(lapply(paste0('IgnoreList/', grep('shots with xG', list.files('IgnoreList'), value = T)), 
#                                function(x) read.csv(x, stringsAsFactors = F) %>% select(-one_of("X")))) %>%
#   left_join(teamnames, by = c('team' = 'FullName')) %>%
#   left_join(teamnames, by = c('team.1' = 'FullName')) %>%
#   mutate(team = Abbr.x,
#          team.1 = Abbr.y) %>%
#   select(-c(Abbr.x, Abbr.y)) %>%
#   left_join(teamnames, by = c('hteam' = 'FullName')) %>%
#   left_join(teamnames, by = c('ateam' = 'FullName')) %>%
#   mutate(hteam = Abbr.x,
#          ateam = Abbr.y) %>%
#   select(-c(Abbr.x, Abbr.y)) %>%
#   mutate(date = as.Date(date, format = ifelse(row_number() %in% grep("/", date), "%m/%d/%Y", "%Y-%m-%d")),
#          year = format(date, '%Y'),
#          time = sapply(strsplit(time, ':'), function(x) as.numeric(x[1]) + as.numeric(x[2])/60))
# 
# shooting14 <- bind_rows(lapply(paste0('IgnoreList/', grep('shotdata with xgoals', list.files("IgnoreList"), value = T)), 
#                                function(x) read.csv(x, stringsAsFactors = F))) %>%
#   left_join(teamnames, by = c('Team' = 'FullName')) %>%
#   left_join(teamnames, by = c('Team.1' = 'FullName')) %>%
#   mutate(team = Abbr.x,
#          team.1 = Abbr.y) %>%
#   select(-c(Abbr.x, Abbr.y)) %>%
#   left_join(teamnames, by = c('hteam' = 'FullName')) %>%
#   left_join(teamnames, by = c('ateam' = 'FullName')) %>%
#   mutate(hteam = Abbr.x,
#          ateam = Abbr.y) %>%
#   select(-c(Abbr.x, Abbr.y)) %>%
#   mutate(Date = as.Date(Date, origin = '1899-12-30'),
#          year = format(Date, '%Y'),
#          Time = floor(Time) + (Time - floor(Time))/0.6)
# 
# shooting14 <- shooting14 %>%
#   mutate(gameID = 0) %>%
#   select(date = Date, time = Time, half = Half, shooter = Shooter, gameID,
#          team, goalie = Keeper, team.1, passer = Passer, assisted = Assisted,
#          through = Through, cross = Cross, distance = Distance, angle = Angle,
#          available = Available, keepreach = KeepReach, dive = Dive, gmlocz = GMLocZ,
#          bodypart = Body.Part, result = Result, hteam, ateam, hplayers = PlayerH, 
#          aplayers = PlayerA, year, hscore, ascore, final, patternOfPlay = Pattern.of.Play)
# 
# shooting <- bind_rows(shooting14,
#                       shooting15) %>%
#   mutate(patternOfPlay.model = ifelse(patternOfPlay == 'Throw in', 'Regular', patternOfPlay),
#          distance = ifelse(patternOfPlay == 'Penalty', 
#                            12, 
#                            distance),
#          available = ifelse(patternOfPlay == 'Penalty', 
#                             8, 
#                             ifelse(is.na(available),
#                                    0, 
#                                    available)))

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
                   data = shooting,
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
                            filter(result == 'Goal' | result == 'Saved'),
                          family = binomial)

# Note: Tested interaction between free kicks and distance: worse fit on holdout 2015 - 2017

## ??????
## Not sure what this is? -----------------------------------------------------
# Append predictions ####
# Weight current season against previous season
season.progress <- (shooting %>%
                      filter(year == max(year)) %>%
                      group_by(team) %>%
                      summarize(games = length(unique(gameID))) %>%
                      ungroup() %>%
                      summarize(progress = mean(games)/34) %>%
                      select(progress))$progress

## Predicting xG on new data --------------------------------------------------
shooting[['xGShooter1']] <- predict(xgoal.model, 
                                    shooting %>%
                                      mutate(year = ifelse(as.numeric(year) == max(as.numeric(year)), 
                                                           as.character(max(as.numeric(year)) - 1), 
                                                           year)), 
                                    type = 'response')
shooting[['xGShooter2']] <- predict(xgoal.model, 
                                    shooting, 
                                    type = 'response')

shooting[['xGTeam1']] <- predict(xgoal.model, shooting %>%
                                   mutate(patternOfPlay.model = ifelse(patternOfPlay.model == 'Penalty', 
                                                                       'Regular', 
                                                                       patternOfPlay.model),
                                          year = ifelse(as.numeric(year) == max(as.numeric(year)), 
                                                        as.character(max(as.numeric(year)) - 1), 
                                                        year)), 
                                 type = 'response')
shooting[['xGTeam2']] <- predict(xgoal.model, shooting %>%
                                   mutate(patternOfPlay.model = ifelse(patternOfPlay.model == 'Penalty', 
                                                                       'Regular', 
                                                                       patternOfPlay.model)), 
                                 type = 'response')

shooting[['xGKeeper1']][shooting$result %in% c('Goal', 'Saved')] <- predict(xgoal.model.keeper, 
                                                                            shooting %>%
                                                                              filter(result == 'Goal' | result == 'Saved') %>%
                                                                              mutate(year = ifelse(as.numeric(year) == max(as.numeric(year)), 
                                                                                                   as.character(max(as.numeric(year)) - 1), 
                                                                                                   year)), 
                                                                            type = 'response')
shooting[['xGKeeper2']][shooting$result %in% c('Goal', 'Saved')] <- predict(xgoal.model.keeper, 
                                                                            shooting %>%
                                                                              filter(result == 'Goal' | result == 'Saved'), 
                                                                            type = 'response')

## ?????????????
## not sure what this is ------------------------------------------------------
shooting <- shooting %>%
  mutate(xGShooter = season.progress*xGShooter2 + (1 - season.progress)*xGShooter1,
         xGTeam = season.progress*xGTeam2 + (1 - season.progress)*xGTeam1,
         xGKeeper = season.progress*xGKeeper2 + (1 - season.progress)*xGKeeper1) %>%
  select(-c(xGShooter1, xGShooter2, xGTeam1, xGTeam2, xGKeeper1, xGKeeper2))

## Add new xG data to xG tables in server -------------------------------------
