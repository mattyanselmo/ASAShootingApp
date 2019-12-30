# Date: 2019-10-01
# Updated: 2019-12-29
# These functions use the user inputs to filter the data

# This function organizes the xG team per game table filters
get_team_xg_pergame <- function(team_xg_inputs){
  if(team_xg_inputs$team_seasonordate == 'Season'){
    if(team_xg_inputs$team_homeadjusted == "Home-adjusted"){
      dt <- teamxgoals.func(teamxgoals.adj, 
                            date1 = as.Date('2000-01-01'), 
                            date2 = as.Date('9999-12-31'),
                            season = team_xg_inputs$team_seasonfilter,
                            even = team_xg_inputs$team_evenstate,
                            pattern = team_xg_inputs$team_pattern,
                            pergame = T,
                            advanced = ifelse(team_xg_inputs$team_advanced == 'Basic stats', F, T),
                            venue = team_xg_inputs$team_home,
                            byseasons = team_xg_inputs$team_byseasons,
                            confview = team_xg_inputs$team_conferenceview) %>%
        mutate(`Comp ($MM)` = Comp/1000000) %>%
        select(-Comp) 
    } else{
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = as.Date('2000-01-01'), 
                            date2 = as.Date('9999-12-31'),
                            season = team_xg_inputs$team_seasonfilter,
                            even = team_xg_inputs$team_evenstate,
                            pattern = team_xg_inputs$team_pattern,
                            pergame = T,
                            advanced = ifelse(team_xg_inputs$team_advanced == 'Basic stats', F, T),
                            venue = team_xg_inputs$team_home,
                            byseasons = team_xg_inputs$team_byseasons,
                            confview = team_xg_inputs$team_conferenceview) %>%
        mutate(`Comp ($MM)` = Comp/1000000) %>%
        select(-Comp)
    }
    
  } else{
    if(team_xg_inputs$team_homeadjusted == "Home-adjusted"){
      dt <- teamxgoals.func(teamxgoals.adj, 
                            date1 = team_xg_inputs$team_date1, 
                            date2 = team_xg_inputs$team_date2,
                            season = as.numeric(format(team_xg_inputs$team_date1, "%Y")):as.numeric(format(team_xg_inputs$team_date2, "%Y")),
                            even = team_xg_inputs$team_evenstate,
                            pattern = team_xg_inputs$team_pattern,
                            pergame = T,
                            advanced = ifelse(team_xg_inputs$team_advanced == 'Basic stats', F, T),
                            venue = team_xg_inputs$team_home,
                            byseasons = team_xg_inputs$team_byseasons,
                            confview = team_xg_inputs$team_conferenceview) %>%
        mutate(`Comp ($MM)` = Comp/1000000) %>%
        select(-Comp)
    }else{
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = team_xg_inputs$team_date1, 
                            date2 = team_xg_inputs$team_date2,
                            season = as.numeric(format(team_xg_inputs$team_date1, "%Y")):as.numeric(format(team_xg_inputs$team_date2, "%Y")),
                            even = team_xg_inputs$team_evenstate,
                            pattern = team_xg_inputs$team_pattern,
                            pergame = T,
                            advanced = ifelse(team_xg_inputs$team_advanced == 'Basic stats', F, T),
                            venue = team_xg_inputs$team_home,
                            byseasons = team_xg_inputs$team_byseasons,
                            confview = team_xg_inputs$team_conferenceview) %>%
        mutate(`Comp ($MM)` = Comp/1000000) %>%
        select(-Comp)
    }
  }
  
  is.num <- sapply(dt, is.numeric)
  dt[is.num] <- lapply(dt[is.num], round, 3)
  return(dt)
}

# This function organizes the xG team total table filters 
get_team_xg_total <- function(team_xg_inputs){
  if(team_xg_inputs$team_seasonordate == 'Season'){
      if(team_xg_inputs$team_homeadjusted == "Home-adjusted"){
        dt <- teamxgoals.func(teamxgoals.adj,
                              date1 = as.Date('2000-01-01'),
                              date2 = as.Date('9999-12-31'),
                              season = team_xg_inputs$team_seasonfilter,
                              even = team_xg_inputs$team_evenstate,
                              pattern = team_xg_inputs$team_pattern,
                              pergame = F,
                              advanced = ifelse(team_xg_inputs$team_advanced == 'Basic stats', F, T),
                              venue = team_xg_inputs$team_home,
                              byseasons = team_xg_inputs$team_byseasons,
                              confview = team_xg_inputs$team_conferenceview) %>%
          mutate(`Comp ($MM)` = Comp/1000000) %>%
          select(-Comp)
      } else{
        dt <- teamxgoals.func(teamxgoals,
                              date1 = as.Date('2000-01-01'),
                              date2 = as.Date('9999-12-31'),
                              season = team_xg_inputs$team_seasonfilter,
                              even = team_xg_inputs$team_evenstate,
                              pattern = team_xg_inputs$team_pattern,
                              pergame = F,
                              advanced = ifelse(team_xg_inputs$team_advanced == 'Basic stats', F, T),
                              venue = team_xg_inputs$team_home,
                              byseasons = team_xg_inputs$team_byseasons,
                              confview = team_xg_inputs$team_conferenceview) %>%
          mutate(`Comp ($MM)` = Comp/1000000) %>%
          select(-Comp)
      }

    } else{
      if(team_xg_inputs$team_homeadjusted == "Home-adjusted"){

        dt <- teamxgoals.func(teamxgoals.adj,
                              date1 = team_xg_inputs$team_date1,
                              date2 = team_xg_inputs$team_date2,
                              season = as.numeric(format(team_xg_inputs$team_date1, "%Y")):as.numeric(format(team_xg_inputs$team_date2, "%Y")),
                              even = team_xg_inputs$team_evenstate,
                              pattern = team_xg_inputs$team_pattern,
                              pergame = F,
                              advanced = ifelse(team_xg_inputs$team_advanced == 'Basic stats', F, T),
                              venue = team_xg_inputs$team_home,
                              byseasons = team_xg_inputs$team_byseasons,
                              confview = team_xg_inputs$team_conferenceview) %>%
          mutate(`Comp ($MM)` = Comp/1000000) %>%
          select(-Comp)
      }else{
        dt <- teamxgoals.func(teamxgoals,
                              date1 = team_xg_inputs$team_date1,
                              date2 = team_xg_inputs$team_date2,
                              season = as.numeric(format(team_xg_inputs$team_date1, "%Y")):as.numeric(format(team_xg_inputs$team_date2, "%Y")),
                              even = team_xg_inputs$team_evenstate,
                              pattern = team_xg_inputs$team_pattern,
                              pergame = F,
                              advanced = ifelse(team_xg_inputs$team_advanced == 'Basic stats', F, T),
                              venue = team_xg_inputs$team_home,
                              byseasons = team_xg_inputs$team_byseasons,
                              confview = team_xg_inputs$team_conferenceview) %>%
          mutate(`Comp ($MM)` = Comp/1000000) %>%
          select(-Comp)
      }
    }
   
  is.num <- sapply(dt, is.numeric)
  dt[is.num] <- lapply(dt[is.num], round, 3)
  
  return(dt)
}
# This function organizes the xG Player total table filters. 
### At some point this will be converted such that the output is a SQL query ##
get_dt_total <- function(shooter_inputs){
  # In the original script, but commented out as below
  # if(shooter_inputs$teamfilter == "All"){
  #   teamfilter <- unique(playerxgoals$team)
  # } else{
  #   teamfilter <- shooter_inputs$teamfilter
  # }
  
  if(shooter_inputs$seasonordate == 'Season'){
    dt_total <- shooterxgoals.func(playerxgoals,
                                   date1 = as.Date('2000-01-01'),
                                   date2 = as.Date('9999-12-31'),
                                   season = shooter_inputs$seasonfilter,
                                   minfilter = shooter_inputs$minimum_minutes,
                                   shotfilter = shooter_inputs$minimum_shots,
                                   keyfilter = shooter_inputs$minimum_keypasses,
                                   teamfilter = shooter_inputs$teamfilter,
                                   byteams = shooter_inputs$split_byteams,
                                   byseasons = shooter_inputs$split_byseasons,
                                   OpenPlay = "Open" %in% shooter_inputs$pattern,
                                   FK = "FK" %in% shooter_inputs$pattern,
                                   PK = "PK" %in% shooter_inputs$pattern,
                                   SetPiece = "Setpiece" %in% shooter_inputs$pattern) %>%
      mutate(`xG/shot` = ifelse(Shots > 0, xG/Shots, 0),
             `xA/pass` = ifelse(KeyP > 0, xA/KeyP, 0),
             `G-xG/shot` = ifelse(Shots > 0, `G-xG`/Shots, 0),
             `A-xA/pass` = ifelse(KeyP > 0, `A-xA`/KeyP, 0)) %>%
      mutate(`Comp ($K)` = round(Salary/1000, 0)) %>%
      select(-Salary) %>%
      rename(A = Assts,
             G = Goals)
  } else{
    dt_total <- shooterxgoals.func(playerxgoals,
                                   date1 = shooter_inputs$date1,
                                   date2 = shooter_inputs$date2,
                                   season = as.numeric(format(shooter_inputs$date1, "%Y")):as.numeric(format(shooter_inputs$date2, "%Y")),
                                   minfilter = shooter_inputs$minimum_minutes,
                                   shotfilter = shooter_inputs$minimum_shots,
                                   keyfilter = shooter_inputs$minimum_keypasses,
                                   teamfilter = shooter_inputs$teamfilter,
                                   byteams = shooter_inputs$split_byteams,
                                   byseasons = shooter_inputs$split_byseasons,
                                   OpenPlay = "Open" %in% shooter_inputs$pattern,
                                   FK = "FK" %in% shooter_inputs$pattern,
                                   PK = "PK" %in% shooter_inputs$pattern,
                                   SetPiece = "Setpiece" %in% shooter_inputs$pattern) %>%
      mutate(`xG/shot` = ifelse(Shots > 0, xG/Shots, 0),
             `xA/pass` = ifelse(KeyP > 0, xA/KeyP, 0),
             `G-xG/shot` = ifelse(Shots > 0, `G-xG`/Shots, 0),
             `A-xA/pass` = ifelse(KeyP > 0, `A-xA`/KeyP, 0)) %>%
      mutate(`Comp ($K)` = round(Salary/1000, 0)) %>%
      select(-Salary) %>%
      rename(A = Assts,
             G = Goals)
  }
  
  if("Pos" %in% names(dt_total)){
    dt_total %>% filter(Pos %in% shooter_inputs$position_checkbox | is.na(Pos))
  } else{
    dt_total
  }
  
  return(dt_total)
}

# This function organizes the xG Player per 96 table filters. 
get_dt_per96 <- function(shooter_inputs){
    # if(shooter_inputs$teamfilter == "All"){
    #   teamfilter <- unique(playerxgoals$team)
    # } else{
    #   teamfilter <- shooter_inputs$teamfilter
    # }
  if(shooter_inputs$seasonordate == 'Season'){
    dt_per96 <- shooterxgoals_perminute(playerxgoals,
                                   date1 = as.Date('2000-01-01'),
                                   date2 = as.Date('9999-12-31'),
                                   season = shooter_inputs$seasonfilter,
                                   minfilter = shooter_inputs$minimum_minutes,
                                   shotfilter = shooter_inputs$minimum_shots,
                                   keyfilter = shooter_inputs$minimum_keypasses,
                                   teamfilter = shooter_inputs$teamfilter,
                                   byteams = shooter_inputs$split_byteams,
                                   byseasons = shooter_inputs$split_byseasons,
                                   OpenPlay = "Open" %in% shooter_inputs$pattern,
                                   FK = "FK" %in% shooter_inputs$pattern,
                                   PK = "PK" %in% shooter_inputs$pattern,
                                   SetPiece = "Setpiece" %in% shooter_inputs$pattern) %>%
      mutate(`xG/shot` = ifelse(Shots > 0, xG/Shots, 0),
             `xA/pass` = ifelse(KeyP > 0, xA/KeyP, 0),
             `G-xG/shot` = ifelse(Shots > 0, `G-xG`/Shots, 0),
             `A-xA/pass` = ifelse(KeyP > 0, `A-xA`/KeyP, 0)) %>%
      mutate(`Comp ($K)` = round(Salary/1000, 0)) %>%
      select(-Salary) %>%
      rename(A = Assts,
             G = Goals)
  } else{
    dt_per96 <- shooterxgoals_perminute(playerxgoals,
                                   date1 = shooter_inputs$date1,
                                   date2 = shooter_inputs$date2,
                                   season = as.numeric(format(shooter_inputs$date1, "%Y")):as.numeric(format(shooter_inputs$date2, "%Y")),
                                   minfilter = shooter_inputs$minimum_minutes,
                                   shotfilter = shooter_inputs$minimum_shots,
                                   keyfilter = shooter_inputs$minimum_keypasses,
                                   teamfilter = shooter_inputs$teamfilter,
                                   byteams = shooter_inputs$split_byteams,
                                   byseasons = shooter_inputs$split_byseasons,
                                   OpenPlay = "Open" %in% shooter_inputs$pattern,
                                   FK = "FK" %in% shooter_inputs$pattern,
                                   PK = "PK" %in% shooter_inputs$pattern,
                                   SetPiece = "Setpiece" %in% shooter_inputs$pattern) %>%
      mutate(`xG/shot` = ifelse(Shots > 0, xG/Shots, 0),
             `xA/pass` = ifelse(KeyP > 0, xA/KeyP, 0),
             `G-xG/shot` = ifelse(Shots > 0, `G-xG`/Shots, 0),
             `A-xA/pass` = ifelse(KeyP > 0, `A-xA`/KeyP, 0)) %>%
      mutate(`Comp ($K)` = round(Salary/1000, 0)) %>%
      select(-Salary) %>%
      rename(A = Assts,
             G = Goals)
  }
    
    dt_per96 %>% filter(Pos %in% shooter_inputs$shooting_position)
    
    return(dt_per96)
    
}