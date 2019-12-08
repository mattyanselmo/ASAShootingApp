
# This function organizes the xG table filters. 
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