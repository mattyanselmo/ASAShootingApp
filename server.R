
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  output$shootertable <- DT::renderDataTable({
    
    if(input$shooting_seasonordate == 'Season'){
    dt <- shooterxgoals.func(playerxgoals,
                             date1 = as.Date('2000-01-01'),
                             date2 = as.Date('9999-12-31'),
                             season = input$shooting_seasonfilter,
                             shotfilter = input$shooting_minshots,
                             keyfilter = input$shooting_minkeypasses,
                             byteams = input$shooting_byteams,
                             OtherShots = input$shooting_other,
                             FK = input$shooting_fk,
                             PK = input$shooting_pk)
    } else{
      dt <- shooterxgoals.func(playerxgoals,
                               date1 = input$shooting_date1,
                               date2 = input$shooting_date2,
                               season = min(playerxgoals$Season):max(playerxgoals$Season),
                               shotfilter = input$shooting_minshots,
                               keyfilter = input$shooting_minkeypasses,
                               byteams = input$shooting_byteams,
                               OtherShots = input$shooting_other,
                               FK = input$shooting_fk,
                               PK = input$shooting_pk)
    }
      
    datatable(dt,
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = 25,
                           lengthMenu = seq(25, 100, 25)))) %>%
      formatRound(columns = c('Dist', 'xG', 'G-xG', 'Dist.key', 'xA', 'A-xA', 'xG+xA'), 
                  digits = 1) %>%
      formatRound(columns = c('Unassisted'), digits = 2)
  })
  
  output$player_download <- downloadHandler(
    filename = 'ASAshootertable.csv',
    
    content = function(file){
      if(input$shooting_seasonordate == 'Season'){
        dt <- shooterxgoals.func(playerxgoals,
                                 date1 = as.Date('2000-01-01'),
                                 date2 = as.Date('9999-12-31'),
                                 season = input$shooting_seasonfilter,
                                 shotfilter = input$shooting_minshots,
                                 keyfilter = input$shooting_minkeypasses,
                                 byteams = input$shooting_byteams,
                                 FK = input$shooting_fk,
                                 PK = input$shooting_pk)
      } else{
        dt <- shooterxgoals.func(playerxgoals,
                                 date1 = input$shooting_date1,
                                 date2 = input$shooting_date2,
                                 season = min(playerxgoals$Season):max(playerxgoals$Season),
                                 shotfilter = input$shooting_minshots,
                                 keyfilter = input$shooting_minkeypasses,
                                 byteams = input$shooting_byteams,
                                 OtherShots = input$shooting_other,
                                 FK = input$shooting_fk,
                                 PK = input$shooting_pk)
      }
      write.csv(dt, file, row.names = F)
    }
  )
  
  output$keepertable <- DT::renderDataTable({
    
    if(input$keeper_seasonordate == 'Season'){
      dt <- keeperxgoals.func(keeperxgoals,
                               date1 = as.Date('2000-01-01'),
                               date2 = as.Date('9999-12-31'),
                               season = input$keeper_seasonfilter,
                               shotfilter = input$keeper_minshots,
                               byteams = input$keeper_byteams,
                               OtherShots = input$keeper_othershots,
                               FK = input$keeper_fk,
                               PK = input$keeper_pk)
    } else{
      dt <- keeperxgoals.func(keeperxgoals,
                              date1 = input$keeper_date1,
                              date2 = input$keeper_date2,
                              season = min(playerxgoals$Season):max(playerxgoals$Season),
                              shotfilter = input$keeper_minshots,
                              byteams = input$keeper_byteams,
                              OtherShots = input$keeper_othershots,
                              FK = input$keeper_fk,
                              PK = input$keeper_pk)
    }
    
    datatable(dt,
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = 25,
                           lengthMenu = seq(25, 100, 25)))) %>%
      formatRound(columns = c('Dist'), 
                  digits = 1) %>%
      formatRound(columns = c('Header%', 'xG', 'G-xG'), 
                  digits = 2)
  })
  
  output$keeper_download <- downloadHandler(
    filename = 'ASAkeepertable.csv',
    
    content = function(file){
      if(input$keeper_seasonordate == 'Season'){
        dt <- keeperxgoals.func(keeperxgoals,
                                date1 = as.Date('2000-01-01'),
                                date2 = as.Date('9999-12-31'),
                                season = input$keeper_seasonfilter,
                                shotfilter = input$keeper_minshots,
                                byteams = input$keeper_byteams,
                                OtherShots = input$keeper_othershots,
                                FK = input$keeper_fk,
                                PK = input$keeper_pk)
      } else{
        dt <- keeperxgoals.func(keeperxgoals,
                                date1 = input$keeper_date1,
                                date2 = input$keeper_date2,
                                season = min(playerxgoals$Season):max(playerxgoals$Season),
                                shotfilter = input$keeper_minshots,
                                byteams = input$keeper_byteams,
                                OtherShots = input$keeper_othershots,
                                FK = input$keeper_fk,
                                PK = input$keeper_pk)
      }
      write.csv(dt, file, row.names = F)
    }
  )
  
  output$teamtotalxgoalswest <- DT::renderDataTable({
    if(input$team_seasonordate == 'Season'){
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = as.Date('2000-01-01'), 
                            date2 = as.Date('9999-12-31'),
                            season = input$team_seasonfilter,
                            even = input$team_evenstate,
                            pattern = input$team_pattern,
                            pergame = F,
                            advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                            venue = input$team_home)
      
    } else{
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = input$team_date1, 
                            date2 = input$team_date2,
                            season = min(teamxgoals$Season):max(teamxgoals$Season),
                            even = input$team_evenstate,
                            pattern = input$team_pattern,
                            pergame = F,
                            advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                            venue = input$team_home)
    }
    
    is.num <- sapply(dt, is.numeric)
    dt[is.num] <- lapply(dt[is.num], round, 2)
    
    if('Conf' %in% names(dt)){
      dt <- dt %>%
        filter(Conf == 'west') %>%
        select(-Conf)
    }
    
    datatable(dt,
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = 100,
                           dom = 't')))
  })
  
  output$teamtotalxgoalseast <- DT::renderDataTable({
    if(input$team_seasonordate == 'Season'){
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = as.Date('2000-01-01'), 
                            date2 = as.Date('9999-12-31'),
                            season = input$team_seasonfilter,
                            even = input$team_evenstate,
                            pattern = input$team_pattern,
                            pergame = F,
                            advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                            venue = input$team_home)
      
    } else{
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = input$team_date1, 
                            date2 = input$team_date2,
                            season = min(teamxgoals$Season):max(teamxgoals$Season),
                            even = input$team_evenstate,
                            pattern = input$team_pattern,
                            pergame = F,
                            advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                            venue = input$team_home)
    }
    
    is.num <- sapply(dt, is.numeric)
    dt[is.num] <- lapply(dt[is.num], round, 2)
    
    if('Conf' %in% names(dt)){
      dt <- dt %>%
        filter(Conf == 'east') %>%
        select(-Conf)
    }
    
    datatable(dt,
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = 100,
                           dom = 't')))
  })
  
  output$team_download <- downloadHandler(
    filename = 'ASAteamtable_total.csv',
    
    content = function(file){
      if(input$team_seasonordate == 'Season'){
        dt <- teamxgoals.func(teamxgoals, 
                              date1 = as.Date('2000-01-01'), 
                              date2 = as.Date('9999-12-31'),
                              season = input$team_seasonfilter,
                              even = input$team_evenstate,
                              pattern = input$team_pattern,
                              pergame = F,
                              advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                              venue = input$team_home)
        
      } else{
        dt <- teamxgoals.func(teamxgoals, 
                              date1 = input$team_date1, 
                              date2 = input$team_date2,
                              season = min(teamxgoals$Season):max(teamxgoals$Season),
                              even = input$team_evenstate,
                              pattern = input$team_pattern,
                              pergame = F,
                              advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                              venue = input$team_home)
      }
      
      write.csv(dt, file, row.names = F)
    }
  )
  
  output$teampergamexgoalswest <- DT::renderDataTable({
    if(input$team_seasonordate == 'Season'){
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = as.Date('2000-01-01'), 
                            date2 = as.Date('9999-12-31'),
                            season = input$team_seasonfilter,
                            even = input$team_evenstate,
                            pattern = input$team_pattern,
                            pergame = T,
                            advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                            venue = input$team_home)
      
    } else{
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = input$team_date1, 
                            date2 = input$team_date2,
                            season = min(teamxgoals$Season):max(teamxgoals$Season),
                            even = input$team_evenstate,
                            pattern = input$team_pattern,
                            pergame = T,
                            advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                            venue = input$team_home)
    }
    
    is.num <- sapply(dt, is.numeric)
    dt[is.num] <- lapply(dt[is.num], round, 2)
    
    if('Conf' %in% names(dt)){
      dt <- dt %>%
        filter(Conf == 'west') %>%
        select(-Conf)
    }
    
    datatable(dt,
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = 100,
                           dom = 't')))
  })
  
  output$teampergamexgoalseast <- DT::renderDataTable({
    if(input$team_seasonordate == 'Season'){
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = as.Date('2000-01-01'), 
                            date2 = as.Date('9999-12-31'),
                            season = input$team_seasonfilter,
                            even = input$team_evenstate,
                            pattern = input$team_pattern,
                            pergame = T,
                            advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                            venue = input$team_home)
      
    } else{
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = input$team_date1, 
                            date2 = input$team_date2,
                            season = min(teamxgoals$Season):max(teamxgoals$Season),
                            even = input$team_evenstate,
                            pattern = input$team_pattern,
                            pergame = T,
                            advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                            venue = input$team_home)
    }
    
    is.num <- sapply(dt, is.numeric)
    dt[is.num] <- lapply(dt[is.num], round, 2)
    
    if('Conf' %in% names(dt)){
      dt <- dt %>%
        filter(Conf == 'east') %>%
        select(-Conf)
    }
    
    datatable(dt,
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = 100,
                           dom = 't')))
  })
  
  output$team_download_pergame <- downloadHandler(
    filename = 'ASAteamtable_pergame.csv',
    
    content = function(file){
      if(input$team_seasonordate == 'Season'){
        dt <- teamxgoals.func(teamxgoals, 
                              date1 = as.Date('2000-01-01'), 
                              date2 = as.Date('9999-12-31'),
                              season = input$team_seasonfilter,
                              even = input$team_evenstate,
                              pattern = input$team_pattern,
                              pergame = T,
                              advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                              venue = input$team_home)
        
      } else{
        dt <- teamxgoals.func(teamxgoals, 
                              date1 = input$team_date1, 
                              date2 = input$team_date2,
                              season = min(teamxgoals$Season):max(teamxgoals$Season),
                              even = input$team_evenstate,
                              pattern = input$team_pattern,
                              pergame = T,
                              advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                              venue = input$team_home)
      }
      write.csv(dt, file, row.names = F)
    }
  )
  
  ## Add output for by game xgoals and download handers (copy code from above)
  output$teamxgoalsbygame <- renderDataTable({
    if(input$teambygame_seasonordate == 'Season'){
      dt <- xgbygame %>%
          filter(Season %in% input$teambygame_seasonfilter) %>%
        arrange(desc(Date))
      
    } else{
      dt <- xgbygame %>%
          filter(Date >= input$teambygame_date1, Date <= input$teambygame_date2) %>%
        arrange(desc(Date))
    }
    
    is.num <- sapply(dt, is.numeric)
    dt[is.num] <- lapply(dt[is.num], round, 2)
    
    datatable(dt,
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = 25,
                           lengthMenu = seq(25, 100, 25))))
  })
  
  output$teambygame_download <- downloadHandler(
    filename = 'ASAxGoals_gamebygame.csv',
    
    content = function(file){
      if(input$teambygame_seasonordate == 'Season'){
        dt <- xgbygame %>%
          filter(Season %in% input$teambygame_seasonfilter) %>%
          arrange(desc(Date))
        
      } else{
        dt <- xgbygame %>%
          filter(Date >= input$teambygame_date1, Date <= input$teambygame_date2) %>%
          arrange(desc(Date))
      }
      
      is.num <- sapply(dt, is.numeric)
      dt[is.num] <- lapply(dt[is.num], round, 2)
      
      write.csv(dt, file, row.names = F)
    }
  )
  
})
