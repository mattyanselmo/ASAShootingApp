
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  # Shooter reactive values ####
  shooter_inputs <- reactiveValues(shooting_seasonordate = 'Season',
                                   shooting_date1 = as.Date('2000-01-01'),
                                   shooting_date2 = as.Date('9999-12-31'),
                                   shooting_seasonfilter = max(playerxgoals$Season),
                                   shooting_minshots = 0,
                                   shooting_minkeypasses = 0,
                                   shooting_byteams = F,
                                   shooting_byseasons = T,
                                   shooting_other = T,
                                   shooting_fk = T,
                                   shooting_pk = T,
                                   shooterplot_xvar = 'xG',
                                   shooterplot_yvar = 'xA')
  
  observeEvent(input$shooting_action,
               {
                 shooter_inputs$shooting_seasonordate <- input$shooting_seasonordate
                 shooter_inputs$shooting_date1 <- input$shooting_date1
                 shooter_inputs$shooting_date2 <- input$shooting_date2
                 shooter_inputs$shooting_seasonfilter <- input$shooting_seasonfilter
                 shooter_inputs$shooting_minshots <- input$shooting_minshots
                 shooter_inputs$shooting_minkeypasses <- input$shooting_minkeypasses
                 shooter_inputs$shooting_byteams <- input$shooting_byteams
                 shooter_inputs$shooting_byseasons <- input$shooting_byseasons
                 shooter_inputs$shooting_other <- input$shooting_other
                 shooter_inputs$shooting_fk <- input$shooting_fk
                 shooter_inputs$shooting_pk <- input$shooting_pk
                 shooter_inputs$shooterplot_xvar <- input$shooterplot_xvar
                 shooter_inputs$shooterplot_yvar <- input$shooterplot_yvar
               })
  
  # Shooter plots ####
  output$shooterplot <- renderPlot({
    
    if(shooter_inputs$shooting_seasonordate == 'Season'){
      dt <- shooterxgoals.func(playerxgoals,
                               date1 = as.Date('2000-01-01'),
                               date2 = as.Date('9999-12-31'),
                               season = shooter_inputs$shooting_seasonfilter,
                               shotfilter = shooter_inputs$shooting_minshots,
                               keyfilter = shooter_inputs$shooting_minkeypasses,
                               byteams = shooter_inputs$shooting_byteams,
                               byseasons = shooter_inputs$shooting_byseasons,
                               OtherShots = shooter_inputs$shooting_other,
                               FK = shooter_inputs$shooting_fk,
                               PK = shooter_inputs$shooting_pk) %>%
        mutate(xGperShot = ifelse(Shots > 0, xG/Shots, 0),
               xAperPass = ifelse(KeyP > 0, xA/KeyP, 0))
    } else{
      dt <- shooterxgoals.func(playerxgoals,
                               date1 = shooter_inputs$shooting_date1,
                               date2 = shooter_inputs$shooting_date2,
                               season = min(playerxgoals$Season):max(playerxgoals$Season),
                               shotfilter = shooter_inputs$shooting_minshots,
                               keyfilter = shooter_inputs$shooting_minkeypasses,
                               byteams = shooter_inputs$shooting_byteams,
                               byseasons = shooter_inputs$shooting_byseasons,
                               OtherShots = shooter_inputs$shooting_other,
                               FK = shooter_inputs$shooting_fk,
                               PK = shooter_inputs$shooting_pk) %>%
        mutate(xGperShot = ifelse(Shots > 0, xG/Shots, 0),
               xAperPass = ifelse(KeyP > 0, xA/KeyP, 0))
    }
    
    dt[['extreme']] <- rank(dt[[shooter_inputs$shooterplot_xvar]]) + rank(dt[[shooter_inputs$shooterplot_yvar]])
    if(length(unique(dt$Season)) > 1){
      dt[['plotnames']] <- paste(unlist(lapply(strsplit(dt$Player, " "), function(x) { return(x[length(x)]) })), dt$Season)
      
    }else{
      dt[['plotnames']] <- unlist(lapply(strsplit(dt$Player, " "), function(x) { return(x[length(x)]) }))
    }
    
    xlim <- min(dt[[shooter_inputs$shooterplot_xvar]]) - 0.05*(max(dt[[shooter_inputs$shooterplot_xvar]]) - min(dt[[shooter_inputs$shooterplot_xvar]]))
    ylim <- min(dt[[shooter_inputs$shooterplot_yvar]]) - 0.05*(max(dt[[shooter_inputs$shooterplot_yvar]]) - min(dt[[shooter_inputs$shooterplot_yvar]]))
    
    p <- dt  %>%
      ggplot(
        aes_string(x = paste0('`', shooter_inputs$shooterplot_xvar, '`'), 
                   y = paste0('`', shooter_inputs$shooterplot_yvar, '`'))) +
      geom_point(color = '#0000cc') +
      geom_text(aes(label = ifelse(dt$extreme >= sort(dt$extreme, decreasing = T)[min(5, nrow(dt))] |
                                     dt[[shooter_inputs$shooterplot_xvar]] == max(dt[[shooter_inputs$shooterplot_xvar]]) |
                                     dt[[shooter_inputs$shooterplot_yvar]] == max(dt[[shooter_inputs$shooterplot_yvar]]),
                                   dt$plotnames, ''), 
                    hjust = 'inward'),
                size = 5,
                check_overlap = F,
                color = '#ff3300') +
      expand_limits(x = xlim,
                    y = ylim) +
      theme(legend.position = "none",
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 14))
    p + geom_smooth(method = 'lm', se = F) +
      geom_text(x = min(dt[[shooter_inputs$shooterplot_xvar]]) - 0.05*(max(dt[[shooter_inputs$shooterplot_xvar]]) - min(dt[[shooter_inputs$shooterplot_xvar]])),
                y = min(dt[[shooter_inputs$shooterplot_yvar]]) - 0.05*(max(dt[[shooter_inputs$shooterplot_yvar]]) - min(dt[[shooter_inputs$shooterplot_yvar]])),
                hjust = 0,
                label = lm_eqn(dt, 
                               paste0('`', shooter_inputs$shooterplot_xvar, '`'), 
                               paste0('`', shooter_inputs$shooterplot_yvar, '`')),
                parse = TRUE,
                color = 'black',
                size = 7)
    
    
  }, height = 500, width = 700)
  
  # Shooter tables ####
  output$shootertable <- DT::renderDataTable({
    
    if(shooter_inputs$shooting_seasonordate == 'Season'){
    dt <- shooterxgoals.func(playerxgoals,
                             date1 = as.Date('2000-01-01'),
                             date2 = as.Date('9999-12-31'),
                             season = shooter_inputs$shooting_seasonfilter,
                             shotfilter = shooter_inputs$shooting_minshots,
                             keyfilter = shooter_inputs$shooting_minkeypasses,
                             byteams = shooter_inputs$shooting_byteams,
                             byseasons = shooter_inputs$shooting_byseasons,
                             OtherShots = shooter_inputs$shooting_other,
                             FK = shooter_inputs$shooting_fk,
                             PK = shooter_inputs$shooting_pk)
    } else{
      dt <- shooterxgoals.func(playerxgoals,
                               date1 = shooter_inputs$shooting_date1,
                               date2 = shooter_inputs$shooting_date2,
                               season = min(playerxgoals$Season):max(playerxgoals$Season),
                               shotfilter = shooter_inputs$shooting_minshots,
                               keyfilter = shooter_inputs$shooting_minkeypasses,
                               byteams = shooter_inputs$shooting_byteams,
                               byseasons = shooter_inputs$shooting_byseasons,
                               OtherShots = shooter_inputs$shooting_other,
                               FK = shooter_inputs$shooting_fk,
                               PK = shooter_inputs$shooting_pk)
    }
      
    datatable(dt,
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = 25,
                           lengthMenu = seq(25, 100, 25)))) %>%
      formatRound(columns = c('Dist', 'xG', 'G-xG', 'xPlace', 'Dist.key', 'xA', 'A-xA', 'xG+xA'), 
                  digits = 1) %>%
      formatPercentage(columns = c('Solo'), digits = 1)
  })
  
  
  output$player_download <- downloadHandler(
    filename = 'ASAshootertable.csv',
    
    content = function(file){
      if(shooter_inputs$shooting_seasonordate == 'Season'){
        dt <- shooterxgoals.func(playerxgoals,
                                 date1 = as.Date('2000-01-01'),
                                 date2 = as.Date('9999-12-31'),
                                 season = shooter_inputs$shooting_seasonfilter,
                                 shotfilter = shooter_inputs$shooting_minshots,
                                 keyfilter = shooter_inputs$shooting_minkeypasses,
                                 byteams = shooter_inputs$shooting_byteams,
                                 byseasons = shooter_inputs$shooting_byseasons,
                                 FK = shooter_inputs$shooting_fk,
                                 PK = shooter_inputs$shooting_pk)
      } else{
        dt <- shooterxgoals.func(playerxgoals,
                                 date1 = shooter_inputs$shooting_date1,
                                 date2 = shooter_inputs$shooting_date2,
                                 season = min(playerxgoals$Season):max(playerxgoals$Season),
                                 shotfilter = shooter_inputs$shooting_minshots,
                                 keyfilter = shooter_inputs$shooting_minkeypasses,
                                 byteams = shooter_inputs$shooting_byteams,
                                 byseasons = shooter_inputs$shooting_byseasons,
                                 OtherShots = shooter_inputs$shooting_other,
                                 FK = shooter_inputs$shooting_fk,
                                 PK = shooter_inputs$shooting_pk)
      }
      write.csv(dt, file, row.names = F)
    }
  )
  
  # Keeper tables ####
  output$keepertable <- DT::renderDataTable({
    
    if(input$keeper_seasonordate == 'Season'){
      dt <- keeperxgoals.func(keeperxgoals,
                               date1 = as.Date('2000-01-01'),
                               date2 = as.Date('9999-12-31'),
                               season = input$keeper_seasonfilter,
                               shotfilter = input$keeper_minshots,
                               byteams = input$keeper_byteams,
                               byseasons = input$keeper_byseasons,
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
                              byseasons = input$keeper_byseasons,
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
                                byseasons = input$keeper_byseasons,
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
                                byseasons = input$keeper_byseasons,
                                OtherShots = input$keeper_othershots,
                                FK = input$keeper_fk,
                                PK = input$keeper_pk)
      }
      write.csv(dt, file, row.names = F)
    }
  )
  
  # Keeper plots ####
  #Figure out how to label these guys better!
  output$keeperplot <- renderPlot({
    if(input$keeper_seasonordate == 'Season'){
      dt <- keeperxgoals.func(keeperxgoals,
                              date1 = as.Date('2000-01-01'),
                              date2 = as.Date('9999-12-31'),
                              season = input$keeper_seasonfilter,
                              shotfilter = input$keeper_minshots,
                              byteams = input$keeper_byteams,
                              byseasons = input$keeper_byseasons,
                              OtherShots = input$keeper_othershots,
                              FK = input$keeper_fk,
                              PK = input$keeper_pk) %>%
        mutate(GAperShot = Goals/Shots, 
               xGperShot = xG/Shots,
               GmxGperShot = `G-xG`/Shots)
    } else{
      dt <- keeperxgoals.func(keeperxgoals,
                              date1 = input$keeper_date1,
                              date2 = input$keeper_date2,
                              season = min(playerxgoals$Season):max(playerxgoals$Season),
                              shotfilter = input$keeper_minshots,
                              byteams = input$keeper_byteams,
                              byseasons = input$keeper_byseasons,
                              OtherShots = input$keeper_othershots,
                              FK = input$keeper_fk,
                              PK = input$keeper_pk) %>%
        mutate(GAperShot = Goals/Shots, 
               xGperShot = xG/Shots,
               GmxGperShot = `G-xG`/Shots)
    }
    dt[['extreme']] <- rank(dt[[input$keeperplot_xvar]]) + rank(dt[[input$keeperplot_yvar]])
    if(length(unique(dt$Season)) > 1){
      dt[['plotnames']] <- paste(unlist(lapply(strsplit(dt$Keeper, " "), function(x) { return(x[length(x)]) })), dt$Season)
      
    }else{
      dt[['plotnames']] <- unlist(lapply(strsplit(dt$Keeper, " "), function(x) { return(x[length(x)]) }))
    }
    
    p <- dt  %>%
      ggplot(
        aes_string(x = paste0('`', input$keeperplot_xvar, '`'), 
                   y = paste0('`', input$keeperplot_yvar, '`'))) +
      geom_point(color = '#0000cc') +
      geom_text(aes(label = ifelse(dt$extreme >= sort(dt$extreme, decreasing = T)[min(3, nrow(dt))] |
                                     dt$extreme <= sort(dt$extreme)[min(3, nrow(dt))] |
                                     dt[[input$keeperplot_xvar]] == max(dt[[input$keeperplot_xvar]]) |
                                     dt[[input$keeperplot_yvar]] == max(dt[[input$keeperplot_yvar]]),
                                   dt$plotnames, ''), 
                    hjust = 'inward'),
                size = 5,
                check_overlap = F,
                color = '#ff3300') +
      theme(legend.position = "none",
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 14))
    p
    
  }, height = 500, width = 700)
  
  # Team reactive values ####
  team_inputs <- reactiveValues(team_seasonordate = 'Season',
                                team_date1 = as.Date('2000-01-01'),
                                team_date2 = as.Date('9999-12-31'),
                                team_seasonfilter = max(teamxgoals$Season),
                                shooting_byseasons = T,
                                team_advanced = 'Basic stats',
                                team_home = c('Home', 'Away'),
                                team_pattern =  'All',
                                team_evenstate = F,
                                teamplot_xvar = 'xG',
                                teamplot_yvar = 'xA')
  
  team_seasonordate_cond <- 'Season'
  team_seasonfilter_cond <- isolate(input$team_seasonfilter)
  
  observeEvent(input$team_action,
               {
                 team_inputs$team_seasonordate <- input$team_seasonordate
                 team_inputs$team_date1 <- input$team_date1
                 team_inputs$team_date2 <- input$team_date2
                 team_inputs$team_seasonfilter <- input$team_seasonfilter
                 team_inputs$team_byseasons <- input$team_byseasons
                 team_inputs$team_advanced <- input$team_advanced
                 team_inputs$team_home <- input$team_home
                 team_inputs$team_pattern <- input$team_pattern
                 team_inputs$team_evenstate <- input$team_evenstate
                 team_inputs$teamplot_xvar <- input$teamplot_xvar
                 team_inputs$teamplot_yvar <- input$teamplot_yvar
                 team_seasonordate_cond <- isolate(input$team_seasonordate)
                 team_seasonfilter_cond <- isolate(input$team_seasonfilter)
                 
               })
  
  output$team_seasonordate <- team_seasonordate_cond
  output$team_seasonfilter <- team_seasonfilter_cond
  outputOptions(output, "team_seasonordate_cond", suspendWhenHidden = FALSE)  
  outputOptions(output, "team_seasonfilter_cond", suspendWhenHidden = FALSE)  
  
  
  # Team tables ####
  output$teamtotalxgoalswest <- DT::renderDataTable({
    if(team_inputs$team_seasonordate == 'Season'){
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = as.Date('2000-01-01'), 
                            date2 = as.Date('9999-12-31'),
                            season = team_inputs$team_seasonfilter,
                            even = team_inputs$team_evenstate,
                            pattern = team_inputs$team_pattern,
                            pergame = F,
                            advanced = ifelse(team_inputs$team_advanced == 'Basic stats', F, T),
                            venue = team_inputs$team_home,
                            byseasons = team_inputs$team_byseasons)
      
    } else{
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = team_inputs$team_date1, 
                            date2 = team_inputs$team_date2,
                            season = min(teamxgoals$Season):max(teamxgoals$Season),
                            even = team_inputs$team_evenstate,
                            pattern = team_inputs$team_pattern,
                            pergame = F,
                            advanced = ifelse(team_inputs$team_advanced == 'Basic stats', F, T),
                            venue = team_inputs$team_home,
                            byseasons = team_inputs$team_byseasons)
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
                           pageLength = nrow(dt),
                           dom = 't')))
  })
  
  output$teamtotalxgoalseast <- DT::renderDataTable({
    if(team_inputs$team_seasonordate == 'Season'){
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = as.Date('2000-01-01'), 
                            date2 = as.Date('9999-12-31'),
                            season = team_inputs$team_seasonfilter,
                            even = team_inputs$team_evenstate,
                            pattern = team_inputs$team_pattern,
                            pergame = F,
                            advanced = ifelse(team_inputs$team_advanced == 'Basic stats', F, T),
                            venue = team_inputs$team_home,
                            byseasons = team_inputs$team_byseasons)
      
    } else{
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = team_inputs$team_date1, 
                            date2 = team_inputs$team_date2,
                            season = min(teamxgoals$Season):max(teamxgoals$Season),
                            even = team_inputs$team_evenstate,
                            pattern = team_inputs$team_pattern,
                            pergame = F,
                            advanced = ifelse(team_inputs$team_advanced == 'Basic stats', F, T),
                            venue = team_inputs$team_home,
                            byseasons = team_inputs$team_byseasons)
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
                           pageLength = nrow(dt),
                           dom = 't')))
  })
  
  output$team_download <- downloadHandler(
    filename = 'ASAteamtable_total.csv',
    
    content = function(file){
      if(team_inputs$team_seasonordate == 'Season'){
        dt <- teamxgoals.func(teamxgoals, 
                              date1 = as.Date('2000-01-01'), 
                              date2 = as.Date('9999-12-31'),
                              season = team_inputs$team_seasonfilter,
                              even = team_inputs$team_evenstate,
                              pattern = team_inputs$team_pattern,
                              pergame = F,
                              advanced = ifelse(team_inputs$team_advanced == 'Basic stats', F, T),
                              venue = team_inputs$team_home,
                              byseasons = team_inputs$team_byseasons)
        
      } else{
        dt <- teamxgoals.func(teamxgoals, 
                              date1 = team_inputs$team_date1, 
                              date2 = team_inputs$team_date2,
                              season = min(teamxgoals$Season):max(teamxgoals$Season),
                              even = team_inputs$team_evenstate,
                              pattern = team_inputs$team_pattern,
                              pergame = F,
                              advanced = ifelse(team_inputs$team_advanced == 'Basic stats', F, T),
                              venue = team_inputs$team_home,
                              byseasons = team_inputs$team_byseasons)
      }
      
      write.csv(dt, file, row.names = F)
    }
  )
  
  output$teampergamexgoalswest <- DT::renderDataTable({
    if(team_inputs$team_seasonordate == 'Season'){
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = as.Date('2000-01-01'), 
                            date2 = as.Date('9999-12-31'),
                            season = team_inputs$team_seasonfilter,
                            even = team_inputs$team_evenstate,
                            pattern = team_inputs$team_pattern,
                            pergame = T,
                            advanced = ifelse(team_inputs$team_advanced == 'Basic stats', F, T),
                            venue = team_inputs$team_home,
                            byseasons = team_inputs$team_byseasons)
      
    } else{
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = team_inputs$team_date1, 
                            date2 = team_inputs$team_date2,
                            season = min(teamxgoals$Season):max(teamxgoals$Season),
                            even = team_inputs$team_evenstate,
                            pattern = team_inputs$team_pattern,
                            pergame = T,
                            advanced = ifelse(team_inputs$team_advanced == 'Basic stats', F, T),
                            venue = team_inputs$team_home,
                            byseasons = team_inputs$team_byseasons)
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
                           pageLength = nrow(dt),
                           dom = 't')))
  })
  
  output$teampergamexgoalseast <- DT::renderDataTable({
    if(team_inputs$team_seasonordate == 'Season'){
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = as.Date('2000-01-01'), 
                            date2 = as.Date('9999-12-31'),
                            season = team_inputs$team_seasonfilter,
                            even = team_inputs$team_evenstate,
                            pattern = team_inputs$team_pattern,
                            pergame = T,
                            advanced = ifelse(team_inputs$team_advanced == 'Basic stats', F, T),
                            venue = team_inputs$team_home,
                            byseasons = team_inputs$team_byseasons)
      
    } else{
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = team_inputs$team_date1, 
                            date2 = team_inputs$team_date2,
                            season = min(teamxgoals$Season):max(teamxgoals$Season),
                            even = team_inputs$team_evenstate,
                            pattern = team_inputs$team_pattern,
                            pergame = T,
                            advanced = ifelse(team_inputs$team_advanced == 'Basic stats', F, T),
                            venue = team_inputs$team_home,
                            byseasons = team_inputs$team_byseasons)
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
                           pageLength = nrow(dt),
                           dom = 't')))
  })
  
  output$team_download_pergame <- downloadHandler(
    filename = 'ASAteamtable_pergame.csv',
    
    content = function(file){
      if(team_inputs$team_seasonordate == 'Season'){
        dt <- teamxgoals.func(teamxgoals, 
                              date1 = as.Date('2000-01-01'), 
                              date2 = as.Date('9999-12-31'),
                              season = team_inputs$team_seasonfilter,
                              even = team_inputs$team_evenstate,
                              pattern = team_inputs$team_pattern,
                              pergame = T,
                              advanced = ifelse(team_inputs$team_advanced == 'Basic stats', F, T),
                              venue = team_inputs$team_home,
                              byseasons = team_inputs$team_byseasons)
        
      } else{
        dt <- teamxgoals.func(teamxgoals, 
                              date1 = team_inputs$team_date1, 
                              date2 = team_inputs$team_date2,
                              season = min(teamxgoals$Season):max(teamxgoals$Season),
                              even = team_inputs$team_evenstate,
                              pattern = team_inputs$team_pattern,
                              pergame = T,
                              advanced = ifelse(team_inputs$team_advanced == 'Basic stats', F, T),
                              venue = team_inputs$team_home,
                              byseasons = team_inputs$team_byseasons)
      }
      write.csv(dt, file, row.names = F)
    }
  )
  
  # Team plots ####
  
  # Consider the following:
  ## Indent dropdown inputs
  ## Include trend line or labeled quadrants
  output$teamplot <- renderPlot({
    if(team_inputs$team_seasonordate == 'Season'){
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = as.Date('2000-01-01'), 
                            date2 = as.Date('9999-12-31'),
                            season = team_inputs$team_seasonfilter,
                            even = team_inputs$team_evenstate,
                            pattern = team_inputs$team_pattern,
                            pergame = T,
                            advanced = T,
                            venue = team_inputs$team_home,
                            byseasons = team_inputs$team_byseasons,
                            plot = T)
      
    } else{
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = team_inputs$team_date1, 
                            date2 = team_inputs$team_date2,
                            season = min(teamxgoals$Season):max(teamxgoals$Season),
                            even = team_inputs$team_evenstate,
                            pattern = team_inputs$team_pattern,
                            pergame = T,
                            advanced = T,
                            venue = team_inputs$team_home,
                            byseasons = team_inputs$team_byseasons,
                            plot = T)
    }
    
    # dt[['extreme']] <- rank(dt[[team_inputs$teamplot_xvar]]) + rank(dt[[team_inputs$teamplot_yvar]])
    if(length(unique(dt$Season)) > 1){
      dt[['plotnames']] <- paste(unlist(lapply(strsplit(dt$Team, " "), function(x) { return(x[length(x)]) })), dt$Season)
    }else{
      dt[['plotnames']] <- unlist(lapply(strsplit(dt$Team, " "), function(x) { return(x[length(x)]) }))
    }
    
    p <- dt  %>%
      ggplot(
        aes_string(x = paste0('`', team_inputs$teamplot_xvar, '`'), 
                   y = paste0('`', team_inputs$teamplot_yvar, '`'))) +
      geom_point(color = '#0000cc') +
      geom_text(aes(label = plotnames, 
                    hjust = 'inward'),
                size = 5,
                check_overlap = T,
                color = '#ff3300') +
      theme(legend.position = "none",
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 14))
    p + geom_smooth(method = 'lm', se = F) +
        geom_text(x = min(dt[[team_inputs$teamplot_xvar]]), 
                  y = min(dt[[team_inputs$teamplot_yvar]]),
                  hjust = 0,
                  label = lm_eqn(dt, team_inputs$teamplot_xvar, team_inputs$teamplot_yvar), 
                  parse = TRUE, 
                  color = 'black',
                  size = 7)
    
  
    
  }, height = 500, width = 700)
  
  ## XGoals by game ####
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
  
  
  # Glossary ####
  output$glossary <- renderDataTable({
    datatable(glossary %>% select(-Notes),
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = nrow(glossary),
                           dom = 'ft')))
  })
  
})
