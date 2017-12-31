
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  # Player reactive values ####
  
  # Initial values
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
                                   shooterplot_yvar = 'G-xG')
  
  shooter_per96_inputs <- reactiveValues(shooting_per96_seasonordate = 'Season',
                                         shooting_per96_date1 = as.Date('2000-01-01'),
                                         shooting_per96_date2 = as.Date('9999-12-31'),
                                         shooting_per96_seasonfilter = max(playerxgoals$Season),
                                         shooting_per96_minshots = 0,
                                         shooting_per96_minkeypasses = 0,
                                         shooting_per96_minfilter = 0,
                                         #shooting_per96_byteams = F,
                                         shooting_per96_byseasons = T,
                                         shooting_per96_other = T,
                                         shooting_per96_fk = T,
                                         shooting_per96_pk = T,
                                         shooterplot_per96_xvar = 'Min',
                                         shooterplot_per96_yvar = 'xG')
  
  # Updated values
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
  
  observeEvent(input$shooting_per96_action,
               {
                 shooter_per96_inputs$shooting_per96_seasonordate <- input$shooting_per96_seasonordate
                 shooter_per96_inputs$shooting_per96_date1 <- input$shooting_per96_date1
                 shooter_per96_inputs$shooting_per96_date2 <- input$shooting_per96_date2
                 shooter_per96_inputs$shooting_per96_seasonfilter <- input$shooting_per96_seasonfilter
                 shooter_per96_inputs$shooting_per96_minshots <- input$shooting_per96_minshots
                 shooter_per96_inputs$shooting_per96_minkeypasses <- input$shooting_per96_minkeypasses
                 shooter_per96_inputs$shooting_per96_minfilter <- input$shooting_per96_minfilter
                 #shooter_per96_inputs$shooting_per96_byteams <- input$shooting_per96_byteams
                 shooter_per96_inputs$shooting_per96_byseasons <- input$shooting_per96_byseasons
                 shooter_per96_inputs$shooting_per96_other <- input$shooting_per96_other
                 shooter_per96_inputs$shooting_per96_fk <- input$shooting_per96_fk
                 shooter_per96_inputs$shooting_per96_pk <- input$shooting_per96_pk
                 shooter_per96_inputs$shooterplot_per96_xvar <- input$shooterplot_per96_xvar
                 shooter_per96_inputs$shooterplot_per96_yvar <- input$shooterplot_per96_yvar
               })
  
  
  # Player tables ####
  dt_total <- reactive({
    if(shooter_inputs$shooting_seasonordate == 'Season'){
      dt_total <- shooterxgoals.func(playerxgoals,
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
               xAperPass = ifelse(KeyP > 0, xA/KeyP, 0),
               GmxGperShot = ifelse(Shots > 0, `G-xG`/Shots, 0),
               AmxAperPass = ifelse(KeyP > 0, `A-xA`/KeyP, 0))
    } else{
      dt_total <- shooterxgoals.func(playerxgoals,
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
               xAperPass = ifelse(KeyP > 0, xA/KeyP, 0),
               GmxGperShot = ifelse(Shots > 0, `G-xG`/Shots, 0),
               AmxAperPass = ifelse(KeyP > 0, `A-xA`/KeyP, 0))
    }
    
    dt_total[['extreme']] <- rank(dt_total[[shooter_inputs$shooterplot_xvar]]) + rank(dt_total[[shooter_inputs$shooterplot_yvar]])
    if(length(unique(dt_total$Season)) > 1){
      dt_total[['plotnames']] <- paste(unlist(lapply(strsplit(dt_total$Player, " "), function(x) { return(x[length(x)]) })), dt_total$Season)
      
    }else{
      dt_total[['plotnames']] <- unlist(lapply(strsplit(dt_total$Player, " "), function(x) { return(x[length(x)]) }))
    }
    
    dt_total
  })
  
  dt_per96 <- reactive({
    if(shooter_per96_inputs$shooting_per96_seasonordate == 'Season'){
      dt_per96 <- shooterxgoals_perminute(playerxgoals,
                                          minutes_df = minutesPlayed,
                                          date1 = as.Date('2000-01-01'),
                                          date2 = as.Date('9999-12-31'),
                                          season = shooter_per96_inputs$shooting_per96_seasonfilter,
                                          shotfilter = shooter_per96_inputs$shooting_per96_minshots,
                                          keyfilter = shooter_per96_inputs$shooting_per96_minkeypasses,
                                          minfilter = shooter_per96_inputs$shooting_per96_minfilter,
                                          byseasons = shooter_per96_inputs$shooting_per96_byseasons,
                                          OtherShots = shooter_per96_inputs$shooting_per96_other,
                                          FK = shooter_per96_inputs$shooting_per96_fk,
                                          PK = shooter_per96_inputs$shooting_per96_pk)
    } else{
      dt_per96 <- shooterxgoals_perminute(playerxgoals,
                                          minutes_df = minutesPlayed,
                                          date1 = shooter_per96_inputs$shooting_per96_date1,
                                          date2 = shooter_per96_inputs$shooting_per96_date2,
                                          season = min(playerxgoals$Season):max(playerxgoals$Season),
                                          shotfilter = shooter_per96_inputs$shooting_per96_minshots,
                                          keyfilter = shooter_per96_inputs$shooting_per96_minkeypasses,
                                          minfilter = shooter_per96_inputs$shooting_per96_minfilter,
                                          byseasons = shooter_per96_inputs$shooting_per96_byseasons,
                                          OtherShots = shooter_per96_inputs$shooting_per96_other,
                                          FK = shooter_per96_inputs$shooting_per96_fk,
                                          PK = shooter_per96_inputs$shooting_per96_pk)
    }
    
    dt_per96[['extreme']] <- rank(dt_per96[[shooter_per96_inputs$shooterplot_per96_xvar]]) + rank(dt_per96[[shooter_per96_inputs$shooterplot_per96_yvar]])
    if(length(unique(dt_per96$Season)) > 1){
      dt_per96[['plotnames']] <- paste(unlist(lapply(strsplit(dt_per96$Player, " "), function(x) { return(x[length(x)]) })), dt_per96$Season)
      
    }else{
      dt_per96[['plotnames']] <- unlist(lapply(strsplit(dt_per96$Player, " "), function(x) { return(x[length(x)]) }))
    }
    
    dt_per96
    
  })
  
  # Player table - totals
  output$shootertable <- DT::renderDataTable({
    
    datatable(dt_total() %>% select(-c(Dist.key, xGperShot, xAperPass, GmxGperShot, AmxAperPass, extreme, plotnames)),
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = 25,
                           lengthMenu = seq(25, 100, 25)))) %>%
      formatRound(columns = c('Dist', 'xG', 'G-xG', 'xPlace', 'xA', 'A-xA', 'xG+xA'), 
                  digits = 1) %>%
      formatPercentage(columns = c('Solo'), digits = 1)
  })
  
  # Player table - per96
  output$shootertable_per96 <- DT::renderDataTable({
    
    
    datatable(dt_per96() %>% select(-c(extreme, plotnames)),
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = 25,
                           lengthMenu = seq(25, 100, 25)))) %>%
      formatRound(columns = c("Shots", "SoT", "Goals", "xG", "xPlace", "G-xG", "KeyP", "Assts", "xA", "A-xA", "xG+xA"), 
                  digits = 2)
  })
  
  # Player plots ####
  output$shooterplot <- renderPlot({
    xlim <- min(dt_total()[[shooter_inputs$shooterplot_xvar]]) - 0.05*(max(dt_total()[[shooter_inputs$shooterplot_xvar]]) - min(dt_total()[[shooter_inputs$shooterplot_xvar]]))
    ylim <- min(dt_total()[[shooter_inputs$shooterplot_yvar]]) - 0.05*(max(dt_total()[[shooter_inputs$shooterplot_yvar]]) - min(dt_total()[[shooter_inputs$shooterplot_yvar]]))
    
    p <- dt_total() %>%
      ggplot(
        aes_string(x = paste0('`', shooter_inputs$shooterplot_xvar, '`'), 
                   y = paste0('`', shooter_inputs$shooterplot_yvar, '`'))) +
      geom_point(color = '#0000cc') +
      geom_text(aes(label = ifelse(dt_total()$extreme >= sort(dt_total()$extreme, decreasing = T)[min(5, nrow(dt_total()))] |
                                     dt_total()[[shooter_inputs$shooterplot_xvar]] == max(dt_total()[[shooter_inputs$shooterplot_xvar]]) |
                                     dt_total()[[shooter_inputs$shooterplot_yvar]] == max(dt_total()[[shooter_inputs$shooterplot_yvar]]),
                                   dt_total()$plotnames, ''), 
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
      geom_text(x = min(dt_total()[[shooter_inputs$shooterplot_xvar]]) - 0.05*(max(dt_total()[[shooter_inputs$shooterplot_xvar]]) - min(dt_total()[[shooter_inputs$shooterplot_xvar]])),
                y = min(dt_total()[[shooter_inputs$shooterplot_yvar]]) - 0.05*(max(dt_total()[[shooter_inputs$shooterplot_yvar]]) - min(dt_total()[[shooter_inputs$shooterplot_yvar]])),
                hjust = 0,
                label = lm_eqn(dt_total(), 
                               paste0('`', shooter_inputs$shooterplot_xvar, '`'), 
                               paste0('`', shooter_inputs$shooterplot_yvar, '`')),
                parse = TRUE,
                color = 'black',
                size = 7)
    
    
  }, height = 500, width = 700)
  
  output$shooterplot_per96 <- renderPlot({
    xlim <- min(dt_per96()[[shooter_inputs$shooterplot_xvar]]) - 0.05*(max(dt_per96()[[shooter_inputs$shooterplot_xvar]]) - min(dt_per96()[[shooter_inputs$shooterplot_xvar]]))
    ylim <- min(dt_per96()[[shooter_inputs$shooterplot_yvar]]) - 0.05*(max(dt_per96()[[shooter_inputs$shooterplot_yvar]]) - min(dt_per96()[[shooter_inputs$shooterplot_yvar]]))
    
    p <- dt_per96() %>%
      ggplot(
        aes_string(x = paste0('`', shooter_per96_inputs$shooterplot_per96_xvar, '`'), 
                   y = paste0('`', shooter_per96_inputs$shooterplot_per96_yvar, '`'))) +
      geom_point(color = '#0000cc') +
      geom_text(aes(label = ifelse(dt_per96()$extreme >= sort(dt_per96()$extreme, decreasing = T)[min(5, nrow(dt_per96()))] |
                                     dt_per96()[[shooter_per96_inputs$shooterplot_per96_xvar]] == max(dt_per96()[[shooter_per96_inputs$shooterplot_per96_xvar]]) |
                                     dt_per96()[[shooter_per96_inputs$shooterplot_per96_yvar]] == max(dt_per96()[[shooter_per96_inputs$shooterplot_per96_yvar]]),
                                   dt_per96()$plotnames, ''), 
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
      geom_text(x = min(dt_per96()[[shooter_per96_inputs$shooterplot_per96_xvar]]) - 0.05*(max(dt_per96()[[shooter_per96_inputs$shooterplot_per96_xvar]]) - min(dt_per96()[[shooter_per96_inputs$shooterplot_per96_xvar]])),
                y = min(dt_per96()[[shooter_per96_inputs$shooterplot_per96_yvar]]) - 0.05*(max(dt_per96()[[shooter_per96_inputs$shooterplot_per96_yvar]]) - min(dt_per96()[[shooter_per96_inputs$shooterplot_per96_yvar]])),
                hjust = 0,
                label = lm_eqn(dt_per96(), 
                               paste0('`', shooter_per96_inputs$shooterplot_per96_xvar, '`'), 
                               paste0('`', shooter_per96_inputs$shooterplot_per96_yvar, '`')),
                parse = TRUE,
                color = 'black',
                size = 7)
  }, height = 500, width = 700)
  
  
  # Player downloads ####
  output$player_download <- downloadHandler(
    filename = 'ASAplayertable_totals.csv',
    
    content = function(file){
      
      write.csv(dt_total() %>% select(-c(extreme, plotnames)), file, row.names = F)
    }
  )
  
  output$player_per96_download <- downloadHandler(
    filename = 'ASAplayertable_per96.csv',
    
    content = function(file){
      
      write.csv(dt_per96() %>% select(-c(extreme, plotnames)), file, row.names = F)
    }
  )
  
  # Keeper reactive values ####
  
  # Initial values
  keeper_inputs <- reactiveValues(keeper_minshots = 0,
                                  keeper_date1 = as.Date('2000-01-01'),
                                  keeper_date2 = as.Date('9999-12-31'),
                                  keeper_seasonfilter = max(keeperxgoals$Season),
                                  keeper_seasonordate = 'Season',
                                  keeper_byseasons = T,
                                  keeper_byteams = F,
                                  keeper_pk = T,
                                  keeper_fk = T,
                                  keeper_othershots = T,
                                  keeperplot_xvar = 'xG',
                                  keeperplot_yvar = 'G-xG')
  
  # Updated values
  observeEvent(input$keeper_action,
               {
                 keeper_inputs$keeper_minshots <- input$keeper_minshots
                 keeper_inputs$keeper_date1 <- input$keeper_date1
                 keeper_inputs$keeper_date2 <- input$keeper_date2
                 keeper_inputs$keeper_seasonfilter <- input$keeper_seasonfilter
                 keeper_inputs$keeper_seasonordate <- input$keeper_seasonordate
                 keeper_inputs$keeper_byseasons <- input$keeper_byseasons
                 keeper_inputs$keeper_byteams <- input$keeper_byteams
                 keeper_inputs$keeper_pk <- input$keeper_pk
                 keeper_inputs$keeper_fk <- input$keeper_fk
                 keeper_inputs$keeper_othershots <- input$keeper_othershots
                 keeper_inputs$keeperplot_xvar <- input$keeperplot_xvar
                 keeper_inputs$keeperplot_yvar <- input$keeperplot_yvar
               })
  
  # Keeper tables ####
  dt_keeper <- reactive({
    if(keeper_inputs$keeper_seasonordate == 'Season'){
      dt_keeper <- keeperxgoals.func(keeperxgoals,
                              date1 = as.Date('2000-01-01'),
                              date2 = as.Date('9999-12-31'),
                              season = keeper_inputs$keeper_seasonfilter,
                              shotfilter = keeper_inputs$keeper_minshots,
                              byteams = keeper_inputs$keeper_byteams,
                              byseasons = keeper_inputs$keeper_byseasons,
                              OtherShots = keeper_inputs$keeper_othershots,
                              FK = keeper_inputs$keeper_fk,
                              PK = keeper_inputs$keeper_pk) %>%
        mutate(GAperShot = Goals/Shots, 
               xGperShot = xG/Shots,
               GmxGperShot = `G-xG`/Shots)
    } else{
      dt_keeper <- keeperxgoals.func(keeperxgoals,
                              date1 = keeper_inputs$keeper_date1,
                              date2 = keeper_inputs$keeper_date2,
                              season = min(playerxgoals$Season):max(playerxgoals$Season),
                              shotfilter = keeper_inputs$keeper_minshots,
                              byteams = keeper_inputs$keeper_byteams,
                              byseasons = keeper_inputs$keeper_byseasons,
                              OtherShots = keeper_inputs$keeper_othershots,
                              FK = keeper_inputs$keeper_fk,
                              PK = keeper_inputs$keeper_pk) %>%
        mutate(GAperShot = Goals/Shots, 
               xGperShot = xG/Shots,
               GmxGperShot = `G-xG`/Shots)
    }
    
    dt_keeper[['extreme']] <- rank(dt_keeper[[keeper_inputs$keeperplot_yvar]]) # Only show most extreme across y-axis
    if(length(unique(dt_keeper$Season)) > 1){
      dt_keeper[['plotnames']] <- paste(unlist(lapply(strsplit(dt_keeper$Keeper, " "), function(x) { return(x[length(x)]) })), dt_keeper$Season)
      
    }else{
      dt_keeper[['plotnames']] <- unlist(lapply(strsplit(dt_keeper$Keeper, " "), function(x) { return(x[length(x)]) }))
    }
    dt_keeper
  })
  
  output$keepertable <- DT::renderDataTable({
    
    datatable(dt_keeper() %>% select(-c(GAperShot, xGperShot, GmxGperShot, extreme, plotnames)),
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
      write.csv(dt_keeper() %>% select(-c(extreme, plotnames)), file, row.names = F)
    }
  )
  
  # Keeper plots ####
  #Figure out how to label these guys better!
  output$keeperplot <- renderPlot({
   
   xlim <- min(dt_keeper()[[keeper_inputs$keeperplot_xvar]]) - 0.05*(max(dt_keeper()[[keeper_inputs$keeperplot_xvar]]) - min(dt_keeper()[[keeper_inputs$keeperplot_xvar]]))
   ylim <- min(dt_keeper()[[keeper_inputs$keeperplot_yvar]]) - 0.05*(max(dt_keeper()[[keeper_inputs$keeperplot_yvar]]) - min(dt_keeper()[[keeper_inputs$keeperplot_yvar]]))
    
    p <- dt_keeper()  %>%
      ggplot(
        aes_string(x = paste0('`', keeper_inputs$keeperplot_xvar, '`'), 
                   y = paste0('`', keeper_inputs$keeperplot_yvar, '`'))) +
      geom_point(color = '#0000cc') +
      geom_text(aes(label = ifelse(dt_keeper()$extreme >= sort(dt_keeper()$extreme, decreasing = T)[min(3, nrow(dt_keeper()))] |
                                     dt_keeper()$extreme <= sort(dt_keeper()$extreme)[min(3, nrow(dt_keeper()))] |
                                     dt_keeper()[[keeper_inputs$keeperplot_xvar]] == max(dt_keeper()[[keeper_inputs$keeperplot_xvar]]) |
                                     dt_keeper()[[keeper_inputs$keeperplot_yvar]] == max(dt_keeper()[[keeper_inputs$keeperplot_yvar]]),
                                   dt_keeper()$plotnames, ''), 
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
      geom_text(x = min(dt_keeper()[[keeper_inputs$keeperplot_xvar]]) - 0.05*(max(dt_keeper()[[keeper_inputs$keeperplot_xvar]]) - min(dt_keeper()[[keeper_inputs$keeperplot_xvar]])),
                y = min(dt_keeper()[[keeper_inputs$keeperplot_yvar]]) - 0.05*(max(dt_keeper()[[keeper_inputs$keeperplot_yvar]]) - min(dt_keeper()[[keeper_inputs$keeperplot_yvar]])),
                hjust = 0,
                label = lm_eqn(dt_keeper(), 
                               paste0('`', keeper_inputs$keeperplot_xvar, '`'), 
                               paste0('`', keeper_inputs$keeperplot_yvar, '`')),
                parse = TRUE,
                color = 'black',
                size = 7)
    
  }, height = 500, width = 700)
  
  # Team tables ####
  
  dt_team <- reactive({
    if(input$team_seasonordate == 'Season'){
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = as.Date('2000-01-01'), 
                            date2 = as.Date('9999-12-31'),
                            season = input$team_seasonfilter,
                            even = input$team_evenstate,
                            pattern = input$team_pattern,
                            pergame = F,
                            advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                            venue = input$team_home,
                            byseasons = input$team_byseasons,
                            confview = input$team_conferenceview)
      
    } else{
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = input$team_date1, 
                            date2 = input$team_date2,
                            season = min(teamxgoals$Season):max(teamxgoals$Season),
                            even = input$team_evenstate,
                            pattern = input$team_pattern,
                            pergame = F,
                            advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                            venue = input$team_home,
                            byseasons = input$team_byseasons,
                            confview = input$team_conferenceview)
    }
    
    is.num <- sapply(dt, is.numeric)
    dt[is.num] <- lapply(dt[is.num], round, 3)
    
    dt
  })
  
  output$teamtotalxgoalswest <- DT::renderDataTable({
    dt <- dt_team()
    
    if('Conf' %in% names(dt)){
      dt <- dt %>%
        filter(Conf == 'west') %>%
        select(-Conf)
    }
    
    DT::datatable(dt,
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = 25,
                           dom = 't'))) %>%
      formatPercentage(columns = c('SoT%F', 'SoT%A', 'Finish%F', 'Finish%A'), digits = 1)
  })
  
  output$teamtotalxgoalseast <- DT::renderDataTable({
    dt <- dt_team()
    
    if('Conf' %in% names(dt)){
      dt <- dt %>%
        filter(Conf == 'east') %>%
        select(-Conf)
    }
    
    DT::datatable(dt,
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = 25,
                           dom = 't'))) %>%
      formatPercentage(columns = c('SoT%F', 'SoT%A', 'Finish%F', 'Finish%A'), digits = 1)
  })
  
  output$team_download <- downloadHandler(
    filename = 'ASAteamtable_total.csv',
    
    content = function(file){
      write.csv(dt_team(), file, row.names = F)
    }
  )
  
  # Per game team stats
  dt_team_pergame <- reactive({
    if(input$team_seasonordate == 'Season'){
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = as.Date('2000-01-01'), 
                            date2 = as.Date('9999-12-31'),
                            season = input$team_seasonfilter,
                            even = input$team_evenstate,
                            pattern = input$team_pattern,
                            pergame = T,
                            advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                            venue = input$team_home,
                            byseasons = input$team_byseasons,
                            confview = input$team_conferenceview)
      
    } else{
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = input$team_date1, 
                            date2 = input$team_date2,
                            season = min(teamxgoals$Season):max(teamxgoals$Season),
                            even = input$team_evenstate,
                            pattern = input$team_pattern,
                            pergame = T,
                            advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                            venue = input$team_home,
                            byseasons = input$team_byseasons,
                            confview = input$team_conferenceview)
    }
    
    is.num <- sapply(dt, is.numeric)
    dt[is.num] <- lapply(dt[is.num], round, 3)
    dt
  })
  
  output$teampergamexgoalswest <- DT::renderDataTable({
    
    if('Conf' %in% names(dt_team_pergame())){
      dt <- dt_team_pergame() %>%
        filter(Conf == 'west') %>%
        select(-Conf)
    } else{
      dt <- dt_team_pergame()
    }
    
    datatable(dt,
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = 25,
                           dom = 't'))) %>%
      formatPercentage(columns = c('SoT%F', 'SoT%A', 'Finish%F', 'Finish%A'), digits = 1) %>%
      formatRound(columns = c('ShtF', 'ShtA', 'SoTF', 'SoTA'), digits = 1) %>%
      formatRound(columns = c('GF', 'GA', 'GD', 'Pts'), digits = 2)
  })
  
  output$teampergamexgoalseast <- DT::renderDataTable({
  
    if('Conf' %in% names(dt_team_pergame())){
      dt <- dt_team_pergame() %>%
        filter(Conf == 'east') %>%
        select(-Conf)
    } else{
      dt <- dt_team_pergame()
    }
    
    datatable(dt,
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = 25,
                           dom = 't'))) %>%
      formatPercentage(columns = c('SoT%F', 'SoT%A', 'Finish%F', 'Finish%A'), digits = 1)%>%
      formatRound(columns = c('ShtF', 'ShtA', 'SoTF', 'SoTA'), digits = 1) %>%
      formatRound(columns = c('GF', 'GA', 'GD', 'Pts'), digits = 2)
  })
  
  output$team_download_pergame <- downloadHandler(
    filename = 'ASAteamtable_pergame.csv',
    
    content = function(file){
      write.csv(dt_team_pergame(), file, row.names = F)
    }
  )
  
  # Team reactive values ####
  team_inputs <- reactiveValues(team_seasonordate = 'Season',
                                team_date1 = as.Date('2000-01-01'),
                                team_date2 = as.Date('9999-12-31'),
                                team_seasonfilter = max(teamxgoals$Season),
                                team_byseasons = T,
                                team_conferenceview = T,
                                team_advanced = 'Basic stats',
                                team_home = c('Home', 'Away'),
                                team_pattern =  'All',
                                team_evenstate = F,
                                teamplot_xvar = 'xG',
                                teamplot_yvar = 'xA')
  
  observeEvent(input$team_action,
               {
                 team_inputs$team_seasonordate <- input$team_seasonordate
                 team_inputs$team_date1 <- input$team_date1
                 team_inputs$team_date2 <- input$team_date2
                 team_inputs$team_seasonfilter <- input$team_seasonfilter
                 team_inputs$team_conferenceview <- input$team_conferenceview
                 team_inputs$team_byseasons <- input$team_byseasons
                 team_inputs$team_advanced <- input$team_advanced
                 team_inputs$team_home <- input$team_home
                 team_inputs$team_pattern <- input$team_pattern
                 team_inputs$team_evenstate <- input$team_evenstate
                 team_inputs$teamplot_xvar <- input$teamplot_xvar
                 team_inputs$teamplot_yvar <- input$teamplot_yvar
                 
               })
  
  # Team plots ####
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
  output$teamxgoalsbygame <- DT::renderDataTable({
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
  output$glossary <- DT::renderDataTable({
    datatable(glossary %>% select(-Notes),
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = nrow(glossary),
                           dom = 'ft')))
  })
  
})