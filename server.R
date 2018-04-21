
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  # Shooter reactive values ####
  
  # Initial values
  shooter_inputs <- reactiveValues(shooting_seasonordate = 'Season',
                                   shooting_date1 = as.Date('2000-01-01'),
                                   shooting_date2 = as.Date('9999-12-31'),
                                   shooting_seasonfilter = max(playerxgoals$Season),
                                   shooting_minfilter = 0,
                                   shooting_minshots = 0,
                                   shooting_minkeypasses = 0,
                                   shooting_position = c("G", "D", "B", "M", "A", "F", "S"),
                                   shooting_byteams = F,
                                   shooting_byseasons = T,
                                   shooting_other = T,
                                   shooting_fk = T,
                                   shooting_pk = T,
                                   shooterplot_xvar = 'xG',
                                   shooterplot_yvar = 'G-xG',
                                   shooterplot_per96_xvar = 'xG',
                                   shooterplot_per96_yvar = 'G-xG')
  
  # Updated values
  observeEvent(input$shooting_action,
               {
                 shooter_inputs$shooting_seasonordate <- input$shooting_seasonordate
                 shooter_inputs$shooting_date1 <- input$shooting_date1
                 shooter_inputs$shooting_date2 <- input$shooting_date2
                 shooter_inputs$shooting_seasonfilter <- input$shooting_seasonfilter
                 shooter_inputs$shooting_minfilter <- input$shooting_minfilter
                 shooter_inputs$shooting_minshots <- input$shooting_minshots
                 shooter_inputs$shooting_minkeypasses <- input$shooting_minkeypasses
                 shooter_inputs$shooting_position <- input$shooting_position
                 shooter_inputs$shooting_byteams <- input$shooting_byteams
                 shooter_inputs$shooting_byseasons <- input$shooting_byseasons
                 shooter_inputs$shooting_other <- input$shooting_other
                 shooter_inputs$shooting_fk <- input$shooting_fk
                 shooter_inputs$shooting_pk <- input$shooting_pk
                 shooter_inputs$shooterplot_xvar <- input$shooterplot_xvar
                 shooter_inputs$shooterplot_yvar <- input$shooterplot_yvar
                 shooter_inputs$shooterplot_per96_xvar <- input$shooterplot_per96_xvar
                 shooter_inputs$shooterplot_per96_yvar <- input$shooterplot_per96_yvar
               })
  
  # Shooter tables ####
  dt_total <- reactive({
    if(shooter_inputs$shooting_seasonordate == 'Season'){
      dt_total <- shooterxgoals.func(playerxgoals,
                                     date1 = as.Date('2000-01-01'),
                                     date2 = as.Date('9999-12-31'),
                                     season = shooter_inputs$shooting_seasonfilter,
                                     minfilter = shooter_inputs$shooting_minfilter,
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
                                     minfilter = shooter_inputs$shooting_minfilter,
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
    
    if("Pos" %in% names(dt_total)){
      dt_total %>% filter(Pos %in% shooter_inputs$shooting_position)
    } else{
      dt_total
    }
  })
  
  dt_per96 <- reactive({
    if(shooter_inputs$shooting_seasonordate == 'Season'){
      dt_per96 <- shooterxgoals_perminute(playerxgoals,
                                          minutes_df = minutesPlayed,
                                          date1 = as.Date('2000-01-01'),
                                          date2 = as.Date('9999-12-31'),
                                          season = shooter_inputs$shooting_seasonfilter[shooter_inputs$shooting_seasonfilter >= 2015],
                                          shotfilter = shooter_inputs$shooting_minshots,
                                          keyfilter = shooter_inputs$shooting_minkeypasses,
                                          minfilter = shooter_inputs$shooting_minfilter,
                                          byseasons = shooter_inputs$shooting_byseasons,
                                          byteams = shooter_inputs$shooting_byteams,
                                          OtherShots = shooter_inputs$shooting_other,
                                          FK = shooter_inputs$shooting_fk,
                                          PK = shooter_inputs$shooting_pk)
    } else{
      dt_per96 <- shooterxgoals_perminute(playerxgoals,
                                          minutes_df = minutesPlayed,
                                          date1 = max(shooter_inputs$shooting_date1, as.Date("2015-01-01")),
                                          date2 = max(shooter_inputs$shooting_date2, as.Date("2015-01-01")),
                                          season = min(playerxgoals$Season):max(playerxgoals$Season),
                                          shotfilter = shooter_inputs$shooting_minshots,
                                          keyfilter = shooter_inputs$shooting_minkeypasses,
                                          minfilter = shooter_inputs$shooting_minfilter,
                                          byseasons = shooter_inputs$shooting_byseasons,
                                          byteams = shooter_inputs$shooting_byteams,
                                          OtherShots = shooter_inputs$shooting_other,
                                          FK = shooter_inputs$shooting_fk,
                                          PK = shooter_inputs$shooting_pk)
    }
    
    dt_per96[['extreme']] <- rank(dt_per96[[shooter_inputs$shooterplot_xvar]]) + rank(dt_per96[[shooter_inputs$shooterplot_yvar]])
    if(length(unique(dt_per96$Season)) > 1){
      dt_per96[['plotnames']] <- paste(unlist(lapply(strsplit(dt_per96$Player, " "), function(x) { return(x[length(x)]) })), dt_per96$Season)
      
    }else{
      dt_per96[['plotnames']] <- unlist(lapply(strsplit(dt_per96$Player, " "), function(x) { return(x[length(x)]) }))
    }
    
    dt_per96 %>% filter(Pos %in% shooter_inputs$shooting_position)
    
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
  
  # Shooter plots ####
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
        aes_string(x = paste0('`', shooter_inputs$shooterplot_per96_xvar, '`'), 
                   y = paste0('`', shooter_inputs$shooterplot_per96_yvar, '`'))) +
      geom_point(color = '#0000cc') +
      geom_text(aes(label = ifelse(dt_per96()$extreme >= sort(dt_per96()$extreme, decreasing = T)[min(5, nrow(dt_per96()))] |
                                     dt_per96()[[shooter_inputs$shooterplot_per96_xvar]] == max(dt_per96()[[shooter_inputs$shooterplot_per96_xvar]]) |
                                     dt_per96()[[shooter_inputs$shooterplot_per96_yvar]] == max(dt_per96()[[shooter_inputs$shooterplot_per96_yvar]]),
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
      geom_text(x = min(dt_per96()[[shooter_inputs$shooterplot_per96_xvar]]) - 0.05*(max(dt_per96()[[shooter_inputs$shooterplot_per96_xvar]]) - min(dt_per96()[[shooter_inputs$shooterplot_per96_xvar]])),
                y = min(dt_per96()[[shooter_inputs$shooterplot_per96_yvar]]) - 0.05*(max(dt_per96()[[shooter_inputs$shooterplot_per96_yvar]]) - min(dt_per96()[[shooter_inputs$shooterplot_per96_yvar]])),
                hjust = 0,
                label = lm_eqn(dt_per96(), 
                               paste0('`', shooter_inputs$shooterplot_per96_xvar, '`'), 
                               paste0('`', shooter_inputs$shooterplot_per96_yvar, '`')),
                parse = TRUE,
                color = 'black',
                size = 7)
  }, height = 500, width = 700)
  
  # Shooter downloads ####
  output$player_download <- downloadHandler(
    filename = paste0("ASAplayertable_", 
                      ifelse(input$player_subtab %in% c("tablestotals", "plotstotals"), "totals", "per96"),
                      ".csv"),
    
    content = function(file){
      if(input$player_subtab %in% c("tablestotals", "plotstotals")){
        namesFL <- as.data.frame(do.call("rbind", strsplit(sub(" ", ";", dt_total()$Player), ";")))
        names(namesFL) <- c("First", "Last")
        write.csv(data.frame(namesFL, dt_total() %>% select(-c(extreme, plotnames))), file, row.names = F)
      } else{
        namesFL <- as.data.frame(do.call("rbind", strsplit(sub(" ", ";", dt_per96()$Player), ";")))
        names(namesFL) <- c("First", "Last")
        write.csv(data.frame(namesFL, dt_per96() %>% select(-c(extreme, plotnames))), file, row.names = F)        
      }
    }
  )
  
  # Passer reactive values ####
  
  # Initial values
  passer_inputs <- reactiveValues(passing_position = c("G", "D", "B", "M", "A", "F", "S", "Heaven"),
                                  passing_third = "All",
                                  passing_seasonfilter = max(playerxgoals$Season),
                                  passing_minpasses = 0,
                                  passing_minfilter = 0,
                                  passing_byteams = F,
                                  passing_byseasons = T,
                                  passerplot_xvar = 'xG',
                                  passerplot_yvar = 'G-xG')
  
  # passer_per96_inputs <- reactiveValues(passing_seasonfilter = max(playerxgoals$Season),
  #                                 passing_per96_minpasses = 0,
  #                                 passing_per96_minfilter = 0,
  #                                 passing_per96_byteams = F,
  #                                 passing_per96_byseasons = T,
  #                                 passerplot_per96_xvar = 'xG',
  #                                 passerplot_per96_yvar = 'G-xG')
  
  # Updated values
  observeEvent(input$passing_action,
               {
                 passer_inputs$passing_position <- input$passing_position
                 passer_inputs$passing_third <- input$passing_third
                 passer_inputs$passing_seasonfilter <- input$passing_seasonfilter
                 passer_inputs$passing_minpasses <- input$passing_minpasses
                 passer_inputs$passing_minfilter <- input$passing_minfilter
                 passer_inputs$passing_byteams <- input$passing_byteams
                 passer_inputs$passing_byseasons <- input$passing_byseasons
                 passer_inputs$passerplot_xvar <- input$passerplot_xvar
                 passer_inputs$passerplot_yvar <- input$passerplot_yvar
               })
  
  # observeEvent(input$passing_per96_action,
  #              {
  #                passer_per96_inputs$passing_per96_seasonfilter <- input$passing_per96_seasonfilter
  #                passer_per96_inputs$passing_per96_minpasses <- input$passing_per96_minpasses
  #                passer_per96_inputs$passing_per96_minfilter <- input$passing_per96_minfilter
  #                passer_per96_inputs$passing_per96_byteams <- input$passing_per96_byteams
  #                passer_per96_inputs$passing_per96_byseasons <- input$passing_per96_byseasons
  #                passer_per96_inputs$passerplot_per96_xvar <- input$passerplot_per96_xvar
  #                passer_per96_inputs$passerplot_per96_yvar <- input$passerplot_per96_yvar
  #              })
  
  # Passer tables ####
  dt_passer <- reactive({
    dt <- passer.xpasses(playerpassing,
                         minpasses = passer_inputs$passing_minpasses,
                         minfilter = passer_inputs$passing_minfilter,
                         seasonfilter = passer_inputs$passing_seasonfilter,
                         byteams = passer_inputs$passing_byteams,
                         byseasons = passer_inputs$passing_byseasons,
                         third.filter = passer_inputs$passing_third,
                         pos.filter = passer_inputs$passing_position)
    
    dt
    # Append passer names and extreme obs for plotting?
  })
  
  dt_passer_per96 <- reactive({
    # NEW PASSING FUNCTION(minfilter, minpasses, seasonfilter, byteams, byseasons)
  })
  
  output$passingtable_player <- DT::renderDataTable({
    DT::datatable(dt_passer(),
                  rownames = F,
                  options(list(autoWidth = T,
                               pageLength = 25,
                               lengthMenu = seq(25, 100, 25)))) %>%
      formatRound(columns = c("Score", "Per100", "Distance", "Vertical"), 
                  digits = 1) %>%
      formatPercentage(columns = c("PassPct", "xPassPct", "Touch%")[c(T, T, passer_inputs$passing_third == "All")], 
                       digits = 1)
  })
  
  # Passer plots ####
  
  # Passer downloads ####
  output$passing_download <- downloadHandler(
    filename = 'ASApassertable_totals.csv',
    
    content = function(file){
      namesFL <- as.data.frame(do.call("rbind", strsplit(sub(" ", ";", dt_passer()$Player), ";")))
      names(namesFL) <- c("First", "Last")
      write.csv(data.frame(namesFL, dt_passer()), file, row.names = F)
    }
  )
  
  # output$passer_per96_download <- downloadHandler(
  #   filename = 'ASApassertable_per96.csv',
  #   
  #   content = function(file){
  #     
  #     write.csv(dt_passer_per96(), file, row.names = F)
  #   }
  # )
  # Keeper reactive values ####
  
  # Initial values
  keeper_inputs <- reactiveValues(keeper_minshots = 0,
                                  keeper_minfilter = 0,
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
                 keeper_inputs$keeper_minfilter <- input$keeper_minfilter
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
                                     minutes_df = minutesPlayed,
                                     date1 = as.Date('2000-01-01'),
                                     date2 = as.Date('9999-12-31'),
                                     season = keeper_inputs$keeper_seasonfilter,
                                     shotfilter = keeper_inputs$keeper_minshots,
                                     minfilter = keeper_inputs$keeper_minfilter,
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
                                     minutes_df = minutesPlayed,
                                     date1 = keeper_inputs$keeper_date1,
                                     date2 = keeper_inputs$keeper_date2,
                                     season = min(playerxgoals$Season):max(playerxgoals$Season),
                                     shotfilter = keeper_inputs$keeper_minshots,
                                     minfilter = keeper_inputs$keeper_minfilter,
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
  
  dt_keeper_per96 <- reactive({
    if(keeper_inputs$keeper_seasonordate == 'Season'){
      dt_keeper_per96 <- keeperxgoals_per96.func(keeperxgoals,
                                           minutes_df = minutesPlayed,
                                           date1 = as.Date('2000-01-01'),
                                           date2 = as.Date('9999-12-31'),
                                           season = keeper_inputs$keeper_seasonfilter[keeper_inputs$keeper_seasonfilter >= 2015],
                                           shotfilter = keeper_inputs$keeper_minshots,
                                           minfilter = keeper_inputs$keeper_minfilter,
                                           byteams = keeper_inputs$keeper_byteams,
                                           byseasons = keeper_inputs$keeper_byseasons,
                                           OtherShots = keeper_inputs$keeper_othershots,
                                           FK = keeper_inputs$keeper_fk,
                                           PK = keeper_inputs$keeper_pk) %>%
        mutate(GAperShot = Goals/Shots, 
               xGperShot = xG/Shots,
               GmxGperShot = `G-xG`/Shots)
    } else{
      dt_keeper_per96 <- keeperxgoals_per96.func(keeperxgoals,
                                     minutes_df = minutesPlayed,
                                     date1 = max(keeper_inputs$keeper_date1, as.Date("2015-01-01")),
                                     date2 = max(keeper_inputs$keeper_date2, as.Date("2015-01-01")),
                                     season = min(playerxgoals$Season):max(playerxgoals$Season),
                                     shotfilter = keeper_inputs$keeper_minshots,
                                     minfilter = keeper_inputs$keeper_minfilter,
                                     byteams = keeper_inputs$keeper_byteams,
                                     byseasons = keeper_inputs$keeper_byseasons,
                                     OtherShots = keeper_inputs$keeper_othershots,
                                     FK = keeper_inputs$keeper_fk,
                                     PK = keeper_inputs$keeper_pk) %>%
        mutate(GAperShot = Goals/Shots, 
               xGperShot = xG/Shots,
               GmxGperShot = `G-xG`/Shots)
    }
    
    dt_keeper_per96[['extreme']] <- rank(dt_keeper_per96[[keeper_inputs$keeperplot_yvar]]) # Only show most extreme across y-axis
    if(length(unique(dt_keeper_per96$Season)) > 1){
      dt_keeper_per96[['plotnames']] <- paste(unlist(lapply(strsplit(dt_keeper_per96$Keeper, " "), function(x) { return(x[length(x)]) })), dt_keeper_per96$Season)
      
    }else{
      dt_keeper_per96[['plotnames']] <- unlist(lapply(strsplit(dt_keeper_per96$Keeper, " "), function(x) { return(x[length(x)]) }))
    }
    dt_keeper_per96
  })
  
  output$keepertable <- DT::renderDataTable({
    
    datatable(dt_keeper() %>% select(-c(GAperShot, xGperShot, GmxGperShot, extreme, plotnames)),
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = 25,
                           lengthMenu = seq(25, 100, 25)))) %>%
      formatRound(columns = c('Dist'), 
                  digits = 1) %>%
      formatRound(columns = c('xG', 'G-xG'), 
                  digits = 2) %>%
      formatPercentage(columns = c("Header%"), 
                       digits = 1)
  })
  
  output$keepertable_per96 <- DT::renderDataTable({
    
    datatable(dt_keeper_per96() %>% select(-c(GAperShot, xGperShot, GmxGperShot, extreme, plotnames)),
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = 25,
                           lengthMenu = seq(25, 100, 25)))) %>%
      formatRound(columns = c('Dist'), 
                  digits = 1) %>%
      formatRound(columns = c("Shots", "Goals", "Saves", 'xG', 'G-xG'), 
                  digits = 2) %>%
      formatPercentage(columns = c("Header%"), 
                       digits = 1)
  })
  
  # Keeper downloads ####
  output$keeper_download <- downloadHandler(
    filename = paste0("ASAkeepertable_",
                      ifelse(input$keeper_subtab %in% c("tablestotals", "plotstotals"), "totals", "per96"), 
                      ".csv"),
    
    content = function(file){
      if(input$keeper_subtab %in% c("tablestotals", "plotstotals")){
        namesFL <- as.data.frame(do.call("rbind", strsplit(sub(" ", ";", dt_keeper()$Keeper), ";")))
        names(namesFL) <- c("First", "Last")
        write.csv(data.frame(namesFL, dt_keeper() %>% select(-c(extreme, plotnames))), file, row.names = F)
      } else{
        namesFL <- as.data.frame(do.call("rbind", strsplit(sub(" ", ";", dt_keeper_per96()$Keeper), ";")))
        names(namesFL) <- c("First", "Last")
        write.csv(data.frame(namesFL, dt_keeper_per96() %>% select(-c(extreme, plotnames))), file, row.names = F)
      }
    })
  
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
                            season = as.numeric(format(input$team_date1, "%Y")):as.numeric(format(input$team_date2, "%Y")),
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
    
    if(input$team_advanced == "Basic stats"){
      columns.perc1 <- c('SoT%F', 'SoT%A', 'Finish%F', 'Finish%A')
      columns.dec1 <- c()
      columns.dec2 <- c()
    } else{
      columns.perc1 <- c() #c("Solo%F", "Solo%A")
      columns.dec1 <- c("xGF", "xGA", "xGD", "GD-xGD", "PDO")
      columns.dec2 <- c("TSR")
    }
    
    if(!input$team_conferenceview | length(input$team_seasonfilter) > 1){
      DT::datatable(dt,
                    rownames = F,
                    options(list(autoWidth = T,
                                 pageLength = 25))) %>%
        formatPercentage(columns = columns.perc1, digits = 1) %>%
        formatRound(columns = columns.dec1, digits = 1) %>%
        formatRound(columns = columns.dec2, digits = 2)
    } else{
      DT::datatable(dt,
                    rownames = F,
                    options(list(autoWidth = T,
                                 pageLength = 25,
                                 dom = 't'))) %>%
        formatPercentage(columns = columns.perc1, digits = 1) %>%
        formatRound(columns = columns.dec1, digits = 1) %>%
        formatRound(columns = columns.dec2, digits = 2)
      
    }
  })
  
  output$teamtotalxgoalseast <- DT::renderDataTable({
    dt <- dt_team()
    
    if('Conf' %in% names(dt)){
      dt <- dt %>%
        filter(Conf == 'east') %>%
        select(-Conf)
    }
    
    if(input$team_advanced == "Basic stats"){
      columns.perc1 <- c('SoT%F', 'SoT%A', 'Finish%F', 'Finish%A')
      columns.dec1 <- c()
      columns.dec2 <- c()
    } else{
      columns.perc1 <- c() #c("Solo%F", "Solo%A")
      columns.dec1 <- c("xGF", "xGA", "xGD", "GD-xGD", "PDO")
      columns.dec2 <- c("TSR")
    }
    
    DT::datatable(dt,
                  rownames = F,
                  options(list(autoWidth = T,
                               pageLength = 25,
                               dom = 't'))) %>%
      formatPercentage(columns = columns.perc1, digits = 1) %>%
      formatRound(columns = columns.dec1, digits = 1) %>%
      formatRound(columns = columns.dec2, digits = 2)
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
                            season = as.numeric(format(input$team_date1, "%Y")):as.numeric(format(input$team_date2, "%Y")),
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
    
    if(input$team_advanced == "Basic stats"){
      columns.perc1 <- c('SoT%F', 'SoT%A', 'Finish%F', 'Finish%A')
      columns.dec1 <- c("ShtF", "ShtA", "SoTF", "SoTA")
      columns.dec2 <- c("GF", "GA", "GD", "Pts")
    } else{
      columns.perc1 <- c() #c("Solo%F", "Solo%A")
      columns.dec1 <- c("ShtF", "ShtA","PDO")
      columns.dec2 <- c("xGF", "xGA", "xGD", "GF", "GA", "GD", "GD-xGD", "TSR", "Pts")
    }
    
    datatable(dt,
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = 25,
                           dom = 't'))) %>%
      formatPercentage(columns = columns.perc1, digits = 1) %>%
      formatRound(columns = columns.dec1, digits = 1) %>%
      formatRound(columns = columns.dec2, digits = 2)
  })
  
  output$teampergamexgoalseast <- DT::renderDataTable({
    
    if('Conf' %in% names(dt_team_pergame())){
      dt <- dt_team_pergame() %>%
        filter(Conf == 'east') %>%
        select(-Conf)
    } else{
      dt <- dt_team_pergame()
    }
    
    if(input$team_advanced == "Basic stats"){
      columns.perc1 <- c('SoT%F', 'SoT%A', 'Finish%F', 'Finish%A')
      columns.dec1 <- c("ShtF", "ShtA", "SoTF", "SoTA")
      columns.dec2 <- c("GF", "GA", "GD", "Pts")
    } else{
      columns.perc1 <- c() #c("Solo%F", "Solo%A")
      columns.dec1 <- c("ShtF", "ShtA","PDO")
      columns.dec2 <- c("xGF", "xGA", "xGD", "GF", "GA", "GD", "GD-xGD", "TSR", "Pts")
    }
    
    datatable(dt,
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = 25,
                           dom = 't'))) %>%
      formatPercentage(columns = columns.perc1, digits = 1) %>%
      formatRound(columns = columns.dec1, digits = 1) %>%
      formatRound(columns = columns.dec2, digits = 2)
  })
  
  output$team_download_pergame <- downloadHandler(
    filename = 'ASAteamtable_pergame.csv',
    
    content = function(file){
      write.csv(dt_team_pergame(), file, row.names = F)
    }
  )
  
  # Team reactive values #### Unused
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
                                teamplot_xvar = 'xGF',
                                teamplot_yvar = 'xGA')
  
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
  
  # Team passing tables ####
  dt_team_passing <- reactive({
    dt <- teampassing.func(offense = teampassing.offense,
                           defense = teampassing.defense,
                           season = input$teampassing_seasonfilter,
                           byseasons = input$teampassing_byseasons,
                           third.filter = input$teampassing_thirdfilter,
                           games_df = gamesplayed) 
    
    is.num <- sapply(dt, is.numeric)
    dt[is.num] <- lapply(dt[is.num], round, 3)
    
    dt
  })
  
  dt_team_passing_pergame <- reactive({
    dt <- teampassing.func(offense = teampassing.offense,
                           defense = teampassing.defense,
                           season = input$teampassing_seasonfilter,
                           byseasons = input$teampassing_byseasons,
                           third.filter = input$teampassing_thirdfilter,
                           pergame = T,
                           games_df = gamesplayed) 
    
    is.num <- sapply(dt, is.numeric)
    dt[is.num] <- lapply(dt[is.num], round, 3)
    
    dt
  })
  
  output$teampassing_total <- DT::renderDataTable({
    dt <- dt_team_passing()
    
    columns.perc1 <- c("PctF", "xPctF", "PctA", "xPctA")
    columns.dec1 <- c("ScoreF", "ScoreA", "ScoreDiff")
    columns.dec2 <- c("Per100F", "Per100A", "VertF", "VertA", "VertDiff")
    
    DT::datatable(dt,
                  rownames = F,
                  options(list(autoWidth = T,
                               pageLength = 25))) %>%
      formatPercentage(columns = columns.perc1, digits = 1) %>%
      formatRound(columns = columns.dec1, digits = 1) %>%
      formatRound(columns = columns.dec2, digits = 2)
  })
  
  output$teampassing_pergame <- DT::renderDataTable({
    dt <- dt_team_passing_pergame()
    
    columns.perc1 <- c("PctF", "xPctF", "PctA", "xPctA")
    columns.dec1 <- c("PassF/g", "PassA/g")
    columns.dec2 <- c("Per100F", "Per100A", "VertF", "VertA", "VertDiff", "ScoreF/g", "ScoreA/g", "ScoreDiff/g")
    
    DT::datatable(dt,
                  rownames = F,
                  options(list(autoWidth = T,
                               pageLength = 25))) %>%
      formatPercentage(columns = columns.perc1, digits = 1) %>%
      formatRound(columns = columns.dec1, digits = 1) %>%
      formatRound(columns = columns.dec2, digits = 2)
  })
  
  output$teampassing_download <- downloadHandler(
    filename = 'ASAteampassingtable_total.csv',
    
    content = function(file){
      write.csv(dt_team_passing(), file, row.names = F)
    }
  )
  
  output$teampassing_download_pergame <- downloadHandler(
    filename = 'ASAteampassingtable_pergame.csv',
    
    content = function(file){
      write.csv(dt_team_passing_pergame(), file, row.names = F)
    }
  )
  
  ## xGoals by game ####
  dt_bygame <- reactive({
    if(input$teambygame_seasonordate == 'Season'){
      dt <- xgbygame %>%
        filter(Season %in% input$teambygame_seasonfilter) %>%
        arrange(desc(Date)) %>%
        select(-Season)
      
    } else{
      dt <- xgbygame %>%
        filter(Date >= input$teambygame_date1, Date <= input$teambygame_date2) %>%
        arrange(desc(Date)) %>%
        select(-Season)
    }
    
    is.num <- sapply(dt, is.numeric)
    dt[is.num] <- lapply(dt[is.num], round, 2)
    dt
  })
  
  output$teamxgoalsbygame <- DT::renderDataTable({
    
    datatable(dt_bygame(),
              rownames = F,
              options(list(autoWidth = T,
                           pageLength = 25,
                           lengthMenu = seq(25, 100, 25))))
  })
  
  output$teambygame_download <- downloadHandler(
    filename = 'ASAxGoals_gamebygame.csv',
    
    content = function(file){
      write.csv(dt_bygame(), file, row.names = F)
    }
  )
  
  
  # Glossary ####
  output$glossary <- DT::renderDataTable({
    DT::datatable(glossary %>% select(c(1, 2, 3)),
                  rownames = F,
                  options(list(autoWidth = T,
                               pageLength = 20,
                               lengthMenu = c(10, 20, 30, 40, 50))))
  })
  
    })