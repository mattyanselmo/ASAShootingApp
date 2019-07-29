
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output, session) {
  
  # Bookmarks workaround ####
  # observe({
  #   
  #   query <- parseQueryString(session$clientData$url_search)
  #   
  #   for (i in 1:(length(reactiveValuesToList(input)))) {
  #     nameval = names(reactiveValuesToList(input)[i])
  #     valuetoupdate = query[[nameval]]
  #     print(nameval)
  #     print(valuetoupdate)
  #     if (!is.null(query[[nameval]])) {
  #       if (is.na(as.numeric(valuetoupdate))) {
  #         updateTextInput(session, nameval, value = valuetoupdate)
  #       }
  #       else {
  #         updateTextInput(session, nameval, value = as.numeric(valuetoupdate))
  #       }
  #     }
  #     
  #   }
  #   
  # })
  
  
  # Create tab redirect ####
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query[["headnavbar"]])){
      updateNavbarPage(session, "headnavbar", selected = query[["headnavbar"]])
    }
  })
  
  # Shooter reactive values ####
  # Initial values
  shooter_inputs <- reactiveValues(shooting_seasonordate = 'Season',
                                   shooting_date1 = as.Date('2000-01-01'),
                                   shooting_date2 = as.Date('9999-12-31'),
                                   shooting_seasonfilter = max(playerxgoals$Season),
                                   shooting_minfilter = 0,
                                   teamfilter = sort(unique(playerxgoals$team)),
                                   shooting_minshots = 0,
                                   shooting_minkeypasses = 0,
                                   shooting_position = c("G", "D", "B", "M", "A", "F", "S"),
                                   shooting_byteams = F,
                                   shooting_byseasons = T,
                                   pattern = c("Open", "PK", "FK", "Setpiece"),
                                   # shooting_other = T, REPLACED BY SINGLE CHECKBOX GROUP "PATTERN"
                                   # shooting_fk = T,
                                   # shooting_pk = T,
                                   # setpiece = T,
                                   shooterplot_xvar = "xG",
                                   shooterplot_yvar = "G",
                                   shooterplot_per96_xvar = "xG",
                                   shooterplot_per96_yvar = "G")
  
  # Updated values
  observeEvent(input$shooting_action,
               { 
                 shooter_inputs$shooting_seasonordate <- input$shooting_seasonordate
                 shooter_inputs$shooting_date1 <- input$shooting_date1
                 shooter_inputs$shooting_date2 <- input$shooting_date2
                 shooter_inputs$shooting_seasonfilter <- input$shooting_seasonfilter
                 shooter_inputs$shooting_minfilter <- input$shooting_minfilter
                 shooter_inputs$teamfilter <- input$shooting_teamfilter
                 shooter_inputs$shooting_minshots <- input$shooting_minshots
                 shooter_inputs$shooting_minkeypasses <- input$shooting_minkeypasses
                 shooter_inputs$shooting_position <- input$shooting_position
                 shooter_inputs$shooting_byteams <- input$shooting_byteams
                 shooter_inputs$shooting_byseasons <- input$shooting_byseasons
                 shooter_inputs$pattern <- input$shooting_pattern
                 shooter_inputs$shooterplot_xvar <- input$shooterplot_xvar
                 shooter_inputs$shooterplot_yvar <- input$shooterplot_yvar
                 # shooter_inputs$shooterplot_per96_xvar <- input$shooterplot_per96_xvar
                 # shooter_inputs$shooterplot_per96_yvar <- input$shooterplot_per96_yvar
               })

  # Select all checkboxes  
  observeEvent(input$shooting_teamfilter_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "shooting_teamfilter", 
                   choices = sort(unique(playerxgoals$team)),
                   selected = if (input$shooting_teamfilter_selectall) sort(unique(playerxgoals$team))
                 )
               },
               ignoreInit = T)
  
  observeEvent(input$shooting_seasonfilter_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "shooting_seasonfilter", 
                   choices = min(playerxgoals$Season):max(playerxgoals$Season),
                   selected = if (input$shooting_seasonfilter_selectall) min(playerxgoals$Season):max(playerxgoals$Season)
                 )
               },
               ignoreInit = T)
  
  observeEvent(input$shooting_position_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "shooting_position", 
                   choices = c("Keeper (G)" = "G",
                               "Central Def (D)" = "D",
                               "Back (B)" = "B",
                               "Midfielder (M)" = "M",
                               "Attacking Mid (A)" = "A",
                               "Forward (F)" = "F",
                               "Sub (S)" = "S"),
                   selected = if (input$shooting_position_selectall) c("G", "D", "B", "M", "A", "F", "S")
                 )
               },
               ignoreInit = T)
  
  observeEvent(input$shooting_pattern_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "shooting_pattern", 
                   choices = c("Open play" = "Open", "PK", "Direct FK" = "FK", "Set piece" = "Setpiece"),
                   selected = if (input$shooting_pattern_selectall) c("Open play" = "Open", "PK", "Direct FK" = "FK", "Set piece" = "Setpiece")
                 )
               },
               ignoreInit = T)

  # Shooter tables ####
  dt_total <- reactive({
    # if(shooter_inputs$teamfilter == "All"){
    #   teamfilter <- unique(playerxgoals$team)
    # } else{
    #   teamfilter <- shooter_inputs$teamfilter
    # }

    if(shooter_inputs$shooting_seasonordate == 'Season'){
      dt_total <- shooterxgoals.func(playerxgoals,
                                     date1 = as.Date('2000-01-01'),
                                     date2 = as.Date('9999-12-31'),
                                     season = shooter_inputs$shooting_seasonfilter,
                                     minfilter = shooter_inputs$shooting_minfilter,
                                     shotfilter = shooter_inputs$shooting_minshots,
                                     keyfilter = shooter_inputs$shooting_minkeypasses,
                                     teamfilter = shooter_inputs$teamfilter,
                                     byteams = shooter_inputs$shooting_byteams,
                                     byseasons = shooter_inputs$shooting_byseasons,
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
                                     date1 = shooter_inputs$shooting_date1,
                                     date2 = shooter_inputs$shooting_date2,
                                     season = as.numeric(format(shooter_inputs$shooting_date1, "%Y")):as.numeric(format(shooter_inputs$shooting_date2, "%Y")),
                                     minfilter = shooter_inputs$shooting_minfilter,
                                     shotfilter = shooter_inputs$shooting_minshots,
                                     keyfilter = shooter_inputs$shooting_minkeypasses,
                                     teamfilter = shooter_inputs$teamfilter,
                                     byteams = shooter_inputs$shooting_byteams,
                                     byseasons = shooter_inputs$shooting_byseasons,
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
      dt_total %>% filter(Pos %in% shooter_inputs$shooting_position | is.na(Pos))
    } else{
      dt_total
    }
  })
  
  dt_per96 <- reactive({
    # if(shooter_inputs$teamfilter == "All"){
    #   teamfilter <- unique(playerxgoals$team)
    # } else{
    #   teamfilter <- shooter_inputs$teamfilter
    # }
    if(shooter_inputs$shooting_seasonordate == 'Season'){
      dt_per96 <- shooterxgoals_perminute(playerxgoals,
                                          minutes_df = minutesPlayed,
                                          date1 = as.Date('2000-01-01'),
                                          date2 = as.Date('9999-12-31'),
                                          season = shooter_inputs$shooting_seasonfilter[shooter_inputs$shooting_seasonfilter >= 2015],
                                          shotfilter = shooter_inputs$shooting_minshots,
                                          keyfilter = shooter_inputs$shooting_minkeypasses,
                                          minfilter = shooter_inputs$shooting_minfilter,
                                          teamfilter = shooter_inputs$teamfilter,
                                          byseasons = shooter_inputs$shooting_byseasons,
                                          byteams = shooter_inputs$shooting_byteams,
                                          OpenPlay = "Open" %in% shooter_inputs$pattern,
                                          FK = "FK" %in% shooter_inputs$pattern,
                                          PK = "PK" %in% shooter_inputs$pattern,
                                          SetPiece = "Setpiece" %in% shooter_inputs$pattern) %>%
        mutate(`Comp ($K)` = round(Salary/1000, 0)) %>%
        select(-Salary) %>%
        rename(A = Assts,
               G = Goals)
      
    } else{
      dt_per96 <- shooterxgoals_perminute(playerxgoals,
                                          minutes_df = minutesPlayed,
                                          date1 = max(shooter_inputs$shooting_date1, as.Date("2015-01-01")),
                                          date2 = max(shooter_inputs$shooting_date2, as.Date("2015-01-01")),
                                          season = min(playerxgoals$Season):max(playerxgoals$Season),
                                          shotfilter = shooter_inputs$shooting_minshots,
                                          keyfilter = shooter_inputs$shooting_minkeypasses,
                                          minfilter = shooter_inputs$shooting_minfilter,
                                          teamfilter = shooter_inputs$teamfilter,
                                          byseasons = shooter_inputs$shooting_byseasons,
                                          byteams = shooter_inputs$shooting_byteams,
                                          OpenPlay = "Open" %in% shooter_inputs$pattern,
                                          FK = "FK" %in% shooter_inputs$pattern,
                                          PK = "PK" %in% shooter_inputs$pattern,
                                          SetPiece = "Setpiece" %in% shooter_inputs$pattern) %>%
        mutate(`Comp ($K)` = round(Salary/1000, 0)) %>%
        select(-Salary) %>%
        rename(A = Assts,
               G = Goals)
    }
    
    dt_per96 %>% filter(Pos %in% shooter_inputs$shooting_position)
  })
  
  # Player table - totals
  output$shootertable <- DT::renderDataTable({
    
    datatable(dt_total() %>% select(-c(Dist.key, `xG/shot`:`A-xA/pass`)),
              rownames = F,
              options = list(autoWidth = T,
                           pageLength = 25,
                           lengthMenu = seq(25, 100, 25))) %>%
      formatRound(columns = c('Dist', 'xG', 'G-xG', 'xPlace', 'xA', 'A-xA', 'xG+xA'), 
                  digits = 1) %>%
      formatRound(columns = c("PA", "xPA"), 
                  digits = 2) %>%
      formatPercentage(columns = c('Solo'), digits = 1) %>%
      formatCurrency(columns = c("Comp ($K)"),
                     currency = "$",
                     interval = 3,
                     mark = ",",
                     digits = 0)
  })
  
  # Player table - per96
  output$shootertable_per96 <- DT::renderDataTable({
    
    
    datatable(dt_per96() %>% select(-one_of("extreme", "plotnames")),
              rownames = F,
              options = list(autoWidth = T,
                           pageLength = 25,
                           lengthMenu = seq(25, 100, 25))) %>%
      formatRound(columns = c("Shots", "SoT", "G", "xG", "xPlace", "G-xG", 
                              "KeyP", "A", "xA", "A-xA", "xG+xA", "PA", "xPA"), 
                  digits = 2)  %>%
      formatCurrency(columns = c("Comp ($K)"),
                     currency = "$",
                     interval = 3,
                     mark = ",",
                     digits = 0)
  })
  
  # Shooter plots ####
  dt_playershootingplot <- reactive({
    dt <- dt_total() %>%
      left_join(dt_per96(),
                by = c("Player", "Team", "Season")[c(TRUE, shooter_inputs$shooting_byteams, shooter_inputs$shooting_byseasons)],
                suffix = c("", "/96"))
    
    dt[["extreme1"]] <- rank(dt[[shooter_inputs$shooterplot_xvar]], ties.method = "random")
    dt[["extreme2"]] <- rank(dt[[shooter_inputs$shooterplot_yvar]], ties.method = "random")
    
    dt[[shooter_inputs$shooterplot_xvar]] <- round(dt[[shooter_inputs$shooterplot_xvar]], 3)
    dt[[shooter_inputs$shooterplot_yvar]] <- round(dt[[shooter_inputs$shooterplot_yvar]], 3)
    
    if(length(unique(dt$Season)) > 1){
      dt[['plotnames']] <- paste(unlist(lapply(strsplit(dt$Player, " "), function(x) { return(x[length(x)]) })), dt$Season)
      
    }else{
      dt[['plotnames']] <- unlist(lapply(strsplit(dt$Player, " "), function(x) { return(x[length(x)]) }))
    }
    dt
  })
  
  playershooting_plotvalues <- reactiveValues(shooterplot_xvar = "xG", shooterplot_yvar = "Goals")
  
  observeEvent(input$shooting_action, {
    choices.total <- names(dt_total())[!(names(dt_total()) %in% c("Player", "Team", "Season", "Pos"))]
    if(min(input$shooting_seasonfilter) >= 2015){
      choices.96 <- paste0(names(dt_per96())[!(names(dt_per96()) %in% c("Player", "Team", "Season", "Pos", "Min", "Comp ($K)"))], "/96")
    } else{
      choices.96 <- c("")
    }
    updateSelectInput(session,
                      inputId = 'shooterplot_xvar',
                      label = 'X-axis variable',
                      choices = c(choices.total, choices.96),
                      selected = playershooting_plotvalues$shooterplot_xvar)
    
    updateSelectInput(session,
                      inputId = 'shooterplot_yvar',
                      label = 'Y-axis variable',
                      choices = c(choices.total, choices.96),
                      selected = playershooting_plotvalues$shooterplot_yvar)
  })
  
  observeEvent({
    dt_total()
    dt_per96()},
    {
      choices.total <- names(dt_total())[!(names(dt_total()) %in% c("Player", "Team", "Season", "Pos"))]
      if(min(input$shooting_seasonfilter) >= 2015){
        choices.96 <- paste0(names(dt_per96())[!(names(dt_per96()) %in% c("Player", "Team", "Season", "Pos", "Min", "Comp ($K)"))], "/96")
      } else{
        choices.96 <- c("")
      }
      updateSelectInput(session,
                        inputId = 'shooterplot_xvar',
                        label = 'X-axis variable',
                        choices = c(choices.total, choices.96),
                        selected = "xG")
      
      updateSelectInput(session,
                        inputId = 'shooterplot_yvar',
                        label = 'Y-axis variable',
                        choices = c(choices.total, choices.96),
                        selected = "G")             
    },
    once = T)
  
  observeEvent({
    input$shooterplot_xvar 
    input$shooterplot_yvar
  }, 
  {
    playershooting_plotvalues$shooterplot_xvar <- input$shooterplot_xvar
    playershooting_plotvalues$shooterplot_yvar <- input$shooterplot_yvar
  })
  
  output$shooterplot <- renderPlotly({
    xlim <- min(dt_playershootingplot()[[shooter_inputs$shooterplot_xvar]]) - 0*(max(dt_playershootingplot()[[passer_inputs$passerplot_xvar]]) - min(dt_playershootingplot()[[shooter_inputs$shooterplot_xvar]]))
    ylim <- min(dt_playershootingplot()[[passer_inputs$passerplot_yvar]]) - 0*(max(dt_playershootingplot()[[shooter_inputs$shooterplot_yvar]]) - min(dt_playershootingplot()[[shooter_inputs$shooterplot_yvar]]))
    
    p <- dt_playershootingplot() %>%
      ggplot(
        aes_string(x = paste0('`', shooter_inputs$shooterplot_xvar, '`'), 
                   y = paste0('`', shooter_inputs$shooterplot_yvar, '`'))) +
      geom_point(aes(text = paste0(plotnames, "<br>Shots:", Shots)), 
                 color = '#0000cc') +
      expand_limits(x = xlim,
                    y = ylim) +
      geom_smooth(method = 'lm', se = F, color = "black") +
      ggtheme
    
    m <- dt_playershootingplot() %>% 
      filter(extreme1 >= sort(extreme1, decreasing = T)[1] |
               extreme1 <= sort(extreme1, decreasing = F)[1] |
               extreme2 >= sort(extreme2, decreasing = T)[1] |
               extreme2 <= sort(extreme2, decreasing = F)[1])
    
    a <- list(
      x = m[[shooter_inputs$shooterplot_xvar]],
      y = m[[shooter_inputs$shooterplot_yvar]],
      text = m$plotnames,
      xref = "x",
      yref = "y",
      showarrow = F,
      xanchor = "center",
      yanchor = "top",
      font = list(color = "#ff3300",
                  size = 10)
    )
    
    ggplotly(p,
             tooltip = c("x", "y", "text"),
             width = 700,
             height = 500) %>%
      add_markers() %>%
      layout(annotations = a,
             showarrow = F)
    
  })
  
  output$shooterplot_text <- renderText({
    paste0('<font size = "4">', 
           lm_eqn2(dt_playershootingplot(), 
                   paste0('`', shooter_inputs$shooterplot_xvar, '`'),
                   paste0('`', shooter_inputs$shooterplot_yvar, '`')),
           "</font>")
  })
  
  # Shooter downloads ####
  output$player_download <- downloadHandler(
    filename = paste0("ASAshootertable.csv"),
    
    content = function(file){
      namesFL <- as.data.frame(do.call("rbind", strsplit(sub(" ", ";", dt_playershootingplot()$Player), ";")))
      names(namesFL) <- c("First", "Last")
      write.csv(data.frame(namesFL, dt_playershootingplot(), check.names = FALSE), file, row.names = FALSE)
    }
  )
  
  # Passer reactive values ####
  
  # Initial values
  passer_inputs <- reactiveValues(passing_position = c("G", "D", "B", "M", "A", "F", "S", "Heaven"),
                                  passing_third = "All",
                                  passing_seasonordate = "Season",
                                  passing_date1 = as.Date("2000-01-01"),
                                  passing_date2 = as.Date("9999-12-31"),
                                  passing_seasonfilter = max(playerpassing$Season),
                                  teamfilter = sort(unique(playerpassing$team)),
                                  passing_minpasses = 0,
                                  passing_minfilter = 0,
                                  passing_byteams = F,
                                  passing_byseasons = T,
                                  passerplot_xvar = 'xPassPct',
                                  passerplot_yvar = 'PassPct')
  
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
                 passer_inputs$passing_seasonordate <- input$passing_seasonordate
                 passer_inputs$passing_seasonfilter <- input$passing_seasonfilter
                 passer_inputs$teamfilter <- input$passing_teamfilter
                 passer_inputs$passing_date1 <- input$passing_date1
                 passer_inputs$passing_date2 <- input$passing_date2
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
  
  # Select all checkboxes  
  observeEvent(input$passing_position_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "passing_position", 
                   choices = c("Keeper (G)" = "G",
                               "Central Def (D)" = "D",
                               "Back (B)" = "B",
                               "Midfielder (M)" = "M",
                               "Attacking Mid (A)" = "A",
                               "Forward (F)" = "F",
                               "Sub (S)" = "S"),
                   selected = if (input$passing_position_selectall) c("G", "D", "B", "M", "A", "F", "S")
                 )
               },
               ignoreInit = T)
  
  observeEvent(input$passing_seasonfilter_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "passing_seasonfilter", 
                   choices = min(playerpassing$Season):max(playerpassing$Season),
                   selected = if (input$passing_seasonfilter_selectall) min(playerpassing$Season):max(playerpassing$Season)
                 )
               },
               ignoreInit = T)
  
  observeEvent(input$passing_teamfilter_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "passing_teamfilter", 
                   choices = sort(unique(playerpassing$team)),
                   selected = if (input$passing_teamfilter_selectall) sort(unique(playerpassing$team))
                 )
               },
               ignoreInit = T)
 
  # Passer tables ####
  dt_passer <- reactive({
    if(passer_inputs$passing_seasonordate == "Season"){
      dt <- passer.xpasses(playerpassing,
                           minpasses = passer_inputs$passing_minpasses,
                           minfilter = passer_inputs$passing_minfilter,
                           seasonfilter = passer_inputs$passing_seasonfilter,
                           teamfilter = passer_inputs$teamfilter,
                           date1 = as.Date('2000-01-01'),
                           date2 = as.Date('9999-12-31'), 
                           byteams = passer_inputs$passing_byteams,
                           byseasons = passer_inputs$passing_byseasons,
                           third.filter = passer_inputs$passing_third,
                           pos.filter = passer_inputs$passing_position)
    } else{
      dt <- passer.xpasses(playerpassing,
                           minpasses = passer_inputs$passing_minpasses,
                           minfilter = passer_inputs$passing_minfilter,
                           seasonfilter = min(playerpassing$Season):max(playerpassing$Season),
                           teamfilter = passer_inputs$teamfilter,
                           date1 = passer_inputs$passing_date1,
                           date2 = passer_inputs$passing_date2, 
                           byteams = passer_inputs$passing_byteams,
                           byseasons = passer_inputs$passing_byseasons,
                           third.filter = passer_inputs$passing_third,
                           pos.filter = passer_inputs$passing_position)
    }
    
    dt %>%
      mutate(`Comp ($K)` = Comp / 1000) %>%
      select(-Comp)
    # Append passer names and extreme obs for plotting?
  })
  
  dt_passer_per96 <- reactive({
    if(passer_inputs$passing_seasonordate == "Season"){
      dt <- passer.xpasses.p96(playerpassing,
                               minpasses = passer_inputs$passing_minpasses,
                               minfilter = passer_inputs$passing_minfilter,
                               seasonfilter = passer_inputs$passing_seasonfilter,
                               teamfilter = passer_inputs$teamfilter,
                               date1 = as.Date('2000-01-01'),
                               date2 = as.Date('9999-12-31'), 
                               byteams = passer_inputs$passing_byteams,
                               byseasons = passer_inputs$passing_byseasons,
                               third.filter = passer_inputs$passing_third,
                               pos.filter = passer_inputs$passing_position)
      
    } else{
      dt <- passer.xpasses.p96(playerpassing,
                               minpasses = passer_inputs$passing_minpasses,
                               minfilter = passer_inputs$passing_minfilter,
                               seasonfilter = min(playerpassing$Season):max(playerpassing$Season),
                               teamfilter = passer_inputs$teamfilter,
                               date1 = passer_inputs$passing_date1,
                               date2 = passer_inputs$passing_date2,                               
                               byteams = passer_inputs$passing_byteams,
                               byseasons = passer_inputs$passing_byseasons,
                               third.filter = passer_inputs$passing_third,
                               pos.filter = passer_inputs$passing_position)
    }
    dt %>%
      mutate(`Comp ($K)` = Comp / 1000) %>%
      select(-Comp)
  })
  
  output$passingtable_player <- DT::renderDataTable({
    DT::datatable(dt_passer(),
                  rownames = F,
                  options = list(autoWidth = T,
                               pageLength = 25,
                               lengthMenu = seq(25, 100, 25))) %>%
      formatRound(columns = c("Score", "Per100", "Distance", "Vertical"), 
                  digits = 1) %>%
      formatPercentage(columns = c("PassPct", "xPassPct", "Touch%")[c(T, T, passer_inputs$passing_third == "All")], 
                       digits = 1) %>%
      formatCurrency(columns = c("Comp ($K)"),
                     currency = "$",
                     interval = 3,
                     mark = ",",
                     digits = 0) 
  })
  
  output$passingtable_player_per96 <- DT::renderDataTable({
    DT::datatable(dt_passer_per96(),
                  rownames = F,
                  options = list(autoWidth = T,
                               pageLength = 25,
                               lengthMenu = seq(25, 100, 25))) %>%
      formatRound(columns = c("Per100", "Distance", "Vertical", "Passes"), 
                  digits = 1) %>%
      formatRound(columns = c("Score"),
                  digits = 2) %>%
      formatPercentage(columns = c("PassPct", "xPassPct", "Touch%")[c(T, T, passer_inputs$passing_third == "All")], 
                       digits = 1) %>%
      formatCurrency(columns = c("Comp ($K)"),
                     currency = "$",
                     interval = 3,
                     mark = ",",
                     digits = 0) 
  })
  
  # Passer plots ####
  dt_passer_plot <- reactive({
    dt <- dt_passer() %>%
      left_join(dt_passer_per96() %>% select(-one_of(c("Pos", "Min", "Team", "Comp ($K)"))),
                by = c("Player", "Season")[c(T, passer_inputs$passing_byseasons)],
                suffix = c("", "/96"))
    
    dt[["extreme1"]] <- rank(dt[[passer_inputs$passerplot_xvar]], ties.method = "random")
    dt[["extreme2"]] <- rank(dt[[passer_inputs$passerplot_yvar]], ties.method = "random")
    
    dt[[passer_inputs$passerplot_xvar]] <- round(dt[[passer_inputs$passerplot_xvar]], 3)
    dt[[passer_inputs$passerplot_yvar]] <- round(dt[[passer_inputs$passerplot_yvar]], 3)
    
    if(length(unique(dt$Season)) > 1){
      dt[['plotnames']] <- paste(unlist(lapply(strsplit(dt$Player, " "), function(x) { return(x[length(x)]) })), dt$Season)
      
    }else{
      dt[['plotnames']] <- unlist(lapply(strsplit(dt$Player, " "), function(x) { return(x[length(x)]) }))
    }
    dt
  })
  
  playerpassing_plotvalues <- reactiveValues(passerplot_xvar = "xPassPct", passerplot_yvar = "PassPct")
  
  observeEvent(input$passing_action, {
    choices.total <- setdiff(names(dt_passer()), c("Player", "Team", "Season", "Pos", "Comp ($K)"))
    choices.96 <- c("Passes/96", "Score/96")

    updateSelectInput(session,
                      inputId = 'passerplot_xvar',
                      label = 'X-axis variable',
                      choices = c(choices.total, choices.96),
                      selected = playerpassing_plotvalues$passerplot_xvar)
    
    updateSelectInput(session,
                      inputId = 'passerplot_yvar',
                      label = 'Y-axis variable',
                      choices = c(choices.total, choices.96),
                      selected = playerpassing_plotvalues$passerplot_yvar)
  })
  
  observeEvent({
    dt_passer()
    dt_passer_per96()
  },
  {
    choices.total <- setdiff(names(dt_passer()), c("Player", "Team", "Season", "Pos", "Comp ($K)"))
    choices.96 <- c("Passes/96", "Score/96")
    
    updateSelectInput(session,
                      inputId = 'passerplot_xvar',
                      label = 'X-axis variable',
                      choices = c(choices.total, choices.96),
                      selected = "xPassPct")
    
    updateSelectInput(session,
                      inputId = 'passerplot_yvar',
                      label = 'Y-axis variable',
                      choices = c(choices.total, choices.96),
                      selected = "PassPct")          
  },
  once = T)
  
  observeEvent({
    input$passerplot_xvar 
    input$passerplot_yvar
  }, 
  {
    playerpassing_plotvalues$passerplot_xvar <- input$passerplot_xvar
    playerpassing_plotvalues$passerplot_yvar <- input$passerplot_yvar
  })
  
  output$passerplot <- renderPlotly({
    xlim <- min(dt_passer_plot()[[passer_inputs$passerplot_xvar]]) - 0*(max(dt_passer_plot()[[passer_inputs$passerplot_xvar]]) - min(dt_passer_plot()[[passer_inputs$passerplot_xvar]]))
    ylim <- min(dt_passer_plot()[[passer_inputs$passerplot_yvar]]) - 0*(max(dt_passer_plot()[[passer_inputs$passerplot_yvar]]) - min(dt_passer_plot()[[passer_inputs$passerplot_yvar]]))
    
    p <- dt_passer_plot() %>%
      ggplot(
        aes_string(x = paste0('`', passer_inputs$passerplot_xvar, '`'), 
                   y = paste0('`', passer_inputs$passerplot_yvar, '`'))) +
      geom_point(aes(text = paste0(plotnames, "<br>Passes:", Passes)), color = '#0000cc') +
      expand_limits(x = xlim,
                    y = ylim) +
      geom_smooth(method = 'lm', se = F, color = "black") +
      ggtheme
    
    m <- dt_passer_plot() %>% 
      filter(extreme1 >= sort(extreme1, decreasing = T)[1] |
               extreme1 <= sort(extreme1, decreasing = F)[1] |
               extreme2 >= sort(extreme2, decreasing = T)[1] |
               extreme2 <= sort(extreme2, decreasing = F)[1])
    
    a <- list(
      x = m[[passer_inputs$passerplot_xvar]],
      y = m[[passer_inputs$passerplot_yvar]],
      text = m$plotnames,
      xref = "x",
      yref = "y",
      showarrow = F,
      xanchor = "center",
      yanchor = "top",
      font = list(color = '#ff3300',
                  size = 10)
    )
    
    ggplotly(p,
             tooltip = c("x", "y", "text"),
             width = 700,
             height = 500) %>%
      add_markers() %>%
      layout(annotations = a)
    
  })
  
  output$passerplot_text <- renderText({
    paste0('<font size = "4">', 
           lm_eqn2(dt_passer_plot(), 
                   paste0('`', passer_inputs$passerplot_xvar, '`'),
                   paste0('`', passer_inputs$passerplot_yvar, '`')),
           "</font>")
  })
  
  # Passer downloads ####
  output$passing_download <- downloadHandler(
    filename = paste0("ASApassingtable.csv"),
    content = function(file){
      namesFL <- as.data.frame(do.call("rbind", strsplit(sub(" ", ";", dt_passer_plot()$Player), ";")))
      names(namesFL) <- c("First", "Last")
      write.csv(data.frame(namesFL, dt_passer_plot(), check.names = FALSE), file, row.names = FALSE)
    }
  )
  
  # Player xGChain reactive values ####
  # Initial values
  playerxgchain_inputs <- reactiveValues(playerxgchain_position = c("G", "D", "B", "M", "A", "F", "S"),
                                         playerxgchain_seasonordate = "Season",
                                         playerxgchain_date1 = as.Date("2000-01-01"),
                                         playerxgchain_date2 = as.Date("9999-12-31"),
                                         playerxgchain_seasonfilter = max(playerpassing$Season),
                                         playerxgchain_minfilter = 0,
                                         playerxgchain_byteams = F,
                                         playerxgchain_byseasons = T,
                                         #playerxgchain_gamestate0ind = c(0, 1),
                                         playerxgchain_plot_xvar = 'SOMETHINGNEW',
                                         playerxgchain_plot_yvar = 'SOMETHINGNEW')
  
  # Updated values
  observeEvent(input$playerxgchain_action,
               {
                 playerxgchain_inputs$playerxgchain_position <- input$playerxgchain_position
                 playerxgchain_inputs$playerxgchain_seasonordate <- input$playerxgchain_seasonordate
                 playerxgchain_inputs$playerxgchain_seasonfilter <- input$playerxgchain_seasonfilter
                 playerxgchain_inputs$playerxgchain_date1 <- input$playerxgchain_date1
                 playerxgchain_inputs$playerxgchain_date2 <- input$playerxgchain_date2
                 playerxgchain_inputs$playerxgchain_minfilter <- input$playerxgchain_minfilter
                 playerxgchain_inputs$playerxgchain_byteams <- input$playerxgchain_byteams
                 playerxgchain_inputs$playerxgchain_byseasons <- input$playerxgchain_byseasons
                 #playerxgchain_inputs$playerxgchain_gamestate0ind <- ifelse(rep(input$playerxgchain_gamestate0ind, 2), c(1, 1), c(0, 1))
                 #playerxgchain_inputs$playerxgchain_inputs$playerxgchain_xvar <- input$playerxgchain_plot_xvar
                 #playerxgchain_inputs$playerxgchain_inputs$playerxgchain_yvar <- input$playerxgchain_plot_yvar
               })
  
  # Player xGChain tables ####
  dt_playerxgchain_totals <- reactive({
    if(playerxgchain_inputs$playerxgchain_seasonordate == "Season"){
      dt <- xgchain.function(playerchaindata,
                             min.filter = playerxgchain_inputs$playerxgchain_minfilter,
                             season.filter = playerxgchain_inputs$playerxgchain_seasonfilter,
                             date1 = as.Date('2000-01-01'),
                             date2 = as.Date('9999-12-31'), 
                             byteams = playerxgchain_inputs$playerxgchain_byteams,
                             byseasons = playerxgchain_inputs$playerxgchain_byseasons,
                             #gamestateind = playerxgchain_inputs$playerxgchain_gamestate0ind,
                             perminute = F) %>%
        mutate(`Comp ($K)`= Comp / 1000) %>%
        select(-Comp)
    } else{
      dt <- xgchain.function(playerchaindata,
                             min.filter = playerxgchain_inputs$playerxgchain_minfilter,
                             season.filter = min(playerchaindata$Season):max(playerchaindata$Season),
                             date1 = playerxgchain_inputs$playerxgchain_date1,
                             date2 = playerxgchain_inputs$playerxgchain_date2, 
                             byteams = playerxgchain_inputs$playerxgchain_byteams,
                             byseasons = playerxgchain_inputs$playerxgchain_byseasons,
                             #gamestateind = playerxgchain_inputs$playerxgchain_gamestate0ind,
                             perminute = F) %>%
        mutate(`Comp ($K)`= Comp / 1000) %>%
        select(-Comp)
    }
    
    dt %>%
      filter(Pos %in% playerxgchain_inputs$playerxgchain_position)
  })
  
  dt_playerxgchain_per96 <- reactive({
    if(playerxgchain_inputs$playerxgchain_seasonordate == "Season"){
      dt <- xgchain.function(playerchaindata,
                             min.filter = playerxgchain_inputs$playerxgchain_minfilter,
                             season.filter = playerxgchain_inputs$playerxgchain_seasonfilter,
                             date1 = as.Date('2000-01-01'),
                             date2 = as.Date('9999-12-31'), 
                             byteams = playerxgchain_inputs$playerxgchain_byteams,
                             byseasons = playerxgchain_inputs$playerxgchain_byseasons,
                             #gamestateind = playerxgchain_inputs$playerxgchain_gamestate0ind,
                             perminute = T) %>%
        mutate(`Comp ($K)`= Comp / 1000) %>%
        select(-Comp)
    } else{
      dt <- xgchain.function(playerchaindata,
                             min.filter = playerxgchain_inputs$playerxgchain_minfilter,
                             season.filter = min(playerchaindata$Season):max(playerchaindata$Season),
                             date1 = playerxgchain_inputs$playerxgchain_date1,
                             date2 = playerxgchain_inputs$playerxgchain_date2, 
                             byteams = playerxgchain_inputs$playerxgchain_byteams,
                             byseasons = playerxgchain_inputs$playerxgchain_byseasons,
                             #gamestateind = playerxgchain_inputs$playerxgchain_gamestate0ind,
                             perminute = T) %>%
        mutate(`Comp ($K)`= Comp / 1000) %>%
        select(-Comp)
    }
    
    dt %>%
      filter(Pos %in% playerxgchain_inputs$playerxgchain_position)
  })
  
  # Select all checkboxes  
  observeEvent(input$playerxgchain_position_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "playerxgchain_position", 
                   choices = c("Keeper (G)" = "G",
                               "Central Def (D)" = "D",
                               "Back (B)" = "B",
                               "Midfielder (M)" = "M",
                               "Attacking Mid (A)" = "A",
                               "Forward (F)" = "F",
                               "Sub (S)" = "S"),
                   selected = if (input$playerxgchain_position_selectall) c("Keeper (G)" = "G",
                                                                              "Central Def (D)" = "D",
                                                                              "Back (B)" = "B",
                                                                              "Midfielder (M)" = "M",
                                                                              "Attacking Mid (A)" = "A",
                                                                              "Forward (F)" = "F",
                                                                              "Sub (S)" = "S")
                 )
               },
               ignoreInit = T)
  
  observeEvent(input$playerxgchain_seasonfilter_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "playerxgchain_seasonfilter", 
                   choices = min(playerchaindata$Season):max(playerchaindata$Season),
                   selected = if (input$playerxgchain_seasonfilter_selectall) min(playerchaindata$Season):max(playerchaindata$Season)
                 )
               },
               ignoreInit = T)
  
  output$playerxgchain_totals <- DT::renderDataTable({
    DT::datatable(dt_playerxgchain_totals(),
                  rownames = F,
                  options = list(autoWidth = T,
                               pageLength = 25,
                               lengthMenu = seq(25, 100, 25))) %>%
      # formatRound(columns = c("NumChains"), 
      #             digits = 1) %>%
      formatRound(columns = c("xB", "xGChain"), 
                  digits = 2) %>%
      formatPercentage(columns = c("TeamChain%", "ChainShot%", "PlayerShot%", "PlayerKP%", "xB%", "xB% (0)"), 
                       digits = 1) %>%
      formatCurrency(columns = c("Comp ($K)"),
                     currency = "$",
                     interval = 3,
                     mark = ",",
                     digits = 0) 
  })
  
  output$playerxgchain_per96 <- DT::renderDataTable({
    DT::datatable(dt_playerxgchain_per96(),
                  rownames = F,
                  options = list(autoWidth = T,
                               pageLength = 25,
                               lengthMenu = seq(25, 100, 25))) %>%
      formatRound(columns = c("NumChains/96"), 
                  digits = 1) %>%
      formatRound(columns = c("xB/96", "xGChain/96"), 
                  digits = 2) %>%
      formatPercentage(columns = c("TeamChain%", "ChainShot%", "PlayerShot%", "PlayerKP%", "xB%", "xB% (0)"), 
                       digits = 1) %>%
      formatCurrency(columns = c("Comp ($K)"),
                     currency = "$",
                     interval = 3,
                     mark = ",",
                     digits = 0)
  })
  
  # Player xGChain downloads ####
  output$playerxgchain_totals_download <- downloadHandler(
    filename = "ASA_PlayerxGChain_totalstable.csv",
    
    content = function(file){
      namesFL <- as.data.frame(do.call("rbind", strsplit(sub(" ", ";", dt_playerxgchain_totals()$Player), ";")))
      names(namesFL) <- c("First", "Last")
      write.csv(data.frame(namesFL, dt_playerxgchain_totals(), check.names = FALSE), file, row.names = FALSE)
    })
  
  output$playerxgchain_per96_download <- downloadHandler(
    filename = "ASA_PlayerxGChain_per96table.csv",
    
    content = function(file){
      namesFL <- as.data.frame(do.call("rbind", strsplit(sub(" ", ";", dt_playerxgchain_per96()$Player), ";")))
      names(namesFL) <- c("First", "Last")
      write.csv(data.frame(namesFL, dt_playerxgchain_per96(), check.names = FALSE), file, row.names = FALSE)
    })
  
  # Keeper reactive values ####
  # Initial values
  keeper_inputs <- reactiveValues(keeper_minshots = 0,
                                  keeper_minfilter = 0,
                                  teamfilter = unique(keeperxgoals$team.1),
                                  keeper_date1 = as.Date('2000-01-01'),
                                  keeper_date2 = as.Date('9999-12-31'),
                                  keeper_seasonfilter = max(keeperxgoals$Season),
                                  keeper_seasonordate = 'Season',
                                  keeper_byseasons = T,
                                  keeper_byteams = F,
                                  pattern = c("Open", "FK", "PK", "Setpiece"),
                                  keeperplot_xvar = 'xG',
                                  keeperplot_yvar = 'G-xG')
  
  # Updated values
  observeEvent(input$keeper_action,
               {
                 keeper_inputs$keeper_minshots <- input$keeper_minshots
                 keeper_inputs$keeper_minfilter <- input$keeper_minfilter
                 keeper_inputs$teamfilter <- input$keeper_teamfilter
                 keeper_inputs$keeper_date1 <- input$keeper_date1
                 keeper_inputs$keeper_date2 <- input$keeper_date2
                 keeper_inputs$keeper_seasonfilter <- input$keeper_seasonfilter
                 keeper_inputs$keeper_seasonordate <- input$keeper_seasonordate
                 keeper_inputs$keeper_byseasons <- input$keeper_byseasons
                 keeper_inputs$keeper_byteams <- input$keeper_byteams
                 keeper_inputs$pattern <- input$keeper_pattern
                 keeper_inputs$keeperplot_xvar <- input$keeperplot_xvar
                 keeper_inputs$keeperplot_yvar <- input$keeperplot_yvar
               })
  
  # Select all checkboxes  
  observeEvent(input$keeper_teamfilter_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "keeper_teamfilter", 
                   choices = sort(unique(keeperxgoals$team.1)),
                   selected = if (input$keeper_teamfilter_selectall) sort(unique(keeperxgoals$team.1))
                 )
               },
               ignoreInit = T)
  
  observeEvent(input$keeper_seasonfilter_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "keeper_seasonfilter", 
                   choices = min(keeperxgoals$Season):max(keeperxgoals$Season),
                   selected = if (input$keeper_seasonfilter_selectall) min(keeperxgoals$Season):max(keeperxgoals$Season)
                 )
               },
               ignoreInit = T)
  
  observeEvent(input$keeper_pattern_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "keeper_pattern", 
                   choices = c("Open play" = "Open", "PK", "Direct FK" = "FK", "Set piece" = "Setpiece"),
                   selected = if (input$keeper_pattern_selectall) c("Open play" = "Open", "PK", "Direct FK" = "FK", "Set piece" = "Setpiece")
                 )
               },
               ignoreInit = T)
  
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
                                     teamfilter = keeper_inputs$teamfilter,
                                     byteams = keeper_inputs$keeper_byteams,
                                     byseasons = keeper_inputs$keeper_byseasons,
                                     OpenPlay = "Open" %in% keeper_inputs$pattern,
                                     FK = "FK" %in% keeper_inputs$pattern,
                                     PK = "PK" %in% keeper_inputs$pattern,
                                     SetPiece = "Setpiece" %in% keeper_inputs$pattern) %>%
        mutate(`Goals/Shot` = Goals/Shots, 
               `xG/Shot` = xG/Shots,
               `G-xG/Shot` = `G-xG`/Shots)
    } else{
      dt_keeper <- keeperxgoals.func(keeperxgoals,
                                     minutes_df = minutesPlayed,
                                     date1 = keeper_inputs$keeper_date1,
                                     date2 = keeper_inputs$keeper_date2,
                                     season = min(playerxgoals$Season):max(playerxgoals$Season),
                                     shotfilter = keeper_inputs$keeper_minshots,
                                     minfilter = keeper_inputs$keeper_minfilter,
                                     teamfilter = keeper_inputs$teamfilter,
                                     byteams = keeper_inputs$keeper_byteams,
                                     byseasons = keeper_inputs$keeper_byseasons,
                                     OpenPlay = "Open" %in% keeper_inputs$pattern,
                                     FK = "FK" %in% keeper_inputs$pattern,
                                     PK = "PK" %in% keeper_inputs$pattern,
                                     SetPiece = "Setpiece" %in% keeper_inputs$pattern) %>%
        mutate(`Goals/Shot` = Goals/Shots, 
               `xG/Shot` = xG/Shots,
               `G-xG/Shot` = `G-xG`/Shots)
    }
    
    dt_keeper %>%
      mutate(`Comp ($K)` = Comp / 1000) %>%
      select(-Comp)
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
                                                 teamfilter = keeper_inputs$teamfilter,
                                                 byteams = keeper_inputs$keeper_byteams,
                                                 byseasons = keeper_inputs$keeper_byseasons,
                                                 OpenPlay = "Open" %in% keeper_inputs$pattern,
                                                 FK = "FK" %in% keeper_inputs$pattern,
                                                 PK = "PK" %in% keeper_inputs$pattern,
                                                 SetPiece = "Setpiece" %in% keeper_inputs$pattern)
    } else{
      dt_keeper_per96 <- keeperxgoals_per96.func(keeperxgoals,
                                                 minutes_df = minutesPlayed,
                                                 date1 = max(keeper_inputs$keeper_date1, as.Date("2015-01-01")),
                                                 date2 = max(keeper_inputs$keeper_date2, as.Date("2015-01-01")),
                                                 season = min(playerxgoals$Season):max(playerxgoals$Season),
                                                 shotfilter = keeper_inputs$keeper_minshots,
                                                 minfilter = keeper_inputs$keeper_minfilter,
                                                 teamfilter = keeper_inputs$teamfilter,
                                                 byteams = keeper_inputs$keeper_byteams,
                                                 byseasons = keeper_inputs$keeper_byseasons,
                                                 OpenPlay = "Open" %in% keeper_inputs$pattern,
                                                 FK = "FK" %in% keeper_inputs$pattern,
                                                 PK = "PK" %in% keeper_inputs$pattern,
                                                 SetPiece = "Setpiece" %in% keeper_inputs$pattern)
    }
    
    dt_keeper_per96 %>%
      mutate(`Comp ($K)` = Comp / 1000) %>%
      select(-Comp)
  })
  
  output$keepertable <- DT::renderDataTable({
    
    DT::datatable(dt_keeper() %>% select(-c(`Goals/Shot`:`G-xG/Shot`)),
              rownames = F,
              options = list(autoWidth = T,
                           pageLength = 25,
                           lengthMenu = seq(25, 100, 25))) %>%
      formatRound(columns = c('Dist'), 
                  digits = 1) %>%
      formatRound(columns = c('xG', 'G-xG'), 
                  digits = 2) %>%
      formatPercentage(columns = c("Header%"), 
                       digits = 1) %>%
      formatCurrency(columns = c("Comp ($K)"),
                     currency = "$",
                     interval = 3,
                     mark = ",",
                     digits = 1)
  })
  
  output$keepertable_per96 <- DT::renderDataTable({
    
    datatable(dt_keeper_per96(),
              rownames = F,
              options = list(autoWidth = T,
                           pageLength = 25,
                           lengthMenu = seq(25, 100, 25))) %>%
      formatRound(columns = c('Dist'), 
                  digits = 1) %>%
      formatRound(columns = c("Shots", "Goals", "Saves", 'xG', 'G-xG'), 
                  digits = 2) %>%
      formatPercentage(columns = c("Header%"), 
                       digits = 1) %>%
      formatCurrency(columns = c("Comp ($K)"),
                     currency = "$",
                     interval = 3,
                     mark = ",",
                     digits = 1)
  })
  
  # Keeper plots ####
  dt_keeperplot <- reactive({
    dt <- dt_keeper() %>%
      left_join(dt_keeper_per96() %>% select(-one_of(c("Team", "Season")[!c(keeper_inputs$keeper_byteams, keeper_inputs$keeper_byseasons)])),
                by = c("Keeper", "Team", "Season")[c(TRUE, keeper_inputs$keeper_byteams, keeper_inputs$keeper_byseasons)],
                suffix = c("", "/96"))
    
    dt[["extreme1"]] <- rank(dt[[keeper_inputs$keeperplot_xvar]], ties.method = "random")
    dt[["extreme2"]] <- rank(dt[[keeper_inputs$keeperplot_yvar]], ties.method = "random") 
    
    dt[[keeper_inputs$keeperplot_xvar]] <- round(dt[[keeper_inputs$keeperplot_xvar]], 3)
    dt[[keeper_inputs$keeperplot_yvar]] <- round(dt[[keeper_inputs$keeperplot_yvar]], 3)
    
    if(length(unique(dt$Season)) > 1){
      dt[['plotnames']] <- paste(unlist(lapply(strsplit(dt$Keeper, " "), function(x) { return(x[length(x)]) })), dt$Season)
      
    }else{
      dt[['plotnames']] <- unlist(lapply(strsplit(dt$Keeper, " "), function(x) { return(x[length(x)]) }))
    }
    dt
  })
  
  keeper_plotvalues <- reactiveValues(keeperplot_xvar = "xG", keeperplot_yvar = "Goals")
  
  observeEvent(input$keeper_action, {
    choices.total <- names(dt_keeper())[!(names(dt_keeper()) %in% c("Keeper", "Team", "Season"))]
    if(min(input$keeper_seasonfilter) >= 2015){
      choices.96 <- paste0(names(dt_keeper_per96())[!(names(dt_keeper_per96()) %in% c("Keeper", "Team", "Season", "Comp ($K)"))], "/96")
    } else{
      choices.96 <- c("")
    }
    updateSelectInput(session,
                      inputId = 'keeperplot_xvar',
                      label = 'X-axis variable',
                      choices = c(choices.total, choices.96),
                      selected = keeper_plotvalues$keeperplot_xvar)
    
    updateSelectInput(session,
                      inputId = 'keeperplot_yvar',
                      label = 'Y-axis variable',
                      choices = c(choices.total, choices.96),
                      selected = keeper_plotvalues$keeperplot_yvar)
  })
  
  observeEvent({
    dt_keeper()
    dt_keeper_per96()},
    {
      choices.total <- names(dt_keeper())[!(names(dt_keeper()) %in% c("Keeper", "Team", "Season"))]
      if(min(input$keeper_seasonfilter) >= 2015){
        choices.96 <- paste0(names(dt_keeper_per96())[!(names(dt_keeper_per96()) %in% c("Keeper", "Team", "Season", "Comp ($K)"))], "/96")
      } else{
        choices.96 <- c("")
      }
      updateSelectInput(session,
                        inputId = 'keeperplot_xvar',
                        label = 'X-axis variable',
                        choices = c(choices.total, choices.96),
                        selected = keeper_plotvalues$keeperplot_xvar)
      
      updateSelectInput(session,
                        inputId = 'keeperplot_yvar',
                        label = 'Y-axis variable',
                        choices = c(choices.total, choices.96),
                        selected = keeper_plotvalues$keeperplot_yvar)          
    },
    once = T)
  
  observeEvent({
    input$keeperplot_xvar 
    input$keeperplot_yvar
  }, 
  {
    keeper_plotvalues$keeperplot_xvar <- input$keeperplot_xvar
    keeper_plotvalues$keeperplot_yvar <- input$keeperplot_yvar
  })
  
  output$keeperplot <- renderPlotly({
    
    xlim <- min(dt_keeperplot()[[keeper_inputs$keeperplot_xvar]]) - 0.05*(max(dt_keeperplot()[[keeper_inputs$keeperplot_xvar]]) - min(dt_keeperplot()[[keeper_inputs$keeperplot_xvar]]))
    ylim <- min(dt_keeperplot()[[keeper_inputs$keeperplot_yvar]]) - 0.05*(max(dt_keeperplot()[[keeper_inputs$keeperplot_yvar]]) - min(dt_keeperplot()[[keeper_inputs$keeperplot_yvar]]))
    
    p <- dt_keeperplot()  %>%
      ggplot(aes_string(x = paste0('`', keeper_inputs$keeperplot_xvar, '`'), 
                        y = paste0('`', keeper_inputs$keeperplot_yvar, '`'))) +
      geom_point(aes(text = paste0(plotnames, "<br>Shots faced:", Shots)), 
                 color = '#0000cc') +
      # geom_text(aes(label = ifelse(dt_keeperplot()$extreme >= sort(dt_keeperplot()$extreme, decreasing = T)[min(3, nrow(dt_keeperplot()))] |
      #                                dt_keeperplot()$extreme <= sort(dt_keeperplot()$extreme)[min(3, nrow(dt_keeperplot()))] |
      #                                dt_keeperplot()[[keeper_inputs$keeperplot_xvar]] == max(dt_keeperplot()[[keeper_inputs$keeperplot_xvar]]) |
      #                                dt_keeperplot()[[keeper_inputs$keeperplot_yvar]] == max(dt_keeperplot()[[keeper_inputs$keeperplot_yvar]]),
      #                              dt_keeperplot()$plotnames, ''), 
      #               hjust = "inward",
      #               vjust = "inward"),
      #           size = 5,
      #           check_overlap = T,
      #           color = '#ff3300') +
      expand_limits(x = xlim,
                    y = ylim) +
      geom_smooth(method = 'lm', se = F, color = "black") +
      ggtheme
    
    m <- dt_keeperplot() %>% 
      filter(extreme1 >= sort(extreme1, decreasing = T)[1] |
               extreme1 <= sort(extreme1, decreasing = F)[1] |
               extreme2 >= sort(extreme2, decreasing = T)[1] |
               extreme2 <= sort(extreme2, decreasing = F)[1])
    
    a <- list(
      x = m[[keeper_inputs$keeperplot_xvar]],
      y = m[[keeper_inputs$keeperplot_yvar]],
      text = m$plotnames,
      xref = "x",
      yref = "y",
      showarrow = F,
      xanchor = "center",
      yanchor = "top",
      font = list(color = '#ff3300',
                  size = 10)
    )
    
    ggplotly(p,
             tooltip = c("x", "y", "text"),
             width = 700,
             height = 500) %>%
      add_markers() %>%
      layout(annotations = a)
    
  })
  
  output$keeperplot_text <- renderText({
    paste0('<font size = "4">', 
           lm_eqn2(dt_keeperplot(), 
                   paste0('`', keeper_inputs$keeperplot_xvar, '`'),
                   paste0('`', keeper_inputs$keeperplot_yvar, '`')),
           "</font>")
  })
  
  # Keeper downloads ####
  output$keeper_download <- downloadHandler(
    filename = "ASAkeepertable.csv",
    
    content = function(file){
      namesFL <- as.data.frame(do.call("rbind", strsplit(sub(" ", ";", dt_keeperplot()$Keeper), ";")))
      names(namesFL) <- c("First", "Last")
      write.csv(data.frame(namesFL, dt_keeperplot(), check.names = FALSE), file, row.names = FALSE)
    })
  
  # Team shots tables ####
  dt_team <- reactive({
    if(input$team_seasonordate == 'Season'){
      if(input$team_homeadjusted == "Home-adjusted"){
        dt <- teamxgoals.func(teamxgoals.adj, 
                              date1 = as.Date('2000-01-01'), 
                              date2 = as.Date('9999-12-31'),
                              season = input$team_seasonfilter,
                              even = input$team_evenstate,
                              pattern = input$team_pattern,
                              pergame = F,
                              advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                              venue = input$team_home,
                              byseasons = input$team_byseasons,
                              confview = input$team_conferenceview) %>%
          mutate(`Comp ($MM)` = Comp/1000000) %>%
          select(-Comp)  
      } else{
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
                            confview = input$team_conferenceview) %>%
        mutate(`Comp ($MM)` = Comp/1000000) %>%
        select(-Comp)
      }
      
    } else{
      if(input$team_homeadjusted == "Home-adjusted"){
        
        dt <- teamxgoals.func(teamxgoals.adj, 
                              date1 = input$team_date1, 
                              date2 = input$team_date2,
                              season = as.numeric(format(input$team_date1, "%Y")):as.numeric(format(input$team_date2, "%Y")),
                              even = input$team_evenstate,
                              pattern = input$team_pattern,
                              pergame = F,
                              advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                              venue = input$team_home,
                              byseasons = input$team_byseasons,
                              confview = input$team_conferenceview) %>%
          mutate(`Comp ($MM)` = Comp/1000000) %>%
          select(-Comp)
      }else{
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
                            confview = input$team_conferenceview) %>%
        mutate(`Comp ($MM)` = Comp/1000000) %>%
        select(-Comp)
      }
    }
    
    is.num <- sapply(dt, is.numeric)
    dt[is.num] <- lapply(dt[is.num], round, 3)
    
    dt
  })
  
  # Select all controls
  observeEvent(input$team_seasonfilter_selectall,
               { print(c(min(teamxgoals$Season):max(teamxgoals$Season))[-c(length(unique(teamxgoals$Season)))[input$team_subtab == "teamxgoalsplitsplots"]])
                 updateCheckboxGroupInput(
                   session, 
                   "team_seasonfilter", 
                   choices = min(teamxgoals$Season):ifelse(input$team_subtab == "teamxgoalsplitsplots", max(teamxgoals$Season) - 1, max(teamxgoals$Season)),
                   selected = if (input$team_seasonfilter_selectall) min(teamxgoals$Season):ifelse(input$team_subtab == "teamxgoalsplitsplots", max(teamxgoals$Season) - 1, max(teamxgoals$Season))
                 )
               },
               ignoreInit = T)
  
  observeEvent(input$team_seasonfilter2_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "team_seasonfilter2", 
                   choices = min(teamxgoals$Season):ifelse(input$team_subtab == "teamxgoalsplitsplots", max(teamxgoals$Season) - 1, max(teamxgoals$Season)),
                   selected = if (input$team_seasonfilter2_selectall) min(teamxgoals$Season):ifelse(input$team_subtab == "teamxgoalsplitsplots", max(teamxgoals$Season) - 1, max(teamxgoals$Season))
                 )
               },
               ignoreInit = T)

  observeEvent(input$team_pattern_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "team_pattern", 
                   choices = sort(unique(teamxgoals$patternOfPlay.model)),
                   selected = if (input$team_pattern_selectall) sort(unique(teamxgoals$patternOfPlay.model))
                 )
               },
               ignoreInit = T)
  
  
  output$teamtotalxgoalswest <- DT::renderDataTable({
    dt <- dt_team()
    if('Conf' %in% names(dt)){
      dt <- dt %>%
        filter(Conf == 'west') %>%
        select(-Conf)
    }
    
    if(input$team_advanced == "Basic stats"){
      columns.perc1 <- c('SoT%F', 'SoT%A', 'Finish%F', 'Finish%A')      
      columns.dec1 <- c("Pts", "xPts")[c(input$team_homeadjusted == "Home-adjusted", T)]
      columns.dec1.2 <- c("ShtF", "ShtA", "SoTF", "SoTA", "GF", "GA", "GD")[rep(input$team_homeadjusted == "Home-adjusted", 7)]
      columns.dec2 <- c("Gini18")
    } else{
      columns.perc1 <- c() #c("Solo%F", "Solo%A")
      columns.dec1 <- c("xGF", "xGA", "xGD", "GD-xGD", "xPts", "Pts")[c(T, T, T, T, T, input$team_homeadjusted == "Home-adjusted")]
      columns.dec1.2 <- c("ShtF", "ShtA", "GF", "GA", "GD")[rep(input$team_homeadjusted == "Home-adjusted", 5)]
      columns.dec2 <- c("TSR", "Gini18")
    }
    
    if(!input$team_conferenceview | length(input$team_seasonfilter) > 1){
      DT::datatable(dt,
                    rownames = F,
                    options = list(autoWidth = T,
                                 pageLength = 25)) %>%
        formatPercentage(columns = columns.perc1, digits = 1) %>%
        formatRound(columns = columns.dec1, digits = 1) %>%
        formatRound(columns = columns.dec1.2, digits = 1) %>%
        formatRound(columns = columns.dec2, digits = 2) %>%
        formatCurrency(columns = c("Comp ($MM)"),
                       currency = "$",
                       interval = 3,
                       mark = ",",
                       digits = 1) 
    } else{
      DT::datatable(dt,
                    rownames = F,
                    options = list(autoWidth = T,
                                 pageLength = 25,
                                 dom = 't')) %>%
        formatPercentage(columns = columns.perc1, digits = 1) %>%
        formatRound(columns = columns.dec1, digits = 1) %>%
        formatRound(columns = columns.dec1.2, digits = 1) %>%
        formatRound(columns = columns.dec2, digits = 2) %>%
        formatCurrency(columns = c("Comp ($MM)"),
                       currency = "$",
                       interval = 3,
                       mark = ",",
                       digits = 1)
      
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
      columns.dec1 <- c("Pts", "xPts")[c(input$team_homeadjusted == "Home-adjusted", T)]
      columns.dec2 <- c("Gini18")
    } else{
      columns.perc1 <- c() #c("Solo%F", "Solo%A")
      columns.dec1 <- c("xGF", "xGA", "xGD", "GD-xGD", "xPts", "Pts")[c(T, T, T, T, T, input$team_homeadjusted == "Home-adjusted")]
      columns.dec2 <- c("TSR", "Gini18")
    }
    
    DT::datatable(dt,
                  rownames = F,
                  options = list(autoWidth = T,
                               pageLength = 25,
                               dom = 'ft')) %>%
      formatPercentage(columns = columns.perc1, digits = 1) %>%
      formatRound(columns = columns.dec1, digits = 1) %>%
      formatRound(columns = columns.dec2, digits = 2) %>%
      formatCurrency(columns = c("Comp ($MM)"),
                     currency = "$",
                     interval = 3,
                     mark = ",",
                     digits = 1)
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
      if(input$team_homeadjusted == "Home-adjusted"){
        dt <- teamxgoals.func(teamxgoals.adj, 
                              date1 = as.Date('2000-01-01'), 
                              date2 = as.Date('9999-12-31'),
                              season = input$team_seasonfilter,
                              even = input$team_evenstate,
                              pattern = input$team_pattern,
                              pergame = T,
                              advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                              venue = input$team_home,
                              byseasons = input$team_byseasons,
                              confview = input$team_conferenceview) %>%
          mutate(`Comp ($MM)` = Comp/1000000) %>%
          select(-Comp) 
      } else{
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
                            confview = input$team_conferenceview) %>%
        mutate(`Comp ($MM)` = Comp/1000000) %>%
        select(-Comp)
      }
      
    } else{
      if(input$team_homeadjusted == "Home-adjusted"){
        dt <- teamxgoals.func(teamxgoals.adj, 
                              date1 = input$team_date1, 
                              date2 = input$team_date2,
                              season = as.numeric(format(input$team_date1, "%Y")):as.numeric(format(input$team_date2, "%Y")),
                              even = input$team_evenstate,
                              pattern = input$team_pattern,
                              pergame = T,
                              advanced = ifelse(input$team_advanced == 'Basic stats', F, T),
                              venue = input$team_home,
                              byseasons = input$team_byseasons,
                              confview = input$team_conferenceview) %>%
          mutate(`Comp ($MM)` = Comp/1000000) %>%
          select(-Comp)
      }else{
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
                            confview = input$team_conferenceview) %>%
        mutate(`Comp ($MM)` = Comp/1000000) %>%
        select(-Comp)
      }
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
      columns.dec2 <- c("GF", "GA", "GD", "Pts", "xPts")
    } else{
      columns.perc1 <- c() #c("Solo%F", "Solo%A")
      columns.dec1 <- c("ShtF", "ShtA")
      columns.dec2 <- c("xGF", "xGA", "xGD", "GF", "GA", "GD", "GD-xGD", "TSR", "Pts", "xPts")
    }
    
    DT::datatable(dt,
              rownames = F,
              options = list(autoWidth = T,
                           pageLength = 25,
                           dom = 'ft')) %>%
      formatPercentage(columns = columns.perc1, digits = 1) %>%
      formatRound(columns = columns.dec1, digits = 1) %>%
      formatRound(columns = columns.dec2, digits = 2) %>%
      formatCurrency(columns = c("Comp ($MM)"),
                     currency = "$",
                     interval = 3,
                     mark = ",",
                     digits = 1)
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
      columns.dec2 <- c("GF", "GA", "GD", "Pts", "Gini18", "xPts")
    } else{
      columns.perc1 <- c() #c("Solo%F", "Solo%A")
      columns.dec1 <- c("ShtF", "ShtA")
      columns.dec2 <- c("xGF", "xGA", "xGD", "GF", "GA", "GD", "GD-xGD", "TSR", "Pts", "Gini18", "xPts")
    }
    
    DT::datatable(dt,
              rownames = F,
              options = list(autoWidth = T,
                           pageLength = 25,
                           dom = 'ft')) %>%
      formatPercentage(columns = columns.perc1, digits = 1) %>%
      formatRound(columns = columns.dec1, digits = 1) %>%
      formatRound(columns = columns.dec2, digits = 2) %>%
      formatCurrency(columns = c("Comp ($MM)"),
                     currency = "$",
                     interval = 3,
                     mark = ",",
                     digits = 1)
  })
  
  output$team_download_pergame <- downloadHandler(
    filename = 'ASAteamtable_pergame.csv',
    
    content = function(file){
      write.csv(dt_team_pergame(), file, row.names = F)
    }
  )
  
  # Team reactive values ####
  # team_inputs <- reactiveValues(team_seasonordate = 'Season',
  #                               team_date1 = as.Date('2000-01-01'),
  #                               team_date2 = as.Date('9999-12-31'),
  #                               team_seasonfilter = max(teamxgoals$Season),
  #                               team_byseasons = T,
  #                               team_conferenceview = T,
  #                               team_advanced = 'Basic stats',
  #                               team_home = c('Home', 'Away'),
  #                               team_pattern =  'All',
  #                               team_evenstate = F,
  #                               teamplot_xvar = 'xGF',
  #                               teamplot_yvar = 'GF')
  # 
  # observeEvent(input$team_action,
  #              {
  #                team_inputs$team_seasonordate <- input$team_seasonordate
  #                team_inputs$team_date1 <- input$team_date1
  #                team_inputs$team_date2 <- input$team_date2
  #                team_inputs$team_seasonfilter <- input$team_seasonfilter
  #                team_inputs$team_conferenceview <- input$team_conferenceview
  #                team_inputs$team_byseasons <- input$team_byseasons
  #                team_inputs$team_advanced <- input$team_advanced
  #                team_inputs$team_home <- input$team_home
  #                team_inputs$team_pattern <- input$team_pattern
  #                team_inputs$team_evenstate <- input$team_evenstate
  #                team_inputs$teamplot_xvar <- input$teamplot_xvar
  #                team_inputs$teamplot_yvar <- input$teamplot_yvar
  #                
  #              })
  
  # Team shooting plots ####
  dt_teamshots_plot <- reactive({
    
    if(input$team_seasonordate == 'Season'){
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = as.Date('2000-01-01'), 
                            date2 = as.Date('9999-12-31'),
                            season = input$team_seasonfilter,
                            even = input$team_evenstate,
                            pattern = input$team_pattern,
                            pergame = T,
                            advanced = ifelse(input$team_advanced == 'Basic stats', T, F), # Choose opposite of what is selected
                            venue = input$team_home,
                            byseasons = input$team_byseasons,
                            confview = F)
      
    } else{
      dt <- teamxgoals.func(teamxgoals, 
                            date1 = input$team_date1, 
                            date2 = input$team_date2,
                            season = as.numeric(format(input$team_date1, "%Y")):as.numeric(format(input$team_date2, "%Y")),
                            even = input$team_evenstate,
                            pattern = input$team_pattern,
                            pergame = T,
                            advanced = ifelse(input$team_advanced == 'Basic stats', T, F), # Choose opposite of what is selected
                            venue = input$team_home,
                            byseasons = input$team_byseasons,
                            confview = F)
    }
    
    dt %>%
      left_join(dt_team_pergame() %>% select(one_of(c("Team", "Season", 
                                                      setdiff(names(dt_team_pergame()), names(dt))))),
                by = c("Team", "Season")[c(T, input$team_byseasons)]) %>%
      select(-one_of(c("Conf")))
  })
  
  teamshooting_plotvalues <- reactiveValues(teamplot_xvar = "xGF", teamplot_yvar = "GF")
  
  observeEvent({
    input$teamplot_xvar 
    input$teamplot_yvar
  }, 
  {
    teamshooting_plotvalues$teamplot_xvar <- input$teamplot_xvar
    teamshooting_plotvalues$teamplot_yvar <- input$teamplot_yvar
  })
  
  observeEvent(dt_teamshots_plot(), {
    updateSelectInput(session,
                      inputId = 'teamplot_xvar',
                      label = 'X-axis variable',
                      choices = setdiff(names(dt_teamshots_plot()), c("Team", "Season", "Games", "Conf")),
                      selected = teamshooting_plotvalues$teamplot_xvar)
    
    updateSelectInput(session,
                      inputId = 'teamplot_yvar',
                      label = 'Y-axis variable',
                      choices = setdiff(names(dt_teamshots_plot()), c("Team", "Season", "Games", "Conf")),
                      selected = teamshooting_plotvalues$teamplot_yvar)
  })
  
  output$teamplot <- renderPlotly({
    req(input$teamplot_xvar, input$teamplot_yvar)
    dt <- dt_teamshots_plot()
    
    xlim <- min(dt[[input$teamplot_xvar]]) - 0.05*(max(dt[[input$teamplot_xvar]]) - min(dt[[input$teamplot_xvar]]))
    ylim <- min(dt[[input$teamplot_yvar]]) - 0.05*(max(dt[[input$teamplot_yvar]]) - min(dt[[input$teamplot_yvar]]))
    
    dt[["extreme1"]] <- rank(dt[[input$teamplot_xvar]], ties.method = "random")
    dt[["extreme2"]] <- rank(dt[[input$teamplot_yvar]], ties.method = "random")
    
    dt[[input$teamplot_xvar]] <- round(dt[[input$teamplot_xvar]], 3)
    dt[[input$teamplot_yvar]] <- round(dt[[input$teamplot_yvar]], 3)
    
    if(length(unique(dt$Season)) > 1){
      dt[['plotnames']] <- paste(unlist(lapply(strsplit(dt$Team, " "), function(x) { return(x[length(x)]) })), dt$Season)
    }else{
      dt[['plotnames']] <- unlist(lapply(strsplit(dt$Team, " "), function(x) { return(x[length(x)]) }))
    }
    
    p <- dt  %>%
      ggplot(
        aes_string(x = paste0('`', input$teamplot_xvar, '`'),
                   y = paste0('`', input$teamplot_yvar, '`'))) +
      geom_point(aes(text = plotnames), color = '#0000cc') +
      expand_limits(x = xlim,
                    y = ylim) +
      geom_smooth(method = 'lm', se = F) +
      ggtheme
    
    m <- dt %>% 
      ungroup() %>%
      filter(extreme1 >= sort(extreme1, decreasing = T)[2] |
               extreme1 <= sort(extreme1, decreasing = F)[2] |
               extreme2 >= sort(extreme2, decreasing = T)[2] |
               extreme2 <= sort(extreme2, decreasing = F)[2])
    
    a <- list(
      x = m[[input$teamplot_xvar]],
      y = m[[input$teamplot_yvar]],
      text = m$plotnames,
      xref = "x",
      yref = "y",
      showarrow = F,
      xanchor = "center",
      yanchor = "top",
      font = list(color = '#ff3300',
                  size = 10)
    )
    
    ggplotly(p,
             tooltip = c("x", "y", "text"),
             width = 700,
             height = 500) %>%
      add_markers() %>%
      layout(annotations = a)
    
  })
  
  output$teamshootingplot_text <- renderText({
    req(input$teamplot_xvar, input$teamplot_yvar)
    paste0('<font size = "4">',
           lm_eqn2(dt_teamshots_plot(),
                   paste0('`', input$teamplot_xvar, '`'),
                   paste0('`', input$teamplot_yvar, '`')),
           "</font>")
  })
  
  # Team split plots ####
  dt_teamshotssplits_plot <- reactive({
    dt <- teamshootingsplits.func(teamxgoals = teamxgoals,
                                  game_split = input$teamsplitsplot_split,
                                  season = input$team_seasonfilter2,
                                  even = input$team_evenstate,
                                  pattern = input$team_pattern)
    
    dt
    
  })
  
  teamshootingsplits_plotvalues <- reactiveValues(teamsplitsplot_xvar = "xGD (before split)", teamsplitsplot_yvar = "GD (after split)")
  
  observeEvent({
    input$teamsplitsplot_xvar 
    input$teamsplitsplot_yvar
  }, 
  {
    teamshootingsplits_plotvalues$teamsplitsplot_xvar <- input$teamsplitsplot_xvar
    teamshootingsplits_plotvalues$teamsplitsplot_yvar <- input$teamsplitsplot_yvar
  })
  
  observeEvent(dt_teamshotssplits_plot(), {
    updateSelectInput(session,
                      inputId = 'teamsplitsplot_xvar',
                      label = 'X-axis variable',
                      choices = grep("before", names(dt_teamshotssplits_plot()), value = T),
                      selected = teamshootingsplits_plotvalues$teamsplitsplot_xvar)
    
    updateSelectInput(session,
                      inputId = 'teamsplitsplot_yvar',
                      label = 'Y-axis variable',
                      choices = grep("after", names(dt_teamshotssplits_plot()), value = T),
                      selected = teamshootingsplits_plotvalues$teamsplitsplot_yvar)
  })
  
  output$teamsplitsplot <- renderPlotly({
    req(input$teamsplitsplot_xvar, input$teamsplitsplot_yvar)
    dt <- dt_teamshotssplits_plot()
    
    xlim <- min(dt[[input$teamsplitsplot_xvar]]) - 0.05*(max(dt[[input$teamsplitsplot_xvar]]) - min(dt[[input$teamsplitsplot_xvar]]))
    ylim <- min(dt[[input$teamsplitsplot_yvar]]) - 0.05*(max(dt[[input$teamsplitsplot_yvar]]) - min(dt[[input$teamsplitsplot_yvar]]))
    
    dt[["extreme1"]] <- rank(dt[[input$teamsplitsplot_xvar]], ties.method = "random")
    dt[["extreme2"]] <- rank(dt[[input$teamsplitsplot_yvar]], ties.method = "random")
    
    dt[[input$teamsplitsplot_xvar]] <- round(dt[[input$teamsplitsplot_xvar]], 3)
    dt[[input$teamsplitsplot_yvar]] <- round(dt[[input$teamsplitsplot_yvar]], 3)
    
    if(length(unique(dt$Season)) > 1){
      dt[['plotnames']] <- paste(unlist(lapply(strsplit(dt$Team, " "), function(x) { return(x[length(x)]) })), dt$Season)
    }else{
      dt[['plotnames']] <- unlist(lapply(strsplit(dt$Team, " "), function(x) { return(x[length(x)]) }))
    }
    
    p <- dt  %>%
      ggplot(
        aes_string(x = paste0('`', input$teamsplitsplot_xvar, '`'),
                   y = paste0('`', input$teamsplitsplot_yvar, '`'))) +
      geom_point(aes(text = plotnames), color = '#0000cc') +
      expand_limits(x = xlim,
                    y = ylim) +
      geom_smooth(method = 'lm', se = F) +
      ggtheme
    
    m <- dt %>% 
      ungroup() %>%
      filter(extreme1 >= sort(extreme1, decreasing = T)[2] |
               extreme1 <= sort(extreme1, decreasing = F)[2] |
               extreme2 >= sort(extreme2, decreasing = T)[2] |
               extreme2 <= sort(extreme2, decreasing = F)[2])
    
    a <- list(
      x = m[[input$teamsplitsplot_xvar]],
      y = m[[input$teamsplitsplot_yvar]],
      text = m$plotnames,
      xref = "x",
      yref = "y",
      showarrow = F,
      xanchor = "center",
      yanchor = "top",
      font = list(color = '#ff3300',
                  size = 10)
    )
    
    ggplotly(p,
             tooltip = c("x", "y", "text"),
             width = 700,
             height = 500) %>%
      add_markers() %>%
      layout(annotations = a)
    
  })
  
  output$teamshootingsplitsplot_text <- renderText({
    req(input$teamsplitsplot_xvar, input$teamsplitsplot_yvar)
    paste0('<font size = "4">',
           lm_eqn2(dt_teamshotssplits_plot(),
                   paste0('`', input$teamsplitsplot_xvar, '`'),
                   paste0('`', input$teamsplitsplot_yvar, '`')),
           "</font>")
  })
  
  # Team shooting splits downloads ####
  output$team_download_splits <- downloadHandler(
    filename = 'ASAteamtable_splits.csv',
    
    content = function(file){
      write.csv(dt_teamshotssplits_plot(), file, row.names = F)
    }
  )
  
  # Team passing tables ####
  dt_team_passing <- reactive({
    if(input$teampassing_seasonordate == "Season"){
      dt <- teampassing.func(offense = teampassing.offense,
                             defense = teampassing.defense,
                             date1 = as.Date('2000-01-01'), 
                             date2 = as.Date('9999-12-31'),
                             season = input$teampassing_seasonfilter,
                             byseasons = input$teampassing_byseasons,
                             third.filter = input$teampassing_thirdfilter) 
    } else{
      dt <- teampassing.func(offense = teampassing.offense,
                             defense = teampassing.defense,
                             date1 = input$teampassing_date1, 
                             date2 = input$teampassing_date2,
                             season = as.numeric(format(input$teampassing_date1, "%Y")):as.numeric(format(input$teampassing_date2, "%Y")),
                             byseasons = input$teampassing_byseasons,
                             third.filter = input$teampassing_thirdfilter) 
    }
    
    is.num <- sapply(dt, is.numeric)
    dt[is.num] <- lapply(dt[is.num], round, 3)
    
    dt
  })
  
  dt_team_passing_pergame <- reactive({
    if(input$teampassing_seasonordate == "Season"){
      dt <- teampassing.func(offense = teampassing.offense,
                             defense = teampassing.defense,
                             date1 = as.Date('2000-01-01'), 
                             date2 = as.Date('9999-12-31'),
                             season = input$teampassing_seasonfilter,
                             byseasons = input$teampassing_byseasons,
                             third.filter = input$teampassing_thirdfilter,
                             pergame = T) 
    } else{
      dt <- teampassing.func(offense = teampassing.offense,
                             defense = teampassing.defense,
                             date1 = input$teampassing_date1, 
                             date2 = input$teampassing_date2,
                             season = as.numeric(format(input$teampassing_date1, "%Y")):as.numeric(format(input$teampassing_date2, "%Y")),
                             byseasons = input$teampassing_byseasons,
                             third.filter = input$teampassing_thirdfilter,
                             pergame = T) 
    } 
    
    is.num <- sapply(dt, is.numeric)
    dt[is.num] <- lapply(dt[is.num], round, 3)
    
    dt
  })
  
  
  # Select all checkboxes  
  observeEvent(input$teampassing_seasonfilter_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "teampassing_seasonfilter", 
                   choices = min(teampassing.offense$year):max(teampassing.offense$year),
                   selected = if (input$teampassing_seasonfilter_selectall) min(teampassing.offense$year):max(teampassing.offense$year)
                 )
               },
               ignoreInit = T)
  
  observeEvent(input$teampassing_thirdfilter_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "teampassing_thirdfilter", 
                   choices = c("Defensive" = "Def", "Middle" = "Mid", "Attacking" = "Att"),
                   selected = if (input$teampassing_thirdfilter_selectall) c("Defensive" = "Def", "Middle" = "Mid", "Attacking" = "Att")
                 )
               },
               ignoreInit = T)
  
  output$teampassing_total <- DT::renderDataTable({
    dt <- dt_team_passing() %>%
      mutate(`Comp ($MM)` = Comp/1000000) %>%
      select(-Comp)
    
    columns.perc1 <- c("PctF", "xPctF", "PctA", "xPctA")
    columns.dec1 <- c("ScoreF", "ScoreA", "ScoreDiff")
    columns.dec2 <- c("Per100F", "Per100A", "VertF", "VertA", "VertDiff")
    
    DT::datatable(dt,
                  rownames = F,
                  options = list(autoWidth = T,
                               pageLength = 25)) %>%
      formatPercentage(columns = columns.perc1, digits = 1) %>%
      formatRound(columns = columns.dec1, digits = 1) %>%
      formatRound(columns = columns.dec2, digits = 2) %>%
      formatCurrency(columns = c("Comp ($MM)"),
                     currency = "$",
                     interval = 3,
                     mark = ",",
                     digits = 1)
  })
  
  output$teampassing_pergame <- DT::renderDataTable({
    dt <- dt_team_passing_pergame() %>%
      mutate(`Comp ($MM)` = Comp/1000000) %>%
      select(-Comp)
    
    columns.perc1 <- c("PctF", "xPctF", "PctA", "xPctA")
    columns.dec1 <- c("PassF/g", "PassA/g")
    columns.dec2 <- c("Per100F", "Per100A", "VertF", "VertA", "VertDiff", "ScoreF/g", "ScoreA/g", "ScoreDiff/g")
    
    DT::datatable(dt,
                  rownames = F,
                  options = list(autoWidth = T,
                               pageLength = 25)) %>%
      formatPercentage(columns = columns.perc1, digits = 1) %>%
      formatRound(columns = columns.dec1, digits = 1) %>%
      formatRound(columns = columns.dec2, digits = 2) %>%
      formatCurrency(columns = c("Comp ($MM)"),
                     currency = "$",
                     interval = 3,
                     mark = ",",
                     digits = 1)
  })
  
  # Team passing plots ####
  dt_teampassing_plot <- reactive({
    dt_team_passing_pergame()
    # If you ever want to join totals and pergame (currently just plotting per game)    
    # dt_team_passing() %>%
    #   left_join(dt_team_passing_pergame() %>% 
    #               select(one_of(c("Team", "Season", 
    #                               setdiff(names(dt_team_passing_pergame()), names(dt_team_passing()))))),
    #             by = c("Team", "Season")[c(T, input$teampassing_byseasons)]) %>%
    #   select(-one_of(c("Conf")))
    
  })
  
  teampassing_plotvalues <- reactiveValues(teampassingplot_xvar = "xPctF", teampassingplot_yvar = "PctF")
  
  observeEvent({
    input$teampassingplot_xvar 
    input$teampassingplot_yvar
  }, 
  {
    teampassing_plotvalues$teampassingplot_xvar <- input$teampassingplot_xvar
    teampassing_plotvalues$teampassingplot_yvar <- input$teampassingplot_yvar
  })
  
  observeEvent(dt_teampassing_plot(), {
    updateSelectInput(session,
                      inputId = 'teampassingplot_xvar',
                      label = 'X-axis variable',
                      choices = setdiff(names(dt_teampassing_plot()), c("Team", "Season", "Games", "Conf")),
                      selected = teampassing_plotvalues$teampassingplot_xvar)
    
    updateSelectInput(session,
                      inputId = 'teampassingplot_yvar',
                      label = 'Y-axis variable',
                      choices = setdiff(names(dt_teampassing_plot()), c("Team", "Season", "Games", "Conf")),
                      selected = teampassing_plotvalues$teampassingplot_yvar)
  })
  
  output$teampassingplot <- renderPlotly({
    req(input$teampassingplot_xvar, input$teampassingplot_yvar)
    dt <- dt_teampassing_plot()
    
    xlim <- min(dt[[input$teampassingplot_xvar]]) - 0.05*(max(dt[[input$teampassingplot_xvar]]) - min(dt[[input$teampassingplot_xvar]]))
    ylim <- min(dt[[input$teampassingplot_yvar]]) - 0.05*(max(dt[[input$teampassingplot_yvar]]) - min(dt[[input$teampassingplot_yvar]]))
    
    dt[["extreme1"]] <- rank(dt[[input$teampassingplot_xvar]], ties.method = "random")
    dt[["extreme2"]] <- rank(dt[[input$teampassingplot_yvar]], ties.method = "random")
    
    dt[[input$teampassingplot_xvar]] <- round(dt[[input$teampassingplot_xvar]], 3)
    dt[[input$teampassingplot_yvar]] <- round(dt[[input$teampassingplot_yvar]], 3)
    
    if(length(unique(dt$Season)) > 1){
      dt[['plotnames']] <- paste(unlist(lapply(strsplit(as.character(dt$Team), " "), function(x) { return(x[length(x)]) })), dt$Season)
    }else{
      dt[['plotnames']] <- unlist(lapply(strsplit(as.character(dt$Team), " "), function(x) { return(x[length(x)]) }))
    }
    
    p <- dt  %>%
      ggplot(aes_string(x = paste0('`', input$teampassingplot_xvar, '`'),
                        y = paste0('`', input$teampassingplot_yvar, '`'))) +
      geom_point(aes(text = plotnames), color = '#0000cc') +
      expand_limits(x = xlim,
                    y = ylim) +
      geom_smooth(method = 'lm', se = F) +
      ggtheme
    
    m <- dt %>% 
      ungroup() %>%
      filter(extreme1 >= sort(extreme1, decreasing = T)[2] |
               extreme1 <= sort(extreme1, decreasing = F)[2] |
               extreme2 >= sort(extreme2, decreasing = T)[2] |
               extreme2 <= sort(extreme2, decreasing = F)[2])
    
    a <- list(
      x = m[[input$teampassingplot_xvar]],
      y = m[[input$teampassingplot_yvar]],
      text = m$plotnames,
      xref = "x",
      yref = "y",
      showarrow = F,
      xanchor = "center",
      yanchor = "top",
      font = list(color = '#ff3300',
                  size = 10)
    )
    
    ggplotly(p,
             tooltip = c("x", "y", "text"),
             width = 700,
             height = 500) %>%
      add_markers() %>%
      layout(annotations = a)
    
  })
  
  output$teampassingplot_text <- renderText({
    req(input$teampassingplot_xvar, input$teampassingplot_yvar)
    
    paste0('<font size = "4">',
           lm_eqn2(dt_teampassing_plot(),
                   paste0('`', input$teampassingplot_xvar, '`'),
                   paste0('`', input$teampassingplot_yvar, '`')),
           "</font>")
  })
  
  # Team passing downloads ####
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
  
  # Select all/none ####
  observeEvent(input$teambygame_seasonfilter_selectall,
               { 
                 updateCheckboxGroupInput(
                   session, 
                   "teambygame_seasonfilter", 
                   choices = min(teamxgoals$Season):max(teamxgoals$Season),
                   selected = if (input$teambygame_seasonfilter_selectall) min(teamxgoals$Season):max(teamxgoals$Season)
                 )
               },
               ignoreInit = T)
  
  output$teamxgoalsbygame <- DT::renderDataTable({
    columns.dec1 <- c("HxPts", "AxPts")
    DT::datatable(dt_bygame(),
              rownames = F,
              options = list(autoWidth = T,
                           pageLength = 25,
                           lengthMenu = seq(25, 100, 25))) %>%
      formatRound(columns = columns.dec1, digits = 1)
  })
  
  output$teambygame_download <- downloadHandler(
    filename = 'ASAxGoals_gamebygame.csv',
    
    content = function(file){
      write.csv(dt_bygame(), file, row.names = F)
    }
  )
  
  # Predictions ####
  
  # Win probability model ####
  output$winproboutput <- DT::renderDataTable({
    hwinprob <- predict(winmodel.purged, 
                       data.frame(minute = input$minute_winprob,
                                  gamestate = input$gamestate_winprob,
                                  playerdiff = input$playerdiff_winprob),
                       type = "response")
    drawprob <- (1 - hwinprob)*predict(drawmodel.purged, 
                       data.frame(minute = input$minute_winprob,
                                  gamestate = input$gamestate_winprob,
                                  playerdiff = input$playerdiff_winprob),
                       type = "response")
    awinprob <- 1 - hwinprob - drawprob
    
    hxpts = 3*hwinprob + drawprob
    axpts = 3*awinprob + drawprob
    
    DT::datatable(data.frame(`Home win%` = hwinprob,
                             `Draw%` = drawprob,
                             `Away win%` = awinprob,
                             `Home xPts` = hxpts,
                             `Away xPts` = axpts,
                             check.names = F),
                  rownames = F,
                  options = list(autoWidth = T,
                               dom = "t")) %>%
      formatPercentage(columns = c("Home win%", "Draw%", "Away win%"), 
                       digits = 1) %>%
      formatRound(columns = c("Home xPts", "Away xPts"),
                  digits = 1)
    
  })
  
  # winexpreactive <- reactive({
  #   
  #   if(!is.na(input$gamestate_winprob)){
  #   data.frame(winexptable %>%
  #                rename(ModelWin = Win,
  #                       ModelDraw = Draw,
  #                       ModelLoss = Loss,
  #                       ActDraw = ActTie),
  #              check.names = F)
  #   }
  # })
  
  output$download_winprob <- downloadHandler(
    filename = "ASAWinProbByGameState.csv",
    content = function(file){
      write.csv(winexptable, 
                file, 
                row.names = F)
    }
  )
  
  # Team reactive values ####
  winprob_userinputs <- reactiveValues(
    hometeam = as.character(c()),
    awayteam = as.character(c()),
    date = "4/10/1986",
    homegoals = as.character(c()),
    awaygoals = as.character(c()),
    homereds = as.character(c()),
    awayreds = as.character(c()))

  observeEvent(input$winprob_userinput_action,
               {
                 winprob_userinputs$hometeam <- input$winprob_userinput_hometeam
                 winprob_userinputs$awayteam <- input$winprob_userinput_awayteam
                 winprob_userinputs$date <- input$winprob_userinput_date
                 winprob_userinputs$homegoals <- input$winprob_userinput_homegoals
                 winprob_userinputs$awaygoals <- input$winprob_userinput_awaygoals
                 winprob_userinputs$homereds <- input$winprob_userinput_homereds
                 winprob_userinputs$awayreds <- input$winprob_userinput_awayreds
               })
  
  output$winprobchart_userinput <- renderPlot({
    homegoaltimes <- na.omit(as.numeric(sapply(strsplit(winprob_userinputs$homegoals, ","), 
                                       function(x) as.numeric(trimws(x)))))
    awaygoaltimes <- na.omit(as.numeric(sapply(strsplit(winprob_userinputs$awaygoals, ","), 
                                       function(x) as.numeric(trimws(x)))))
    homeredtimes <- na.omit(as.numeric(sapply(strsplit(winprob_userinputs$homereds, ","), 
                                       function(x) as.numeric(trimws(x)))))
    awayredtimes <- na.omit(as.numeric(sapply(strsplit(winprob_userinputs$awayreds, ","), 
                                       function(x) as.numeric(trimws(x)))))
    
    minutes <- c(homegoaltimes, awaygoaltimes, homeredtimes, awayredtimes)
    
    action.input <- data.frame(minute = c(minutes, max(95, minutes)),
                               hteam = c(rep(winprob_userinputs$hometeam, 
                                             length(c(homegoaltimes, awaygoaltimes,
                                                      homeredtimes, awayredtimes))),
                                         ifelse(length(winprob_userinputs$hometeam) == 1,
                                                winprob_userinputs$hometeam,
                                                "Flounders")), 
                               ateam = c(rep(winprob_userinputs$awayteam,
                                             length(c(homegoaltimes, awaygoaltimes,
                                                      homeredtimes, awayredtimes))),
                                         ifelse(length(winprob_userinputs$awayteam) == 1,
                                                winprob_userinputs$awayteam,
                                                "Mighty Timbers")),
                               date = winprob_userinputs$date,
                               team = c(rep(winprob_userinputs$hometeam, length(homegoaltimes)),
                                        rep(winprob_userinputs$awayteam, length(awaygoaltimes)),
                                        rep(winprob_userinputs$hometeam, length(homeredtimes)),
                                        rep(winprob_userinputs$awayteam, length(awayredtimes)),
                                        "None"),
                               Action = c(rep("Goal", length(c(homegoaltimes, awaygoaltimes))),
                                          rep("Red card", length(c(homeredtimes, awayredtimes))),
                                          "lastminute"),
                               final = 0,
                               gameID = 0,
                               stringsAsFactors = F)
    
    action.input <- action.input %>%
      mutate(half = ifelse(minute <= 45, 1, 2)) %>%
      arrange(half, minute) %>%
      mutate(gamestate = cumsum((team == hteam & Action == "Goal") -
                                   (team == ateam & Action == "Goal")),
             playerdiff = cumsum((team == ateam & Action == "Red card") -
                                   (team == hteam & Action == "Red card")))
    
        winprobchart.func(action.input,
                      winmodel.purged,
                      drawmodel.purged)
  },
  width = 600, height = 400)
  
  # Playoffs seeding ####
  output$playoffsseeding_west <- DT::renderDataTable({
    
    DT::datatable(playoffsseeding_west %>% 
                    select(-Bye) %>%
                    left_join(playoffsseeding_west_last %>%
                                select(-Bye) %>%
                                select(Team,
                                       Playoffs_last = Playoffs,
                                       Shield_last = Shield),
                              by = "Team") %>%
                    mutate(POChange = Playoffs - Playoffs_last,
                           SSChange = Shield - Shield_last) %>%
                    select(Team, `1`:Playoffs, POChange, Shield, SSChange),
                  rownames = F,
                  options = list(autoWidth = T,
                               pageLength = 15,
                               dom = "t")) %>%
      formatPercentage(columns = c("1", "2", "3", "4", "5", "6", "7", "Playoffs", "POChange", "Shield", "SSChange"), digits = 1)
  })
  
  output$playoffsseeding_east <- DT::renderDataTable({

    DT::datatable(playoffsseeding_east %>% 
                    select(-Bye) %>%
                    left_join(playoffsseeding_east_last %>%
                                select(-Bye) %>%
                                select(Team,
                                       Playoffs_last = Playoffs,
                                       Shield_last = Shield),
                              by = "Team") %>%
                    mutate(POChange = Playoffs - Playoffs_last,
                           SSChange = Shield - Shield_last) %>%
                    select(Team, `1`:Playoffs, POChange, Shield, SSChange),
                  rownames = F,
                  options = list(autoWidth = T,
                               pageLength = 15,
                               dom = "t")) %>%
      formatPercentage(columns = c("1", "2", "3", "4", "5", "6", "7", "Playoffs", "POChange", "Shield", "SSChange"), digits = 1)
  })
  
  # MLS Cup predictions ####
  
  output$cupchances_table <- DT::renderDataTable({
    
    DT::datatable(cupchances %>%
                    left_join(cupchances_last %>%
                                select(Team,
                                       Champs_last = Champs),
                              by = "Team") %>%
                    mutate(ChampsChange = Champs - Champs_last) %>%
                    select(-Champs_last),
                  rownames = F,
                  options = list(autoWidth = T,
                               pageLength = 15,
                               dom = "t")) %>%
      formatPercentage(columns = c("Conf Semis", "Conf Finals", "Finals", "Champs", "ChampsChange"), digits = 1)
  })
  
  # Salary ####
  # Player salary inputs ####
  playersalaries_inputs <- reactiveValues(posfilter = c("GK", "D", "B", "M", "A", "F"),
                                          teamfilter = "All",
                                          extract_date1 = max(salary.data$Date),
                                          extract_date2 = max(salary.data$Date),
                                          aggregate = F)
  
  observeEvent(input$playersalaries_action,
               {
                 playersalaries_inputs$posfilter <- input$playersalaries_posfilter
                 playersalaries_inputs$teamfilter <- input$playersalaries_teamfilter
                 playersalaries_inputs$extract_date1 <- input$playersalaries_date1
                 playersalaries_inputs$extract_date2 <- input$playersalaries_date2
                 playersalaries_inputs$aggregate <- input$playersalaries_aggregate
               })
  
  dt_playersalaries <- reactive({
    if(playersalaries_inputs$teamfilter == "All"){
      teamfilter <- unique(salary.data$Team)
    } else{
      teamfilter <- playersalaries_inputs$teamfilter 
    }
    
    dt <- player.salary.func(salary.data = salary.data,
                             teamfilter = teamfilter,
                             posfilter = playersalaries_inputs$posfilter,
                             extract.date1 = playersalaries_inputs$extract_date1,
                             extract.date2 = playersalaries_inputs$extract_date2,
                             aggregate = playersalaries_inputs$aggregate)
    dt
  })
  
  output$playersalaries <- DT::renderDataTable({
    if(playersalaries_inputs$aggregate){
      DT::datatable(dt_playersalaries(),
                    rownames = F,
                    options = list(autoWidth = T,
                                 pageLength = 25)) %>%
        formatCurrency(columns = c("Base", "Guaranteed", "Guar Total", "Guar Avg")[c(!playersalaries_inputs$aggregate, !playersalaries_inputs$aggregate, playersalaries_inputs$aggregate, playersalaries_inputs$aggregate)],
                       currency = "$",
                       interval = 3,
                       mark = ",",
                       digits = 0) 
    } else{
    DT::datatable(dt_playersalaries() %>% mutate(Date = format(Date, "%m/%d/%Y")),
                 rownames = F,
                 options = list(autoWidth = T,
                              pageLength = 25)) %>%
      formatCurrency(columns = c("Base", "Guaranteed", "Guar Total", "Guar Avg")[c(!playersalaries_inputs$aggregate, !playersalaries_inputs$aggregate, playersalaries_inputs$aggregate, playersalaries_inputs$aggregate)],
                     currency = "$",
                     interval = 3,
                     mark = ",",
                     digits = 0)
    }
  })
  
  # Player salary downloads ####
  output$playersalaries_download <- downloadHandler(
    filename = paste0("ASA_PlayerSalaries.csv"),
    
    content = function(file){
      write.csv(dt_playersalaries(), file, row.names = FALSE)
    }
  )
  
  # Team salary inputs ####
  teamsalaries_inputs <- reactiveValues(groupby = "Team",
                                        seasonfilter = 2018)
  
  observeEvent(input$teamsalaries_action,
               {
                 teamsalaries_inputs$groupby <- input$teamsalaries_groupby
                 teamsalaries_inputs$seasonfilter <- input$teamsalaries_seasonfilter
               })
  
  dt_teamsalaries <- reactive({
    
    dt <- team.salary.func(salary.data = salary.data,
                           grouping = teamsalaries_inputs$groupby,
                           seasonfilter = teamsalaries_inputs$seasonfilter)
    dt
  })
  
  output$teamsalaries <- DT::renderDataTable({
    DT::datatable(dt_teamsalaries(),
                  rownames = F,
                  options = list(autoWidth = T,
                               pageLength = 25,
                               dom = "t")) %>%
      formatCurrency(columns = c("TotalGuar", "AvgGuar", "MedGuar", "StdDevGuar"),
                     currency = "$",
                     interval = 3,
                     mark = ",",
                     digits = 0)
  })
  
  # Team salary downloads ####
  output$teamsalaries_download <- downloadHandler(
    filename = paste0("ASA_TeamSalaries.csv"),
    
    content = function(file){
      write.csv(dt_teamsalaries(), file, row.names = FALSE)
    }
  )
  
  # Glossary ####
  output$glossary <- DT::renderDataTable({
    DT::datatable(glossary %>% select(c(1, 2, 3)),
                  rownames = F,
                  options = list(autoWidth = T,
                               pageLength = 20,
                               lengthMenu = c(10, 20, 30, 40, 50)))
  })
  
})