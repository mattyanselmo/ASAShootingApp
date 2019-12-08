# This is the module to create the xG players scatter plots
xGPlayersScatterUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidPage(fluidRow(
      # These shooterplot_* values are being rendered and updated here
      # but are stored in another module - the xGPlayersSidebar - originally.
      # Need to move that so that the values are stored in this module
      column(3,
             selectInput(ns('shooterplot_xvar'),
                         label = 'X-axis variable',
                         choices = "xG",
                         selected = "xG")),
      column(3,
             selectInput(ns('shooterplot_yvar'),
                         label = 'Y-axis variable',
                         choices = "G",
                         selected = "G")))),
    # What to do with these [ns()?] to make sure they render properly 
    htmlOutput(ns("shooterplot_text")),
    plotlyOutput(ns('shooterplot'))
      )
  
  }

###############################################################################
###############################################################################
###############################################################################

xGPlayersScatter <- function(input, output, session, 
                             dt_total, 
                             dt_per96,
                             shooter_inputs){
  # Shooter scatter plots ####
  dt_playershootingplot <- reactive({
    dt <- dt_total %>%
      left_join(dt_per96,
                by = c("Player", "Team", "Season")[c(TRUE, shooter_inputs$shooting_byteams, shooter_inputs$shooting_byseasons)],
                suffix = c("", "/96"))
    
    dt[["extreme1"]] <- rank(dt[[input$shooterplot_xvar]], ties.method = "random")
    dt[["extreme2"]] <- rank(dt[[input$shooterplot_yvar]], ties.method = "random")
    
    dt[[input$shooterplot_xvar]] <- round(dt[[input$shooterplot_xvar]], 3)
    dt[[input$shooterplot_yvar]] <- round(dt[[input$shooterplot_yvar]], 3)
    
    if(length(unique(dt$Season)) > 1){
      dt[['plotnames']] <- paste(unlist(lapply(strsplit(dt$Player, " "), function(x) { return(x[length(x)]) })), dt$Season)
      
    }else{
      dt[['plotnames']] <- unlist(lapply(strsplit(dt$Player, " "), function(x) { return(x[length(x)]) }))
    }
    dt
  })
  
  playershooting_plotvalues <- reactiveValues(shooterplot_xvar = "xG", shooterplot_yvar = "Goals")
  
  observeEvent(shooter_inputs$shooting_action, {
    choices.total <- names(dt_total)[!(names(dt_total) %in% c("Player", "Team", "Season", "Pos"))]
    if(min(shooter_inputs$seasonfilter) >= 2015){
      choices.96 <- paste0(names(dt_per96)[!(names(dt_per96) %in% c("Player", "Team", "Season", "Pos", "Min", "Comp ($K)"))], "/96")
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
    dt_total
    dt_per96},
    {
      choices.total <- names(dt_total)[!(names(dt_total) %in% c("Player", "Team", "Season", "Pos"))]
      if(min(shooter_inputs$seasonfilter) >= 2015){
        choices.96 <- paste0(names(dt_per96)[!(names(dt_per96) %in% c("Player", "Team", "Season", "Pos", "Min", "Comp ($K)"))], "/96")
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
    # xlim <- min(dt_playershootingplot()[[input$shooterplot_xvar]]) - 0*(max(dt_playershootingplot()[[passer_inputs$passerplot_xvar]]) - min(dt_playershootingplot()[[input$shooterplot_xvar]]))
    # ylim <- min(dt_playershootingplot()[[passer_inputs$passerplot_yvar]]) - 0*(max(dt_playershootingplot()[[input$shooterplot_yvar]]) - min(dt_playershootingplot()[[input$shooterplot_yvar]]))
    
    p <- dt_playershootingplot() %>%
      ggplot(
        aes_string(x = paste0('`', input$shooterplot_xvar, '`'), 
                   y = paste0('`', input$shooterplot_yvar, '`'))) +
      geom_point(aes(text = paste0(plotnames, "<br>Shots:", Shots)), 
                 color = '#0000cc') +
      # expand_limits(x = xlim,
      #               y = ylim) +
      geom_smooth(method = 'lm', se = F, color = "black") +
      ggtheme
    
    m <- dt_playershootingplot() %>% 
      filter(extreme1 >= sort(extreme1, decreasing = T)[1] |
               extreme1 <= sort(extreme1, decreasing = F)[1] |
               extreme2 >= sort(extreme2, decreasing = T)[1] |
               extreme2 <= sort(extreme2, decreasing = F)[1])
    
    a <- list(
      x = m[[input$shooterplot_xvar]],
      y = m[[input$shooterplot_yvar]],
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
                   paste0('`', input$shooterplot_xvar, '`'),
                   paste0('`', input$shooterplot_yvar, '`')),
           "</font>")
  })
  
}