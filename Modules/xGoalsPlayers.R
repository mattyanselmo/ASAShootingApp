sidebarxGoalsPlayersUI <- function(id){
  ns <- NS(id)
  
  tagList(
    actionButton(ns('refresh_data'),
                 label = "Refresh filters"),
    # unclear whether the input.seasonordate should be changed to
    # input.ns(seasonordate) - probably going to be an issue
    conditionalPanel(condition = 
                       "(input.ns(seasonordate)== 'Season' && 
                     Math.min(parseInt(input.ns(seasonfilter))) >= 2015) ||
                     (input.ns(seasonordate) == 'Date' && 
                     parseInt(input.ns(date1).substring(0,4)) >= 2015)",
                     numericInput(ns("minimum_minutes"),
                                  label = "Minimum minutes:",
                                  value = 0,
                                  min = 0, max = 10000, step = 500,
                                  width = "75%")),
    numericInput(ns('minimum_shots'),
                 "Minimum shots:",
                 value = 0,
                 min = 0, max = 100, step = 10,
                 width = "75%"),
    numericInput(ns('minimum_keypasses'),
                 'Minimum key passes:',
                 value = 0,
                 min = 0, max = 100, step = 10,
                 width = "75%"),
    radioButtons(ns('seasonordate'),
                 'Filter by:',
                 choices = c('Season', 'Date'),
                 inline = T),
    conditionalPanel(condition = "input.ns(seasonordate) == 'Season'",
                     dropdownButton(inputId = ns("seasonfilter_dropdown"),
                                    label = "Seasons:",
                                    circle = FALSE,
                                    width = "75%",
                                    checkboxInput(ns("seasonfilter_selectall"),
                                                  label = "All/None",
                                                  value = F),
                                    checkboxGroupInput(ns("seasonfilter"),
                                                       label = NULL,
                                                       choices = min(playerxgoals$Season):max(playerxgoals$Season),
                                                       selected = max(playerxgoals$Season)))),
    conditionalPanel(condition = "input.ns(seasonordate) == 'Date'",
                     dateInput(ns('date1'),
                               'From:',
                               value = min(playerxgoals$date[playerxgoals$Season == max(playerxgoals$Season)]),
                               min = min(playerxgoals$date),
                               max = max(playerxgoals$date),
                               format = 'mm/dd/yyyy'),
                     dateInput(ns('date2'),
                               'To:',
                               value = max(playerxgoals$date),
                               min = min(playerxgoals$date),
                               max = max(playerxgoals$date),
                               format = 'mm/dd/yyyy')
    ),   
    br(),
    
    dropdownButton(inputId = ns("teamfilter_dropdown"),
                   label = "Teams:",
                   circle = FALSE,
                   width = "100%",
                   #p(HTML("<b>Select teams:</b>")),
                   checkboxInput(ns("teamfilter_selectall"),
                                 label = "All/None",
                                 value = T),
                   checkboxGroupInput(ns("teamfilter"),
                                      label = NULL,
                                      choices = sort(unique(playerxgoals$team)),
                                      selected = sort(unique(playerxgoals$team)))),
    br(),
    conditionalPanel(condition = 
                       "(input.ns(easonordate) == 'Season' && 
                     Math.min(parseInt(input.ns(seasonfilter))) >= 2015) ||
                     (input.ns(seasonordate) == 'Date' && 
                     parseInt(input.ns(date1).substring(0,4)) >= 2015)",
                     dropdownButton(inputId = ns("position_dropdown"),
                                    label = "Positions:",
                                    circle = FALSE,
                                    width = "75%",
                                    checkboxInput(ns("position_selectall"),
                                                  label = "All/None",
                                                  value = T),
                                    checkboxGroupInput(ns("position_checkbox"),
                                                       label = NULL,
                                                       choices = c("Keeper (GK)" = "GK",
                                                                   "Central Def (CB)" = "CB",
                                                                   "Back (FB/WB)" = "FB/WB",
                                                                   "Midfielder (CM)" = "CM",
                                                                   "Attacking Mid (CAM)" = "CAM",
                                                                   "Defensive Mid (CDM)" = "CDM",
                                                                   "Wing (W)" = "Wing",
                                                                   "Forward (F)" = "F",
                                                                   "Sub (S)" = "sub"),
                                                       selected = c("GK", "CB", "FB/WB", "CM", "CDM", "CAM", "Wing", "F", "sub")))
                     ),
    br(),
    
    dropdownButton(inputId = ns("shooting_pattern_dropdown"),
                   label = "Shot patterns:",
                   circle = FALSE,
                   width = "75%",
                   checkboxInput(ns("shooting_pattern_selectall"),
                                 label = "All/None",
                                 value = T),
                   checkboxGroupInput(ns("shooting_pattern"),
                                      label = NULL,
                                      choices = c("Open play" = "Open", "PK", "Direct FK" = "FK", "Set piece" = "Setpiece"),
                                      selected = c("Open play" = "Open", "PK", "Direct FK" = "FK", "Set piece" = "Setpiece"))),
    
    h5(HTML('<b>Other options:</b>')),
    checkboxInput(ns('split_byteams'),
                  label = 'Split players by teams',
                  value = F),
    checkboxInput(ns('split_byseasons'),
                  label = 'Split players by seasons',
                  value = T)
    )
  
}


###############################################################################
sidebarxGoalsPlayers <- function(input, output, session, data=playerxgoals){
  # Shooter reactive values ####
  # Initial values for filters
  shooter_inputs <- reactiveValues(seasonordate = 'Season',
                                   date1 = as.Date('2000-01-01'),
                                   date2 = as.Date('9999-12-31'),
                                   seasonfilter = max(playerxgoals$Season),
                                   minimum_minutes = 0,
                                   teamfilter = sort(unique(playerxgoals$team)),
                                   minimum_shots = 0,
                                   minimum_keypasses = 0,
                                   position_checkbox = c("GK", "CB", "FB/WB", "CM", "CDM", "CAM", "Wing", "F", "sub"),
                                   split_byteams = F,
                                   split_byseasons = T,
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
  observeEvent(input$refresh_data,
               { 
                 shooter_inputs$seasonordate <- input$seasonordate
                 shooter_inputs$date1 <- input$date1
                 shooter_inputs$date2 <- input$date2
                 shooter_inputs$seasonfilter <- input$seasonfilter
                 shooter_inputs$minimum_minutes <- input$minimum_minutes
                 shooter_inputs$teamfilter <- input$teamfilter
                 shooter_inputs$minimum_shots <- input$minimum_shots
                 shooter_inputs$minimum_keypasses <- input$minimum_keypasses
                 shooter_inputs$position_checkbox <- input$position_checkbox
                 shooter_inputs$split_byteams <- input$split_byteams
                 shooter_inputs$split_byseasons <- input$split_byseasons
                 shooter_inputs$pattern <- input$shooting_pattern
                 shooter_inputs$shooterplot_xvar <- input$shooterplot_xvar
                 shooter_inputs$shooterplot_yvar <- input$shooterplot_yvar
                 # shooter_inputs$shooterplot_per96_xvar <- input$shooterplot_per96_xvar
                 # shooter_inputs$shooterplot_per96_yvar <- input$shooterplot_per96_yvar
               })
  
  # Select all checkboxes  
  observeEvent(input$teamfilter_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "teamfilter", 
                   choices = sort(unique(playerxgoals$team)),
                   selected = if (input$teamfilter_selectall) sort(unique(playerxgoals$team))
                 )
               },
               ignoreInit = T)
  
  observeEvent(input$seasonfilter_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "seasonfilter", 
                   choices = min(playerxgoals$Season):max(playerxgoals$Season),
                   selected = if (input$seasonfilter_selectall) min(playerxgoals$Season):max(playerxgoals$Season)
                 )
               },
               ignoreInit = T)
  
  observeEvent(input$position_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "position_checkbox", 
                   choices = c("Keeper (GK)" = "GK",
                               "Central Def (CB)" = "CB",
                               "Back (FB/WB)" = "FB/WB",
                               "Midfielder (CM)" = "CM",
                               "Attacking Mid (CAM)" = "CAM",
                               "Defensive Mid (CDM)" = "CDM",
                               "Wing (W)" = "Wing",
                               "Forward (F)" = "F",
                               "Sub (S)" = "sub"),
                   selected = if (input$position_selectall) c("GK", "CB", "FB/WB", "CM", "CDM", "CAM", "Wing", "F", "sub")
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
  
  
}
