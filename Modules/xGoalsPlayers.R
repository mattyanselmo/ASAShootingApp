sidebarxGoalsPlayersUI <- function(id){
  ns <- NS(id)
  
  
}

actionButton('shooting_action',
             label = "Refresh filters"),
conditionalPanel(condition = 
                   "(input.shooting_seasonordate == 'Season' && 
                 Math.min(parseInt(input.shooting_seasonfilter)) >= 2015) ||
                 (input.shooting_seasonordate == 'Date' && 
                 parseInt(input.shooting_date1.substring(0,4)) >= 2015)",
                 numericInput("shooting_minfilter",
                              label = "Minimum minutes:",
                              value = 0,
                              min = 0, max = 10000, step = 500,
                              width = "75%")),
numericInput('shooting_minshots',
             "Minimum shots:",
             value = 0,
             min = 0, max = 100, step = 10,
             width = "75%"),
numericInput('shooting_minkeypasses',
             'Minimum key passes:',
             value = 0,
             min = 0, max = 100, step = 10,
             width = "75%"),
radioButtons('shooting_seasonordate',
             'Filter by:',
             choices = c('Season', 'Date'),
             inline = T),
conditionalPanel(condition = "input.shooting_seasonordate == 'Season'",
                 dropdownButton(inputId = "shooting_seasonfilter_dropdown",
                                label = "Seasons:",
                                circle = FALSE,
                                width = "75%",
                                checkboxInput("shooting_seasonfilter_selectall",
                                              label = "All/None",
                                              value = F),
                                checkboxGroupInput("shooting_seasonfilter",
                                                   label = NULL,
                                                   choices = min(playerxgoals$Season):max(playerxgoals$Season),
                                                   selected = max(playerxgoals$Season)))),
conditionalPanel(condition = "input.shooting_seasonordate == 'Date'",
                 dateInput('shooting_date1',
                           'From:',
                           value = min(playerxgoals$date[playerxgoals$Season == max(playerxgoals$Season)]),
                           min = min(playerxgoals$date),
                           max = max(playerxgoals$date),
                           format = 'mm/dd/yyyy'),
                 dateInput('shooting_date2',
                           'To:',
                           value = max(playerxgoals$date),
                           min = min(playerxgoals$date),
                           max = max(playerxgoals$date),
                           format = 'mm/dd/yyyy')
),   
br(),

dropdownButton(inputId = "shooting_teamfilter_dropdown",
               label = "Teams:",
               circle = FALSE,
               width = "100%",
               #p(HTML("<b>Select teams:</b>")),
               checkboxInput("shooting_teamfilter_selectall",
                             label = "All/None",
                             value = T),
               checkboxGroupInput("shooting_teamfilter",
                                  label = NULL,
                                  choices = sort(unique(playerxgoals$team)),
                                  selected = sort(unique(playerxgoals$team)))),
br(),
conditionalPanel(condition = 
                   "(input.shooting_seasonordate == 'Season' && 
                 Math.min(parseInt(input.shooting_seasonfilter)) >= 2015) ||
                 (input.shooting_seasonordate == 'Date' && 
                 parseInt(input.shooting_date1.substring(0,4)) >= 2015)",
                 dropdownButton(inputId = "shooting_position_dropdown",
                                label = "Positions:",
                                circle = FALSE,
                                width = "75%",
                                checkboxInput("shooting_position_selectall",
                                              label = "All/None",
                                              value = T),
                                checkboxGroupInput("shooting_position",
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

dropdownButton(inputId = "shooting_pattern_dropdown",
               label = "Shot patterns:",
               circle = FALSE,
               width = "75%",
               checkboxInput("shooting_pattern_selectall",
                             label = "All/None",
                             value = T),
               checkboxGroupInput("shooting_pattern",
                                  label = NULL,
                                  choices = c("Open play" = "Open", "PK", "Direct FK" = "FK", "Set piece" = "Setpiece"),
                                  selected = c("Open play" = "Open", "PK", "Direct FK" = "FK", "Set piece" = "Setpiece"))),

h5(HTML('<b>Other options:</b>')),
checkboxInput('shooting_byteams',
              label = 'Split players by teams',
              value = F),
checkboxInput('shooting_byseasons',
              label = 'Split players by seasons',
              value = T)
)
###############################################################################
sidebarxGoalsPlayers <- function(input, output, session){
  
  
}

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
                                 shooting_position = c("GK", "CB", "FB/WB", "CM", "CDM", "CAM", "Wing", "F", "sub"),
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
                 choices = c("Keeper (GK)" = "GK",
                             "Central Def (CB)" = "CB",
                             "Back (FB/WB)" = "FB/WB",
                             "Midfielder (CM)" = "CM",
                             "Attacking Mid (CAM)" = "CAM",
                             "Defensive Mid (CDM)" = "CDM",
                             "Wing (W)" = "Wing",
                             "Forward (F)" = "F",
                             "Sub (S)" = "sub"),
                 selected = if (input$shooting_position_selectall) c("GK", "CB", "FB/WB", "CM", "CDM", "CAM", "Wing", "F", "sub")
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
